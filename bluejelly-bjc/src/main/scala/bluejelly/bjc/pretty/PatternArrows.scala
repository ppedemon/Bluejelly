package bluejelly.bjc.pretty

import scalaz._
import scalaz.Kleisli._

import syntax.arrow._
import syntax.foldable1._
import syntax.std.option._

import std.option._
import std.function._

import bluejelly.bjc.monad.ArrowPlus
import bluejelly.bjc.monad.KleisliArrow._

object PatternArrows {
  type Pattern[A,B] = Kleisli[Option,A,B]

  trait Operator[A,R,S]
  case class AssocL[A,R,S](p:Pattern[A,(A,A)], f:(R,R) => R) extends Operator[A,R,S]
  case class AssocR[A,R,S](p:Pattern[A,(A,A)], f:(R,R) => R) extends Operator[A,R,S]
  case class Wrap[A,R,S](p:Pattern[A,(S,A)], f:(S,R) => R) extends Operator[A,R,S]

  def buildPrettyPrinter[A,R,S](t:List[NonEmptyList[Operator[A,R,S]]], p:Pattern[A,R]):Pattern[A,R] = 
    t.foldLeft(p) { (p1,ops) => 
      val pats = ops map {
        case AssocL(pat,g) => chainl[A,R](pat, g, p1)
        case AssocR(pat,g) => chainr[A,R](pat, g, p1)
        case Wrap(pat,g) => wrap[A,R,S](pat, g, p1)
      }
      val pat = pats.foldLeft1(implicitly[ArrowPlus[Pattern]].lazyplus(_,_))
      implicitly[ArrowPlus[Pattern]].lazyplus(pat, p1)
    }

  def parens[A](p:Pattern[A,String]):Pattern[A,String] = 
    p.map(s => s"($s)")

  def chainl[A,R](split:Pattern[A,(A,A)], f:(R,R) => R, p:Pattern[A,R])(implicit AP:ArrowPlus[Pattern]):Pattern[A,R] = {
    val rec:(=> Pattern[A,R]) => Pattern[A,R] = c =>
      (split:Kleisli[Option,A,(A,A)]) >>> 
        ((AP.lazyplus(c,p):Kleisli[Option,A,R]) *** p) >>> Arrow[Pattern].arr(f.tupled)
    fix(rec)
  }

  def chainr[A,R](split:Pattern[A,(A,A)], f:(R,R) => R, p:Pattern[A,R])(implicit AP:ArrowPlus[Pattern]):Pattern[A,R] = {
    val rec:(=> Pattern[A,R]) => Pattern[A,R] = c => 
      (split:Kleisli[Option,A,(A,A)]) >>> 
        ((p:Kleisli[Option,A,R]) *** AP.lazyplus(c,p)) >>> Arrow[Pattern].arr(f.tupled)
    fix(rec)
  }

  def wrap[A,R,S](split:Pattern[A,(S,A)], f:(S,R) => R, p:Pattern[A,R])(implicit AP:ArrowPlus[Pattern]):Pattern[A,R] = {
    val rec:(=> Pattern[A,R]) => Pattern[A,R] = c => 
      (split:Kleisli[Option,A,(S,A)]) >>> 
        ((Arrow[Pattern].id[S]:Kleisli[Option,S,S]) *** AP.lazyplus(c,p)) >>> Arrow[Pattern].arr(f.tupled)
    fix(rec)
  }
}

/* TODO: Remove tests */
object TestPatternArrows {
  import PatternArrows._

  trait Expr
  case class Var(v:String) extends Expr
  case class Abs(v:String, e:Expr) extends Expr
  case class App(e0:Expr, e1:Expr) extends Expr

  def pvar:Pattern[Expr,String] = kleisli({
    case Var(v) => some(v)
    case _ => None
  })

  def pabs:Pattern[Expr,(String,Expr)] = kleisli {
    case Abs(v,e) => Some((v,e))
    case _ => None
  }

  def papp:Pattern[Expr,(Expr,Expr)] = kleisli {
    case App(e0,e1) => Some((e0,e1))
    case _ => None
  }

  def expr:Pattern[Expr,String] = {
    val ops:List[NonEmptyList[Operator[Expr,String,String]]] = List(
      NonEmptyList(AssocL[Expr,String,String](papp, (e0,e1) => s"$e0 $e1")), 
      NonEmptyList(Wrap[Expr,String,String](pabs, (s,r) => s"\\$s -> $r"))
    )
    def rec: (=> Pattern[Expr,String]) => Pattern[Expr,String] = 
      p => buildPrettyPrinter(ops, implicitly[ArrowPlus[Pattern]].lazyplus(pvar, parens(p)))
    fix(rec)
  }

  def prettyPrint(e:Expr):String = expr(e)|"Incomplete pattern match"

  trait Eqn
  case class Const(x:Int) extends Eqn
  case class Bin(e0:Eqn, op:Char, e1:Eqn) extends Eqn

  def con:Pattern[Eqn,Int] = kleisli {
    case Const(x) => some(x)
    case _ => None
  }

  def bin(c:Char):Pattern[Eqn,(Eqn,Eqn)] = kleisli {
    case Bin(e0,op,e1) if op == c => some((e0,e1))
    case _ => None
  }

  def eqn:Pattern[Eqn,String] = {
    def binOp(c:Char) = AssocL[Eqn,String,String](bin(c), (e0,e1) => s"$e0 $c $e1")
    def ops:List[NonEmptyList[Operator[Eqn,String,String]]] = List(
      NonEmptyList(binOp('*'), binOp('/')),
      NonEmptyList(binOp('+'), binOp('-'))
    )
    def rec: (=> Pattern[Eqn,String]) => Pattern[Eqn,String] = 
      p => buildPrettyPrinter(ops, implicitly[ArrowPlus[Pattern]].lazyplus(con.map(_.toString), parens(p)))
    fix(rec)
  }

  def pprEqn(e:Eqn):String = eqn(e)|"Incomplete pattern match"

  def main(args:Array[String]) {
    println(prettyPrint(Abs("x", Abs("y", Abs("z", App(App(Var("x"), Var("z")), App(Var("y"), Var("z"))))))))
    println(prettyPrint(App(Abs("x",Var("x")), Abs("x",Var("x")))))

    val e = Bin(Bin(Const(1),'+',Const(2)),'-', Bin(Const(1),'-',Const(2)))
    println(pprEqn(e))
  }
}
