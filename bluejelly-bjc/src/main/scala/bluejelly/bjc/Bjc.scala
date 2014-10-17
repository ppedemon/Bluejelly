/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of 
 * the BSD license, see the LICENSE file for details.
 */
package bluejelly.bjc

/*
import bluejelly.bjc.parser.{Scanner,LayoutScanner,BluejellyParser}
import bluejelly.utils.UnicodeFilter
import bluejelly.bjc.common.Name
import bluejelly.bjc.core._

import java.io.StringReader
import java.io.InputStreamReader
import java.io.FileReader

import scala.util.parsing.input.Reader
*/

import bluejelly.bjc.ast.module
import bluejelly.bjc.core._
import bluejelly.bjc.parser.BluejellyParser
import bluejelly.bjc.static._
import bluejelly.utils.UnicodeFilter

import java.io.{Reader,StringReader,PrintWriter}

/**
 * Enum for the compiler stages.
 * @author ppedemon
 */
object BjcStage extends Enumeration {
  type BjcStage = Value
  var Parser, Reader, All = Value
}

/**
 * This class exposes the Bluejelly compiler API.
 * @author ppedemon
 */
class Bjc(ifaceLoader:IfaceLoader = ProdLoader) {

  val loader = new ModuleLoader(ifaceLoader)

  def parse(r:Reader):Either[String,module.Module] = {
    val in = new UnicodeFilter(r)
    BluejellyParser.phrase(BluejellyParser.program, in) match {
      case err@BluejellyParser.NoSuccess(msg,_) => Left(msg)
      case BluejellyParser.Success(m,_) => Right(m)
    }
  }

  def chaseImports(mod:module.Module, errors:BjcErrors) = {
    val modLoader = new ModuleLoader(ifaceLoader)
    val importChaser = new ImportChaser(modLoader, errors)
    importChaser.chaseImports(mod)    
  }

  private def pipeline(mod:module.Module, errors:BjcErrors) {
    val exps = chaseImports(mod, errors)
    if (!errors.hasErrors) println(exps)
  }

  def compile(r:Reader, errors:BjcErrors) {
    val result = parse(r)
    result match {
      case Left(err) => errors.parseError(err)
      case Right(mod) => 
        pipeline(mod,errors)
        if (errors.hasErrors) errors.dumpTo(new PrintWriter(System.out))  
    }
  }
}

/**
 * Entry point for the Bluejelly compiler.
 * @author ppedemon
 */
object Bjc {
  /*
  import parser.Lexer._
  
  private def scan(s:Reader[Token]):List[Token] = s.first match {
    case EOI() => Nil
    case t@ErrorToken(msg) => t::scan(s.rest)
    case t => t::scan(s.rest)
  }
  */

  def main(args:Array[String]) {
    val module = 
      """
      import Z
      """
    val errs = new BjcErrors("Main.hs")
    val bjc = new Bjc
    bjc.compile(new StringReader(module), errs)

    //val iface = ModIfaceIO.load(args(0))
    //println(iface)
    //val modName = Name(Symbol(args(0)))
    //val env = BjcEnv.withBuiltIns
    //val modDefn = new ModuleLoader().load(env, modName)
    //println(modDefn)
    
    /*
    val start = System.currentTimeMillis()    
    val in = new UnicodeFilter(
        new StringReader(
        """
            data X a = X (Set [a])
            class Eq a where 
              (==),(/=) :: a -> a -> Bool
              x /= y = not (x == y)
        """))
    val result = BluejellyParser.phrase(BluejellyParser.program, in)
    printf("Parsing time: %d\n",l System.currentTimeMillis()-start)
    
    result match {
      case err@BluejellyParser.NoSuccess(_,_) => println(err)
      case BluejellyParser.Success(m,_) => println(m)
    }
    */
    /*
    val start = System.currentTimeMillis
    val in = new UnicodeFilter(new FileReader("tmp/prog.in"))
    val scanner = new LayoutScanner(in)
    val toks = scan(scanner)
    for (t <- toks) println("%s:%s:\n%s" format (t.pos,t,t.pos.longString))
    val end = System.currentTimeMillis()
    printf("Lexing time: %d\n", end-start)
    */
    /*
    val scanner = new LayoutScanner("""data D a = data.Set.Set a case1_'.M.a_'20'''""")
    val toks = scan(scanner)
    for (t <- toks) println("%s:%s:\n%s" format (t.pos,t,t.pos.longString))
    */
  }
}
