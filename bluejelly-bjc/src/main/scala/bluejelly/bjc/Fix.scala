package bluejelly.bjc

object Fix {
  def fix[A](f:(=> A) => A):A = 
    new {val x:A = f(x)}.x
}

object FixTest {
  def fact(n:Long):Long = {
    val rec:(=> (Long => Long)) => Long => Long = f => x => if (x == 0) 1 else x*f.apply(x-1)  
    Fix.fix(rec)(n)
  }

  def main(args:Array[String]) {
    1L to 10 map fact foreach println
    println(fact(20))
  }
}
