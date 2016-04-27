package bluejelly.bjc

object Fix {
  def fix[A](f:(=> A) => A):A = 
    new {val x:A = f(x)}.x
}
