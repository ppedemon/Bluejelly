package bluejelly.bjc

object Panic {
  def apply(msg:String) = sys.error(s"An internal error ocurred during compilation: $msg")
}
