package bluejelly.bjc.ast

case class SourcePos(val line:Int, val column:Int) {
  override def toString = s"line: $line, column: $column"
}

case class SourceSpan(val name:String, val start:SourcePos, val end:SourcePos) {
  def displayStartEnd = s"$start - $end"
  override def toString = s"$name $displayStartEnd"
}

object SourceSpan {
  def internalModuleSourceSpan(name:String) = SourceSpan(name, SourcePos(0,0), SourcePos(0,0))
}
