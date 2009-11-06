import scala.collection.immutable.{ Range, NumericRange }

object Test {
  def rangeForeach(range : Range) = {
    val buffer = new scala.collection.mutable.ListBuffer[Int];
    range.foreach(buffer += _);
    assert(buffer.toList == range.iterator.toList, buffer.toList+"/"+range.iterator.toList)
  }
  
  case class GR[T](val x: T)(implicit val num: Integral[T]) {
    import num._
    
    def negated = GR[T](-x)
    
    def gr1 = NumericRange(x, x, x)
    def gr2 = NumericRange.inclusive(x, x, x)
    def gr3 = NumericRange(x, x * fromInt(10), x)
    def gr4 = NumericRange.inclusive(x, x * fromInt(10), x)
    
    def check = assert(
      gr1.isEmpty && !gr2.isEmpty &&
      gr3.size == 9 && gr4.size == 10 && 
      (gr3.toList ::: negated.gr3.toList).sum == num.zero && 
      !(gr3 contains (x * fromInt(10))) &&
      (gr4 contains (x * fromInt(10)))
    )
  }  
  
  def main(args: Array[String]): Unit = {
    implicit val imp1 = Numeric.BigDecimalAsIfIntegral
    implicit val imp2 = Numeric.DoubleAsIfIntegral
    
    val _grs = List[GR[_]](
      GR(BigDecimal(5.0)),
      GR(BigInt(5)),
      GR(5L),
      GR(5.0d),
      GR(2.toByte)
    )
    val grs = _grs ::: (_grs map (_.negated))
    grs foreach (_.check)
    
    assert(NumericRange(1, 10, 1) sameElements (1 until 10))
    assert(NumericRange.inclusive(1, 10, 1) sameElements (1 to 10))
    assert(NumericRange.inclusive(1, 100, 3) sameElements (1 to 100 by 3))
    
    rangeForeach(1 to 10);
    rangeForeach(1 until 10);
    rangeForeach(10 to 1 by -1);
    rangeForeach(10 until 1 by -1);
    rangeForeach(10 to 1 by -3);
    rangeForeach(10 until 1 by -3);    
  }
}
