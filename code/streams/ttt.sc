object ttt{
  val lv = Vector(Vector('c','d', 'e'), Vector('c','d', 'e'), Vector('b', 'c', 'b', 'c', 'a', 'b', 'c'))


  def findChar(c: Char, levelVector: Vector[Vector[Char]]): (Int, Int) = {
    var x = 0
    val y = levelVector.indexWhere( row => {
      val ix = row.indexOf(c)
      if (ix >= 0)
        x = ix
      ix >= 0
    })
    (x, y)
  }
  findChar('a', lv)
  lv.indexWhere(x => x.indexOf('a')>= 0)


  val a = Set(1,2,3).toStream
  val b = a #::: Stream(4,5,6)
  b.toList
}