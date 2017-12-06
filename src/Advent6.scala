object Advent6 {

  val TestInput: List[Int] = List(2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14)

  def rotateRight[A](ls: List[A], count:Int): List[A] =
    ls.drop(ls.size - count % ls.size) ::: ls.take(ls.size - count % ls.size)

  def redistribute(list:List[Int]): List[Int] = {
    val (max, maxIndex) = list.zipWithIndex.maxBy(_._1)
    val baseDistribution = max / list.length
    val rest = rotateRight(
      list.indices.map(index => if (index < max % list.length) 1 else 0).toList,
      maxIndex + 1
    )

    List(
      List.fill(maxIndex)(0) ++ List(-list.apply(maxIndex)) ++ List.fill(list.length - maxIndex - 1)(0),
      List.fill(list.size)(baseDistribution),
      rest,
      list
    ).transpose.map(_.sum)
  }
  
  def main(args:Array[String]):Unit = {
    val redistributions = Stream.iterate(TestInput)(redistribute)
    val cycles = redistributions.scanLeft(List.empty[List[Int]])((iss, is) => is :: iss).drop(1)

    val solution1 = cycles.takeWhile {
      case Nil => false
      case ls :: lss => !lss.contains(ls)
    }.length

    val solution2 = cycles.collect { case ls :: lss => lss.indexOf(ls) + 1 }.dropWhile(_ == 0).head

    println(s"Solution1: $solution1")
    println(s"Solution2: $solution2")
  }
}