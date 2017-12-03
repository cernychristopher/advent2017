object Advent3 {
  case class Pos(x:Int, y:Int) {
    def neighbors:Seq[Pos] = (for {
      x <- (-1).to(1)
      y <- (-1).to(1)
    } yield if (x==0 && y==0) None else Some(Pos(x+this.x,y+this.y))).flatten
  }

  val TestInput = 361527
  val InitialPos = Pos(0,0)
  val InitialValue = 1
  val InitialMap = Map(InitialPos -> 1)

  def positions(index:Int): List[Pos] = {
    (
      index.to(-index, -1).map(Pos(_, -index)).toList :::
      (-index+1).to(index).map(Pos(-index, _)).toList :::
      (-index+1).to(index).map(Pos(_,index)).toList :::
      (index-1).to(-index+1,-1).map(Pos(index,_)).toList
    ).reverse
  }

  def main(args:Array[String]):Unit = {
    val posStream = Stream.from(0).flatMap(positions)
    
    val sumStream = posStream.drop(1).scanLeft(
      (InitialMap, InitialValue)
    ) { case ((map, _), elementPos) =>
      val nextMax = elementPos.neighbors.map(map.getOrElse(_,0)).sum

      (map + (elementPos -> nextMax), nextMax)
    }
    
    println((null #:: posStream).zipWithIndex.drop(TestInput).headOption.map(p => p._1.x + p._1.y).get)
    println(sumStream.dropWhile(_._2 < TestInput).head._2)
  }
}
