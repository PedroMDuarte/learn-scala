object bfs {

  case class Point(x: Int, y: Int) {
    def neighbors: List[Point] = {
      List(
        Point(x+1, y),
        Point(x, y+1),
        Point(x-1, y),
        Point(x, y-1)
      )
    }

    def validNeighbors =
      neighbors.filter(p => {
        p.x >= 0 && p.y >= 0 && p.x < 3 && p.y < 3
      })
  }

  val p1 = Point(0, 0)
  p1.neighbors
  p1.validNeighbors

  def bfs: Stream[Point] = from(Stream(p1), Set())

  def from(initial: Stream[Point], explored: Set[Point]): Stream[Point] = {
    if (initial.isEmpty) {
      Stream.empty
    }
    else {
      val toVisit = initial.head.validNeighbors.filterNot(p => explored contains p)
      val visitStream = from(initial.tail append toVisit, explored ++ initial ++ toVisit)
      initial.head #:: visitStream
    }
  }

  val n = 15
  bfs.take(n).foreach(println)
  bfs.map((p: Point) => p.toString).take(n) mkString "\n"
}