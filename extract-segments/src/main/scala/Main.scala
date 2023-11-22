def processFile(filename: String) =
  val s = Segment.fromFile(filename)
  println(s"${filename}\t${s.points.length}\t${s.bounds.minlat}\t${s.bounds.minlon}\t${s.bounds.maxlat}\t${s.bounds.maxlon}")
 
def main(args:Array[String]): Unit = 
  if (args.length < 1) {
    println("The command takes as arguments the path of 2 gpx files.")
  } else {
    val routes = args.map(Segment.fromFile).toSeq
    val route1 = routes.head
    val route2 = routes.tail.head
    println(s"overlap between the 2 routes:${route1._1.overlap(route2._1)}")

    // ol => overlaping points/segments
    // nol => non-overlapping points/segments
    case class State(ol_seg:Set[Segment], ol_points:List[Point], nol_seg:Set[Segment], nol_points:List[Point])

    def next(s:State, p:Point):State =
      val pc = route2.findClosestDistance(p)
      val isClose = pc < 1e-7
      (isClose, s.ol_points,s.nol_points) match
        case (false, List(), _) => State(s.ol_seg,s.ol_points, s.nol_seg,p::s.nol_points)
        case (false, _, _) => State(s.ol_seg + Segment.fromPoints(s.ol_points),List(), s.nol_seg,List(p))
        case (true,  _, List()) => State(s.ol_seg,p::s.ol_points, s.nol_seg,s.nol_points)
        case (true,  _, _) => State(s.ol_seg,List(p), s.nol_seg + Segment.fromPoints(s.nol_points),List())

    for(p <- route1._2)
      val pc = route2.findClosestDistance(p)
      println(s"${p}\t${pc < 1e-7}\t${pc}")

    val r = route1._2.foldLeft(State(Set(),List(),Set(),List()))(next)
    val ol_seg = Segment.addToSet(r.ol_points, r.ol_seg)
    val nol_seg = Segment.addToSet(r.nol_points, r.nol_seg)
    println(ol_seg.size)
    println(nol_seg.size)

  }