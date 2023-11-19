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
    for(p <- route1._2)
      val pc = route2.findClosest(p)
      println(s"${p}\t${pc}")

  }