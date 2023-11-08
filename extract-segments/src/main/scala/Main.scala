def processFile(filename: String) =
  val s = Segment.fromFile(filename)
  println(s"${filename}\t${s.points.length}\t${s.bounds.minlat}\t${s.bounds.minlon}\t${s.bounds.maxlat}\t${s.bounds.maxlon}")
 
def main(args:Array[String]): Unit = 
  if (args.length < 1) {
    println("The command takes as arguments the path of gpx files.")
  } else {
    val routes = args.map(Segment.fromFile).toSeq
    val overlap = for {
      r1 <- routes
      r2 <- routes
    } yield (r1._1,r2._1, r1._1.overlap(r2._1))
    println(overlap.filter(_._3<0))
  }
