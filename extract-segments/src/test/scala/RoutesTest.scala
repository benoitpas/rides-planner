import org.junit.Test
import org.junit.Assert.*

class RoutesTest:
  val b1 = Bounds(51.4, -0.3, 51.52, 0.0)
  val b2 = Bounds(51.5, -0.3, 51.53, -0.1)
  val b3 = Bounds(51.47, -0.7, 51.56, -0.3)
  val b4 = Bounds(51.49, -0.2, 51.51, -0.1)

  @Test def b1b1(): Unit =
    assertEquals(1, b1.overlap(b1), 0)

  @Test def b1b2(): Unit =
    assertEquals(0.1, b1.overlap(b2), 2)

  @Test def b2b1(): Unit =
    assertEquals(0.1, b2.overlap(b1), 2)

  @Test def b3b4(): Unit =
    assertEquals(0, b3.overlap(b4), 0)

  val a = Point(2, 1)
  val b = Point(5, 4)
  val c = Point(6, 2)
  val d = Point(0, 4)

  val i = Point(3.75, 2.75)

  @Test def intersectIn() =
    val p = Segment.intersect((a, b), (c, d)).getOrElse(Point(0, 0))
    assertEquals(i, p)

  @Test def intersectOut() =
    assertEquals(None, Segment.intersect((a, c), (b, d)))

  val e = Point(0, 0)

  @Test def noIntersection =
    val noIntersectSeg = List(a, d, b, c, e)
    val r = Segment.nonOverlapping(noIntersectSeg)
    assertEquals(List(noIntersectSeg.reverse), r)

  @Test def intersectingSegment1 =
    val intersectSeg = List(a, b, c, d, e)
    val r = Segment.nonOverlapping(intersectSeg)
    assertEquals(List(List(e, d, i), List(i, c, b, i), List(i, a)), r)

  @Test def intersectingSegment2 =
    val intersectSeg = List(e, a, b, c, d)
    val r = Segment.nonOverlapping(intersectSeg)
    assertEquals(List(List(d, i), List(i, c, b, i), List(i, a, e)), r)

  @Test def inSegment =
    val s = List(e, d, b, c) // a
    assertTrue(Segment.in(a, s, 10))

  @Test def outSeqment =
    val s = List(e, a, d) // c
    assertFalse(Segment.in(c, s, 10))

  @Test def boundsFromPoints =
    val points = List(a, b, c, d, i)
    assertEquals(Bounds(0, 1, 6, 4), Bounds.fromPoints(points))

  val resourcesPath = "extract-segments/src/test/resources/"
  val routeGCH =
    Segment.fromFile(resourcesPath + "Gerrards_Cross_Harrow.gpx")
  val routeHGC =
    Segment.fromFile(resourcesPath + "Harrow_Gerrards_Cross.gpx")
  val routeGCG =
    Segment.fromFile(resourcesPath + "Greenford_Chalfont_St_Giles.gpx")
  val routeGCG2 =
    Segment.fromFile(resourcesPath + "Greenford_Chalfont_St_Giles2.gpx")
  val routeEF =
    Segment.fromFile(resourcesPath + "Ealing_Fulham.gpx")

  @Test def extractSegments1 =
    assertEquals(661, routeGCH._2.size)
    val (ol_seg, nol_seg) = routeGCH.extractSegments(routeGCG)
    assertEquals(2, ol_seg.size)
    assertEquals(3, nol_seg.size)
    assertEquals(Set(60, 89), ol_seg.map(_._2.size))
    assertEquals(Set(154, 1, 357), nol_seg.map(_._2.size))

  @Test def extractSegments2 =
    assertEquals(593, routeGCG._2.size)
    assertEquals(1853, routeGCG2._2.size)
    val (ol_seg, nol_seg) = routeGCG.extractSegments(routeGCG2)
    assertEquals(3, ol_seg.size)
    assertEquals(4, nol_seg.size)
    assertEquals(Set(3, 33, 5), ol_seg.map(_._2.size))
    assertEquals(Set(48, 224, 1, 279), nol_seg.map(_._2.size))

  @Test def extractSegmentsReverse =
    assertEquals(661, routeGCH._2.size)
    assertEquals(660, routeHGC._2.size)
    val (ol_seg, nol_seg) = routeGCH.extractSegments(routeHGC)
    assertEquals(3, ol_seg.size)
    assertEquals(2, nol_seg.size)
    assertEquals(Set(214, 422, 5), ol_seg.map(_._2.size))
    assertEquals(Set(1, 19), nol_seg.map(_._2.size))

  @Test def extractSegmentsNoOverlap =
    assertEquals(661, routeGCH._2.size)
    assertEquals(413, routeEF._2.size)
    val (ol_seg, nol_seg) = routeGCH.extractSegments(routeEF)
    assertEquals(0, ol_seg.size)
    assertEquals(1, nol_seg.size)
    assertEquals(Set(), ol_seg.map(_._2.size))
    assertEquals(Set(661), nol_seg.map(_._2.size))

  @Test def haversineDistance1 =
    val p1 = Point(51.58109, -0.33811)
    val p2 = Point(51.58105, -0.33792)

    val e = 13.861454334483879
    assertEquals(e, p1.haversineDistance(p2), 0.0000000001)
    assertEquals(e, p1.haversineDistance(p2), 0.0000000001)

  @Test def haversineDistance2 =
    val p1 = Point(51.58109, -0.33811)
    val p2 = Point(51.58781, -0.55373)

    val e = 14916.367428883348
    assertEquals(e, p1.haversineDistance(p2), 0.0000000001)
    assertEquals(e, p1.haversineDistance(p2), 0.0000000001)

  @Test def haversineDistanceLat1Degree =
    val p1 = Point(51, -1)
    val p2 = Point(52, -1)

    // 1 degree of latitude in southern england (meters)
    val e = 111194.92664455874
    assertEquals(e, p1.haversineDistance(p2), 0.0000000001)
    assertEquals(e, p1.haversineDistance(p2), 0.0000000001)

  @Test def haversineDistanceLon1Degree =
    val p1 = Point(51, 0)
    val p2 = Point(51, -1)

    // 1 degree of latitude in southern england (meters)
    val e = 69976.69829285977
    assertEquals(e, p1.haversineDistance(p2), 0.0000000001)
    assertEquals(e, p1.haversineDistance(p2), 0.0000000001)

  val lat1m = 1 / Point.lat1DegSEEngland
  val lon1m = 1 / Point.lon1DegSEEngland

  @Test def distanceToSegment1 =
    val a = Point(51 + lat1m, -1 + lon1m)
    val b = Point(51 - lat1m, -1 - lon1m)
    val c = Point(51 + lat1m, -1 - lon1m)
    assertEquals(math.sqrt(2), c.distance(a, b), 0.4)
    assertEquals(2, c.distance(a), 0.01)
    assertEquals(math.sqrt(2), c.distance(b), 0.4)

  @Test def distanceToSegment2 =
    val a = Point(51 - 2 * lat1m, 3 * lon1m)
    val b = Point(51 + 5 * lat1m, 11 * lon1m)
    val c = Point(51 + 3 * lat1m, 5 * lon1m)
    assertEquals(5.46892, c.distance(a, b), 0.001)
    assertEquals(5.46892, c.distance(a), 0.001)
    assertEquals(6.36381, c.distance(b), 0.001)

  @Test def compareDistance1: Unit =
    assertEquals(27.894609721234673, math.sqrt(1e-7 * Point.lat1DegSEEngland * Point.lon1DegSEEngland), 0.001)

    val diffSquared = routeGCG._2.map(p =>
      val d = routeGCH.findClosestDistance(p)
      val dh = routeGCH.findClosestDistanceHaversine(p)
      (d - dh) * (d - dh)
    )
    val rms = math.sqrt(diffSquared.sum / diffSquared.size)

    assertEquals(4.04515, rms, 0.0001)
