import org.junit.Test
import org.junit.Assert.*

class RoutesTest:
  val b1 = Bounds(51.4, -0.3, 51.52, 0.0)
  val b2 = Bounds(51.5, -0.3, 51.53, -0.1)
  val b3 = Bounds(51.47,-0.7,51.56,-0.3)
  val b4 = Bounds(51.49,-0.2,51.51,-0.1)

  @Test def b1b1(): Unit = 
    assertEquals(1, b1.overlap(b1),0)

  @Test def b1b2(): Unit = 
    assertEquals(0.1, b1.overlap(b2), 2)

  @Test def b2b1(): Unit = 
    assertEquals(0.1, b2.overlap(b1), 2)

  @Test def b3b4(): Unit = 
    assertEquals(0, b3.overlap(b4), 0)

  val a = Point(2,1)
  val b = Point(5,4)
  val c = Point(6,2)
  val d = Point(0,4)

  val i = Point(3.75, 2.75)

  @Test def intersectIn() =
    val p = Segment.intersect((a,b), (c,d)).getOrElse(Point(0,0))
    assertEquals(i, p)

  @Test def intersectOut() =
    assertEquals(None, Segment.intersect((a,c), (b,d)))

  val e = Point(0,0)

  @Test def noIntersection =
    val noIntersectSeg = List(a,d,b,c,e)
    val r = Segment.nonOverlapping(noIntersectSeg)
    assertEquals(List(noIntersectSeg.reverse), r)

  @Test def intersectingSegment1 =
    val intersectSeg = List(a,b,c,d,e)
    val r = Segment.nonOverlapping(intersectSeg)
    assertEquals(List(List(e,d,i), List(i,c,b,i), List(i, a)), r)

  @Test def intersectingSegment2 =
    val intersectSeg = List(e,a,b,c,d)
    val r = Segment.nonOverlapping(intersectSeg)
    assertEquals(List(List(d,i), List(i,c,b,i), List(i, a, e)), r)

  @Test def inSegment =
    val s = List(e,d,b,c) //a
    assertTrue(Segment.in(a, s, 10))

  @Test def outSeqment =
    val s = List(e,a,d) //c
    assertFalse(Segment.in(c, s, 10))

  @Test def boundsFromPoints =
    val points = List(a,b,c,d,i)
    assertEquals(Bounds(0,1,6,4), Bounds.fromPoints(points))
