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
    println(b3.overlap(b4))
    assertEquals(0, b3.overlap(b4), 0)
