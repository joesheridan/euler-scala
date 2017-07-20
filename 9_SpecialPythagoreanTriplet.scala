
/**
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 * a2 + b2 = c2
 * For example, 32 + 42 = 9 + 16 = 25 = 52.
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 */

case class Triplet(a: Int, b: Int, c: Int)

// determine whether the specified tuple is a pythagorean triplet
def isPTriplet(t: Triplet) : Boolean = {
  t.a*t.a + t.b*t.b == t.c*t.c
}

val ptriplets = for {
    i <- (1 to 1000)
    j <- (1 to 1000)
    k <- (1 to 1000)
    if ((i < j && j < k) && isPTriplet(Triplet(i,j,k)) && i + j + k == 1000)
  } yield Triplet(i,j,k)

  
