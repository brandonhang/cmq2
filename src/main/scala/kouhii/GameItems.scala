package kouhii

import java.util.Random

/**
  * Utilites used during game generation
  */
object GameItems {
  var randy: Random = _

  /**
    * Picks a pseudorandom number from a given range
    *
    * @param size    The range of the random integer
    * @return        A random integer between the given range
    */
  def nextInt(size: Int): Int = {
    randy = new Random(System.currentTimeMillis)
    randy.nextInt(size)
  }
}
