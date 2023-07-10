package example

object Hello extends Greeting with App {

  private final val BANANE = 1
  private final val CANETTE = 3
  private final val PLASTIQUE = 4
  private final val VERRE = 5
  private final val BONBON = 2

  val names = Map(
    1 -> "BANANE",
    3 -> "CANETTE",
    4 -> "PLASTIQUE",
    5 -> "VERRE",
    2 -> "BONBON",
  )

  val mountain: Vector[Vector[Int]] = Vector(
    Vector(BANANE, VERRE,BONBON,PLASTIQUE,CANETTE),
    Vector(PLASTIQUE,CANETTE,VERRE,PLASTIQUE,VERRE),
    Vector(BONBON, VERRE, BANANE, CANETTE, BONBON),
    Vector(PLASTIQUE, VERRE, BANANE, BANANE, PLASTIQUE),
    Vector(BONBON, CANETTE, BANANE, BONBON, CANETTE),
  )

  private val sum: Int = mountain.map(_.sum).sum
  assert(sum == 75)

  println("let's rock baby !")

  val hikerPos = 2

  private val value: Vector[Int] = getLineInPos(hikerPos, mountain)
  value.reverse.foreach(v => println(names(v)))
}

trait Greeting {
  lazy val greeting: String = "hello"

  final def getLineInPos(hikerPos: Int, mountain: Vector[Vector[Int]]): Vector[Int] = {

    val (isRow, index) = getIsRowAndIndex(hikerPos)
    val slice = if (isRow) {
      mountain(index)
    } else {
      mountain.map(a => a(index))
    }

    slice.distinct.filterNot(_ == 0).foreach { d =>
      val count = slice.count(_ == d)
      val nextHikerPos = hikerPos + count

      val nextMountain = if (isRow) {
        val nextSlice = slice.map { c =>
          if (c == d) {
            0
          } else {
            c
          }
        }
        mountain.updated(index, nextSlice)
      } else {
        mountain.map { m =>
          val c = m(index)

          val update = if (c == d) {
            0
          } else {
            c
          }
          m.updated(index, update)
        }
      }

      nextMountain.foreach { m =>
        println(m.mkString(" "))
      }
      println()

      if (nextMountain.map(_.sum).sum == 0) {
        return Vector(d)
      }

      val value = getLineInPos(nextHikerPos, nextMountain)
      if (value.nonEmpty) {
        return value :+ d
      }
    }

    println("backtrack")
    Vector.empty
  }


  /*
  Grid is
    0  1  2  3  4
19                 5
18                 6
17                 7
16                 8
15                 9
   14 13 12 11 10
   */
  @inline def getIsRowAndIndex(hikerPosition: Int): (Boolean, Int) = {
    (hikerPosition % 20) match {
      case i if i < 5 => (false, i)
      case i if i < 10 => (true, i - 5)
      case i if i < 15 => (false, 14 - i)
      case i if i < 20 => (true, 19 - i)
    }
  }
}
