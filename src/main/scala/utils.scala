import scala.util.Random

object utils {

  def pickOne[T](from: List[T]): T =
    from(Random.nextInt(from.size))

  def pickUpTo[T](n: Int, from: List[T]): List[T] = {
    val m = math.min(from.size, n)
    val (_, res) = 0.to(Random.nextInt(m + 1)).drop(1).foldLeft((from, List.empty[T])) {
      case ((f, r), _) =>
        val p = pickOne(f)
        (f.filterNot(_ == p), p :: r)
    }
    res
  }

}
