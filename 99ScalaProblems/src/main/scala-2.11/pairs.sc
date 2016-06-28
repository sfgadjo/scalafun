

object pairs{
  def isPrime(n: Int): Boolean =
    (2 until n) forall (n % _ != 0)

  val n = 7
  (1 until n) flatMap(i =>
    (1 until i) map(j => (i, j))) filter(pair =>
      isPrime(pair._1 + pair._2))
}

case class Person(name: String, age: Int)

val persons = List(Person("joe", 10), Person("ed", 24))

for(p <- persons if p.age > 20) yield p.name

persons filter (p =>  p.age > 20) map (p => p.name)

val n = 7
for {
  i <- 1 until n
  j <- 1 until i
  if pairs.isPrime(i + j)
} yield (i, j)

val xs = List[Int](1,2,3)
val ys = List[Int](4,5,6)
(for((x,y) <- xs zip ys) yield x * y) sum


def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if(k == 0) Set(List[Int]())
    else
      for{
        queens <- placeQueens(k -1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens

    placeQueens(n)
}

def isSafe(col: Int, queens: List[Int]): Boolean =
{
  val row = queens.length
  val queensWithRow = (row - 1 to 0 by -1) zip queens

  queensWithRow forall {
    case (r, c) => col != c && math.abs(col - c) != row - r
  }
}

val se = List(1,2,3)

val se2 = se.flatMap(i => i until i + 10)
