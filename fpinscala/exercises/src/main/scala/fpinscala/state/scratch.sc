import fpinscala.state._

def func(rng: RNG): (Int, RNG) = rng.nextInt
def func2(rng: RNG): (Int, RNG) = {
  val x = rng.nextInt
  (x._1 + 2, x._2)
}
val x = RNG.sequence(List(func _, func2 _))
val y = x(RNG.Simple.apply(10))