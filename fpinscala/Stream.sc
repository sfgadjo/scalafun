case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] ={
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, ()=> tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOptionWithFoldR: Option[A] = this.foldRight(None:Option[A])((a, _) => Some(a))

  //not tail recursive
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def toListTail: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case _ => acc
    }

    loop(this, List()).reverse //to avoid using .reverse a mutable buffer would be needed
  }

//  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
//    case Cons(h, t) => Some((f(h()), t()))
//  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h, t), 1) => Some((h(), (Empty, 0)))
    case (Cons(h ,t), n) if n > 1 => Some((h(), (t(), n -1)))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  } append Stream(Stream.empty)


  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream[B](z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  //{
//    def go(acc: => Stream[Stream[A]], next: => Stream[A]): Stream[Stream[A]] =
//      next match{
//        case Cons(h, t) => go(Stream.cons(Stream.cons(h(), next), acc), t())
//        case _ => acc
//      }
//
//    go(Stream.cons(Stream[A](), Empty), this)
//  }


  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s) takeWhileViaUnfold(!_._2.isEmpty) forAll {
        case (h, h2) => h == h2
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Empty))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Empty -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  def zipWithViaUnfold[B,C](xs: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, xs)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def drop(n: Int): Stream[A] = (n, this) match {
    case (0, _) => this
    case (nn, Cons(h, t)) => t().drop(nn - 1)
    case _ => sys.error("Kablooey!")
  }

  def take(n: Int): Stream[A] = this match{
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Empty)
    case _ => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match{
    case Cons(h, t) if p(h()) => Cons(h, () => t() takeWhile p)
    case _ => Empty
  }

  def takeWhileWithFoldR(p: A => Boolean): Stream[A] = {
    this.foldRight(Empty:Stream[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Empty)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: B)(f: (A,B) => B): B = this match{
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a,b) => p(a) && b)

  def map[B](f: A => B): Stream[B] = this.foldRight(Stream[B]())((a,b) => Stream.cons(f(a), b))
  def filter(p: A => Boolean): Stream[A] = this.foldRight(Stream[A]())((a, b) => if(p(a)) Stream.cons(a, b) else b)
  def append[B >: A](s: => Stream[B]): Stream[B] = this.foldRight(s)((a, b) => Stream.cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(Stream[B]())((a,b) => f(a) append b)
}
val x = Stream(1,2,3,4).toListTail
val y = Stream(1,2,3,4).toList

val z = Stream(1,2,3,4).drop(2).toList

val aa = Stream((1 to 1000):_*).take(999999).toList

val tw = Stream(1,2,3,4,5,6).takeWhile(a => a != 5).toList
val tw2 = Stream(1,2,3,4,5,6).takeWhileWithFoldR(a => a != 5).toList
val it = tw == tw2
val fa = Stream(2,4,6,8,9).forAll(a => a % 2 == 0)

val ho = Stream().headOptionWithFoldR
val ones: Stream[Int] = Stream.cons(1, ones)

ones.take(5).toList
def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
val const = constant(1).take(10).toList


def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))


val fibs = {
  def go(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, go(b, a + b))
    go(0, 1)
}

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z).map(a => Stream.cons(a._1, unfold(a._2)(f))).getOrElse(Stream[A]())


val uf = unfold(1)(s => if(s < 10) Some((s + 1, s * 2)) else None).toList
def unfold2[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case Some((h,s)) => Stream.cons(h, unfold(s)(f))
    case None => Empty
  }

val uf2 = unfold(1)(s => if(s < 10) Some((s + 1, s * 2)) else None).toList
val fibsViaUnfold =
  unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }

val takit = fibsViaUnfold.take(10)

takit.toList

def from2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))
val fromm = from2(10).take(100).toList
def constant2[A](v: A): Stream[A] = unfold(v)(s => Some((s, s)))

val cc = constant2("xyz").take(10).toList

def ones2(): Stream[Int] = constant2(1)

val oness = ones2.take(10).toList

val sw = Stream(1,2,3,4,5).startsWith(Stream(1,2,3))

val tailsies = Stream(1,2,3).tails.toList

val yy = tailsies.map(a => a.toList)

val sr = Stream(1,2,3).scanRight(0)(_ + _).toList