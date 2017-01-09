//http://aperiodic.net/phil/scala/s-99/

@annotation.tailrec
def last[A](l: List[A]): Option[A] = l match{
  case Nil => None
  case h :: Nil => Some(h)
  case _ :: t => last(t)
}

val lastv = last(List(1,2,3,4))//= 4

@annotation.tailrec
def penultimate[A](l: List[A]): Option[A] = l match{
  case Nil => None
  case h :: _ :: Nil => Some(h)
  case _ :: t => penultimate(t)
}

val penult = penultimate(List(1,2,5,6,8))//= 6


def nth[A](a: Int, l: List[A]): Option[A] = {
  @annotation.tailrec
  def iter(acc: Int, xs: List[A]): Option[A] = xs match {
    case Nil => None
    case h :: _ if (acc >= a) => Some(h)
    case _ :: t => iter(acc + 1, t)
  }

  iter(0, l)
}

val nthVal = nth(5, List(1,2,3,4,5,6,7,8))//6

def nth2[A](n: Int, l: List[A]): Option[A] = (n, l) match{
  case (0, h :: _) => Some(h)
  case (nn, _ :: t) => nth2(nn - 1, t)
  case _ => None
}

val nthVal2 = nth(5, List(1,2,3,4,5,6,7,8))//6

//TODO: return Option[A]
def length[A](l: List[A]): Int =
  l.foldLeft(0)((b,a) => b + 1)


val len = length(List(1,2,5))

def isPalindrome(s: String): Boolean = {
  val a = System.nanoTime()
  val r = s == s.reverse
  val e = System.nanoTime()
  println("Time: " + (e - a).toString)
  r
}

def isPalindromeOptmzd(s: String): Boolean = {
  val x =System.nanoTime()

  @annotation.tailrec
  def loop(a: Int, b: Int): Boolean =
    if (a > b) false
    else if (s(a) == s(b)) true
    else loop(a + 1, b - 1)


  val r = loop(0, s.length - 1)
  val y = System.nanoTime()
  println("Time: " + (y - x).toString)
  r
}

val p = isPalindrome("abcdefghijklmnopponmlkjihgfedcba")
val m = isPalindromeOptmzd("abcdefghijklmnopponmlkjihgfedcba")//a little faster


def flatten(l: List[Any]): List[Any] = l flatMap {
  case h :: t => flatten(h :: t)
  case h => List(h)
}

val f = flatten(List(List(1, 1), "hi", List(3, List(5, 8))))

def compress[A](l: List[A]): List[A] = l.foldRight(List[A]())((a,b) => (a,b) match {
  case (h, h2 :: t) if h == h2 => h2 :: t
  case _ => a :: b
})

def compress2[A](l: List[A]): List[A] = l.foldRight(List[A]()) {(a,b) =>
  if(b.isEmpty || b.head != a) a :: b
  else b}

val c= compress(List("a", "a", "b", "c", "a", "a", "a", "d"))

def pack[A](l: List[A]): List[List[A]] =
  if(l.isEmpty) List[List[A]]()
  else {
    val (packed, next) = l span {_ == l.head }
    if(next == Nil) List(packed)
    else packed :: pack(next)
  }

val pp = pack(List("a", "a", "b", "c", "a", "a", "a", "d"))

val seq = Seq((1 to 10000).toSeq).foldRight(Seq[Int]())((a,b) => b)

def encode[A](l: List[A]): List[(Int,A)] = pack(l) map {a => (a.length, a.head)}

var enc = encode(List("a", "a", "b", "c", "a", "a", "a", "d"))

def encodeModified(l: List[Any]): List[Any] = pack(l) map {
  case h :: Nil => h
  case a => (a.length, a.head)
}

var encM = encodeModified(List("a", "a", 1, "c", "a", "a", "a", "d"))

//faster
def decode[A](l: List[(Int,A)]): List[A] = {
  val x =System.nanoTime()
  val r = l.foldRight(List[A]())((a,b) => {
    @annotation.tailrec
    def go(n: Int, c: A, acc: List[A]): List[A] = {
      if(n > 0) go(n - 1, c, c :: acc)
      else acc
    }
    go(a._1, a._2, b)
  })
  val y =System.nanoTime()
  println("Time: " + (y - x).toString)
  r
}

//slower
def decode2[A](ls: List[(Int, A)]): List[A] = {
  val x =System.nanoTime()
  val r = ls flatMap { e => List.fill(e._1)( e._2) }
  val y =System.nanoTime()
  println("Time: " + (y - x).toString)
  r
}

val dco = decode(List((5000,"a"), (2000,"b"), (5555,"c"), (345452,"a"), (45454,"d")))

val dco2 = decode2(List((2,"a"), (1,"b"), (1,"c"), (3,"a"), (1,"d")))


def encodeDirect[A](l: List[A]): List[(Int, A)] ={
  if(l.isEmpty) Nil
  else {
    val (packed, next) = l span {_ == l.head}
    (packed.length, packed.head) :: encodeDirect(next)
  }
}

val ecd = encodeDirect(List("a", "a", 1, "c", "a", "a", "a", "d"))



val s = "hi there"


def rotateLeft[A](n: Int, ls: List[A]): List[A] = ls match {
  case h :: t if n > 0 => rotateLeft(n - 1, t ::: List[A](h))
  case _ => ls
}

val rotateL = rotateLeft(5, List(1,2,3,4))


def rotate[A](n: Int, ls: List[A]): List[A] = {
  val boundedIndex = if(ls.isEmpty) 0 else n % ls.length
  if(boundedIndex < 0) rotate(boundedIndex + ls.length, ls)
  else (ls drop boundedIndex) ::: (ls take boundedIndex)
}

val rot = rotate(-2, List(1,2,3,4))

val rot2 = rotate(3, List(1,2,3,4))

val  b = -5 % 4
