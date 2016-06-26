

object mergesort{
  def msort[A](xs: List[A])(implicit f: Ordering[A]): List[A] ={
    val n = xs.length / 2
    if(n == 0) xs
    else{
      def merge(xs: List[A], ys: List[A]): List[A] =
        (xs, ys) match{
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case (xh :: xt, yh :: yt) =>
            if(f.lt(xh, yh)) xh :: merge(xt, ys)
            else yh :: merge(xs, yt)
        }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }
}

val nums = List(2, -4, 5, 7, 1)
mergesort.msort(nums)