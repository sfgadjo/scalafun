//package fpinscala.exercises.errorHandling
//
import scala.List
import scala.{Option => _, Some => _, None => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

sealed trait Option[+A]
{
  def map[A,B](f: A => B): Option[B] = this match{
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[A,B](f: A => Option[B]): Option[B] = this match{
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match{
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match{
    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match{
    case None => this
    case Some(a) => {
      if(f(a)) this
      else None
    }
  }
}

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (x => mean(xs.map(m => math.pow(x - m, 2))))

  def traverse[A,B](l: List[A])(f: A => Option[B]): Option[List[B]] =
    l.foldRight[Option[List[A]]}(Some(Nil))((option,acc) => Option.map2(f(option), acc)(_ :: _))

  def sequence[A](l: List[Option[A]]): Option[List[A]] =  Option.traverse(l)(x => x)
}