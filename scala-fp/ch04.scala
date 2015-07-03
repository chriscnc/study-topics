import scala.{Option => _, Either => _, _}
import scala.math

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def getOrElse[B >: A](default: B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing] 


object Option {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a,b))
    } 
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    // xs.map(x => math.pow(x - mean, 2)).sum / xs.length
    sys.error("todo")
  } 

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {

    
  }
}
