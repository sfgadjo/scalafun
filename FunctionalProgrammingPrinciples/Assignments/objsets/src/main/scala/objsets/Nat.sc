

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero: Boolean = true

  def successor: Nat = ???

  def +(that: Nat): Nat = ???

  def -(that: Nat): Nat = ???

  def predecessor: Nat = ???

}



class Succ(n: Nat) extends Nat
{
  def isZero: Boolean = false

  def successor: Nat =

  def +(that: Nat): Nat = {
    if(that.isZero) throw new IndexOutOfBoundsException
    else
  }

  def -(that: Nat): Nat = ???

  def predecessor: Nat = n
}