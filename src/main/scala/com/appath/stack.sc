import scala.reflect.runtime.universe._

class Stack[E: TypeTag] (protected val xs: List[E]) {
  def this() = this(Nil)

  def push(elem: E): Stack[E] = new Stack(elem :: xs)

  def pop: (Stack[E], E) = {
    if (xs.nonEmpty) (new Stack(xs.tail), xs.head)
    else throw new NoSuchElementException("Exception: Calling pop on an empty stack")
  }

  def average(implicit num: Numeric[E]) =
    typeOf[E] match {
      case t if t =:= typeOf[Int] =>
        num.toInt(xs.sum[E]) / xs.size

      case t if t =:= typeOf[Double] =>
        num.toDouble(xs.sum[E]) / xs.size

      case _ => // ignore rest
    }

  override def toString() = xs.mkString("Stack(", ", ", ")")
}

object Stack {
  def empty[E: TypeTag] = new Stack[E](List())
}

Stack.empty[Int].push(1).push(2).push(3)

Stack.empty[Double].push(1).push(2).push(3)

Stack.empty[Int].push(1).push(2).push(3).average
Stack.empty[Double].push(1).push(2).push(3).average


Stack.empty[String].push("alpha").push("gamma").push("delta").average

