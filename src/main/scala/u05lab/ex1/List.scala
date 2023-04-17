package u05lab.ex1

import u05lab.ex1.List

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()

  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())((a, value) => f(a) append value)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  private def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  override def toString: String = this match
    case h :: t => s"$h :: $t"
    case _ => "Nil"

  /** EXERCISES */
  def zipRight: List[(A, Int)] = this match
    // L'ultimo elemento della lista è il primo elemento processato
    // la map viene applicata all'intera lista zippata con indice 0
    case h :: t => (h, -1) :: t.zipRight.map((e: (A, Int)) => (e._1, e._2 + 1))
    case _ => Nil()

  def partition(pred: A => Boolean): (List[A], List[A]) = (this.filter(pred(_)), this.filter(!pred(_)))

  /**
   * Splits the list at the first occurrence of the element that does not satisfy the predicate
   *
   * @param pred the predicate
   * @return a pair of lists, the first one contains the elements that satisfy the predicate, the second one contains the
   */
  def span(pred: A => Boolean): (List[A], List[A]) = this match
    case h :: _ if !pred(h) => (Nil(), this)
    case h :: t => (h :: t.span(pred)._1, t.span(pred)._2)

  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A = this match
    case h :: t if t.isEmpty => h
    case h :: t => op(h, t.reduce(op))
    case _ => throw UnsupportedOperationException("reduce on empty list")

  def takeRight(n: Int): List[A] = this match
    // Base case
    case _ if n >= this.length => this
    // The length of the list decrease at each iteration because we do the takeRight on the tail
    case _ :: t => t.takeRight(n)

  def collect[B](partialFunction: PartialFunction[A,B]): List[B] = this.filter(partialFunction.isDefinedAt) match
    case h :: t if partialFunction.isDefinedAt(h) => partialFunction(h) :: t.collect(partialFunction)
    case _ => Nil()


// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference: List[Int] = List(1, 2, 3, 4)
  //println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  //println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  //println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  //println(reference.span(_ < 3)) // (List(1,  2), List(3, 4))
  //println(reference.reduce(_ + _)) // 10
  //  try Nil.reduce[Int](_ + _)
  //    catch case ex: Exception => println(ex) // prints exception
  //  println(List(10).reduce(_ + _)) // 10
  // println(reference.takeRight(3)) // List(2, 3, 4)
  // Test the collect method
  val partialFunction: PartialFunction[Int, Int] = { case x if x % 2 == 0 => x }
  println(reference.collect(partialFunction)) // List(2, 4)
