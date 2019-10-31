// Aux pattern
// implicit resolution order, LowPriority pattern

sealed trait Boolean
trait True extends Boolean
trait False extends Boolean

trait Not[B <: Boolean] {
  type Out <: Boolean
}


object Not {
  type Aux[B <: Boolean, O <: Boolean] = Not[B] {type Out = O}
}

implicit val notTrue: Not.Aux[True, False] = new Not[True]  { type Out = False }
implicit val notFalse:Not.Aux[False, True] = new Not[False] { type Out = True }


case class For[A <: Boolean]() {
  def proveThatNotNotAIsA[
    NO <: Boolean
    , NNO <: Boolean](implicit
                      notA:Not.Aux[A, NO]
                      , notNotA: Not.Aux[NO, NNO]
                      , prove: A =:= NNO):Unit = ()
}


For[True].proveThatNotNotAIsA
For[False].proveThatNotNotAIsA

/**
 * AND
 */
trait And[A <: Boolean, B <: Boolean] {
  type Out <: Boolean
  def debug:String
}

trait LowPriorityAnd {
  implicit def elseAnd[A <: Boolean, B <: Boolean]:And.Aux[A,B,False] = new And[A,B] { type Out = False; def debug = "FALSE"}

}

object And extends LowPriorityAnd {
  type Aux[A <: Boolean, B <: Boolean, O <: Boolean] = And[A,B] { type Out = O }
  implicit val trueAndTrue:And.Aux[True,True,True] = new And[True,True] { type Out = True; def debug = "TRUE" }

}


def and[A <: Boolean, B <: Boolean](implicit and:And[A,B]):And[A,B] = and
and[True,False].debug
and[False,False].debug
and[False,True].debug
and[True,True].debug
// !(a + b) = !a * !b
// !(a * b) = !a + !b

// HOMEWORK:
def or[A <: Boolean, B <: Boolean](implicit or:Or[A,B]):Or[A,B] = or
or[True,False].debug
or[False,False].debug
or[False,True].debug
or[True,True].debug