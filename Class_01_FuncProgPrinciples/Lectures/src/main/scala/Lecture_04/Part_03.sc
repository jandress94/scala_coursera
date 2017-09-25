abstract class IntSet
class EmptyIntSet extends IntSet
class NonEmptyIntSet extends IntSet

// if you give assertAllPos an EmptyIntSet, it will return an EmptyIntSet
// if you give assertAllPos a NonEmptyIntSet, it will return a NonEmptyIntSet
def assertAllPos[S <: IntSet](r: S): S



val a: Array[NonEmptyIntSet] = Array(new NonEmptyIntSet)
val b: Array[IntSet] = a
b(0) = new EmptyIntSet
val s: NonEmptyIntSet = a(0)