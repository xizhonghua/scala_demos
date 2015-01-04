object Main extends App {
	override def main(args: Array[String]) = {
		def a = new NonEmptySet(3, EmptySet, EmptySet)
		def b = a incl 4
		def c = b incl 2
		def d = new NonEmptySet(7, EmptySet, EmptySet)
		println(a)
		println(b)
		println(c)
		println(a union d)
		println(a contains 3)
		println(b contains 2)
		println(c contains 2)
	}
}

// define an abstract class
abstract class IntSet {
	def incl(x: Int): IntSet
	def contains(x : Int): Boolean
	def union(other: IntSet): IntSet
}

// singleton object
object EmptySet extends IntSet {
	def incl(x: Int): IntSet = new NonEmptySet(x, EmptySet, EmptySet);
	def contains(x: Int): Boolean = false
	def union(other: IntSet): IntSet = other;
	override def toString = "#"
}

class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	def contains(x: Int): Boolean =
		if (x < elem) left contains x
		else if (x > elem) right contains x
		else true
	def incl(x: Int): IntSet = 
		if (x < elem) new NonEmptySet(elem, left incl x, right)
		else if (x > elem) new NonEmptySet(elem, left, right incl x)
		else this
	def union(other: IntSet) =
		((left union right) union other) incl elem
	override def toString = "{" + left + elem + right + "}"
}

// output
// {#3#}
// {#3{#4#}}
// {{#2#}3{#4#}}
// {{#3#}7#}
// true
// false
// true
