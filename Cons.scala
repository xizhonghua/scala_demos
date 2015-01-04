object Main extends App {
	override def main(args: Array[String])
	{
		def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])
		
		println(singleton(1))
		println(singleton(true))
		
		val l1 = new Cons(2, new Cons(3, new Nil))
		var l2 = new Cons(1, l1)
		println(l1)
		println(l2)
		println(l2.nth(2))
		println(l2.nth(-1))
		println(l2.nth(4))
	}
}

trait List[T] {
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
	def nth(n: Int): T
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	def isEmpty = false
	override def nth(n: Int): T = {
		if (n==1) head
		else tail.nth(n-1)
	}
	override def toString = "{" + head + ',' + tail + "}"
}

class Nil[T] extends List[T] {
	def isEmpty = true;
	def head: Nothing = throw new NoSuchElementException("Nil.head")
	def tail: Nothing = throw new NoSuchElementException("Nil.tail")
	override def nth(n: Int) = throw new IndexOutOfBoundsException
	override def toString = "Nil"
}



// output
//{1,Nil}
//{true,Nil}
//{2,{3,Nil}}
//{1,{2,{3,Nil}}}
//2
//exception
