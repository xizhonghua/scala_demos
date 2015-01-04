package week4

object lists {
  val fruit = List("apples", "oranges", "pears")  //> fruit  : List[String] = List(apples, oranges, pears)
  fruit.head                                      //> res0: String = apples
  fruit.tail.head                                 //> res1: String = oranges
  
  // :: == preprend
  val fruit2 = "apples" :: ( "oranges" :: ("pears" :: Nil))
                                                  //> fruit2  : List[String] = List(apples, oranges, pears)
  val fruit3 = "apples" :: "oranges" :: "pears" :: Nil
                                                  //> fruit3  : List[String] = List(apples, oranges, pears)
  val diag = List(List(1,0,0), List(0,1,0), List(0,0,1))
                                                  //> diag  : List[List[Int]] = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case Nil => ys
    case z :: zs => z :: concat(zs, ys)
  }                                               //> concat: [T](xs: List[T], ys: List[T])List[T]
  
  concat(List(1,2,3), List(4,5,6))                //> res2: List[Int] = List(1, 2, 3, 4, 5, 6)
  
  def reverse[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case y :: ys => concat(reverse(ys), List(y))
  }                                               //> reverse: [T](xs: List[T])List[T]
  
  reverse(List(3,2,1))                            //> res3: List[Int] = List(1, 2, 3)
  
  def removeAt[T](xs: List[T], n: Int): List[T] = xs match {
    case Nil => if (n>=0) throw new Error else Nil
    case y :: ys => if (n==0) ys else y :: removeAt(ys, n-1)
  }                                               //> removeAt: [T](xs: List[T], n: Int)List[T]
  
  removeAt(List('a','b','c','d'), 1)              //> res4: List[Char] = List(a, c, d)
  removeAt(List('a','b','c','d'), 3)              //> res5: List[Char] = List(a, b, c)
  removeAt(List('a','b','c','d'), 4)              //> java.lang.Error
                                                  //| 	at week4.lists$$anonfun$main$1.removeAt$1(week4.lists.scala:28)
                                                  //| 	at week4.lists$$anonfun$main$1.removeAt$1(week4.lists.scala:29)
                                                  //| 	at week4.lists$$anonfun$main$1.removeAt$1(week4.lists.scala:29)
                                                  //| 	at week4.lists$$anonfun$main$1.removeAt$1(week4.lists.scala:29)
                                                  //| 	at week4.lists$$anonfun$main$1.removeAt$1(week4.lists.scala:29)
                                                  //| 	at week4.lists$$anonfun$main$1.apply$mcV$sp(week4.lists.scala:34)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week4.lists$.main(week4.lists.scala:3)
                                                  //| 	at week4.lists.main(week4.lists.scala)
}
