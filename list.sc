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
                                                   
}
