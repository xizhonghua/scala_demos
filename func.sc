object func {
  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }                                               //> factorial: (n: Int)Int
  
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(c: Int, acc: Int): Int = {
      if (c > b) acc
      else loop(c + 1, f(c) + acc)
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int

  sum(x => x, 1, 10)                              //> res0: Int = 55
  sum(x => x * x, 1, 10)                          //> res1: Int = 385
  sum(factorial, 1, 10)                           //> res2: Int = 4037913
  
  
  def product(f:Int => Int)(a: Int, b: Int): Int = {
    def loop(c: Int, prod:Int): Int = {
      if(c>b) prod
      else loop(c+1, f(c) * prod)
    }
    loop(a,1)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int
  
  product(x=>x)(1, 5)                             //> res3: Int = 120
  
  def fact(n: Int): Int =
    product(x=>x)(1, n)                           //> fact: (n: Int)Int
        
  fact(10)                                        //> res4: Int = 3628800
}
