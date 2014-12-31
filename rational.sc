object rational {
// x/y
class Rational(x: Int, y: Int) {
  require(y!=0, "denominator should be none zeor")
  
  def this(x: Int) = this(x, 1)
  
  val numer = x / g
  val denom = y / g
  
  def < (rhs: Rational) =
    this.numer * rhs.denom < rhs.numer * this.denom
  
  def max(rhs: Rational) =
    if(this < rhs) rhs else this
  
  def unary_- : Rational = {
    new Rational(-numer, denom)
  }
  
  def + (rhs : Rational): Rational = {
    new Rational(
     (numer * rhs.denom + denom * rhs.numer),
     denom * rhs.denom);
  }
  
  def - (rhs: Rational): Rational = this + (-rhs)
  
  override def toString = {
    numer + "/" + denom
  }
  
  private def gcd(a: Int, b:Int): Int = {
    if(b==0) a else gcd(b, a % b)
  }
  
  private def g = gcd(x,y)
  
}

val a = new Rational(1,2)                         //> a  : rational.Rational = 1/2
-a                                                //> res0: rational.Rational = 1/-2

val b = new Rational(2, 7)                        //> b  : rational.Rational = 2/7

b + a                                             //> res1: rational.Rational = 11/14
a - b                                             //> res2: rational.Rational = 3/14
b + b                                             //> res3: rational.Rational = 4/7
b < a                                             //> res4: Boolean = true
a max b                                           //> res5: rational.Rational = 1/2

val c = new Rational(3)                           //> c  : rational.Rational = 3/1

}
