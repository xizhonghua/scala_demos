object rational {
// x/y
class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
  
  def neg : Rational = {
    new Rational(-numer, denom)
  }
  
  def add(rhs : Rational): Rational = {
    new Rational(
     (numer * rhs.denom + denom * rhs.numer),
     denom * rhs.denom);
  }
  
  def sub(rhs: Rational): Rational = add(rhs.neg)
  
  override def toString = {
    numer + "/" + denom
  }
}

val a = new Rational(1,2)                         //> a  : rational.Rational = 1/2
a.neg                                             //> res0: rational.Rational = -1/2

val b = new Rational(2, 7)                        //> b  : rational.Rational = 2/7

b.add(a)                                          //> res1: rational.Rational = 11/14
a.sub(b)                                          //> res2: rational.Rational = 3/14

}


  
  
