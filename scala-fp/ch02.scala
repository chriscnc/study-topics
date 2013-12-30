def fib(n: Int): Int = {

  def loop(n: Int, nm2: Int, nm1: Int): Int = {
    if (n == 1) nm2
    else if (n == 2) nm1
    else loop(n-1, nm1, nm2 + nm1) 
  }
  loop(n, 0, 1)
} 

def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  def loop(n: Int): Boolean = {
    if(n <= 1) true
    else 
      if (gt(as(n), as(n-1))) loop(n - 1)
      else false
  }
  loop(as.length - 1) 
}

def intGT(x: Int, y: Int): Boolean = x > y
def strGT(x: String, y: String): Boolean = x > y

val a1 = Array(1,2,3,4,5)
val a2 = Array()
val a3 = Array(2)
val a4 = Array(1,2,4,3,5)

assert(isSorted(a2, intGT) == true)
assert(isSorted(a3, intGT) == true)
assert(isSorted(a1, intGT) == true)
assert(isSorted(a4, intGT) == false)


class MyInt(self: Int) {
  def lessThan(x: Int) = self < x
}

def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))