//tail recursive
def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

gcd(14, 21)

// non-tail recursive
def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)

factorial(4)
factorial(5)
// gets stack-overflow
//factorial(1 << 31)

def factorial_tailRec(n: Int): Int = {
  def fact_it(curr: Int, prod: Int): Int =
    if (curr <= 0) prod else fact_it(curr - 1, prod * curr)

  fact_it(n, 1)
}

factorial_tailRec(4)
factorial_tailRec(5)
// doesn't get stack-overflow
factorial_tailRec(1 << 31)