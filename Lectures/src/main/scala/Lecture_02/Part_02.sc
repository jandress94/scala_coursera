// currying

def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumF(a + 1, b)
  sumF
}

def sumInts = sum(x => x)
def sumCubes = sum(x => x * x * x)


sum(x => x * x)(1, 5) // = 1^2 + 2^2 + 3^2 + 4^2 + 5^2

def sum_rewrite(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f)(a + 1, b)

sum_rewrite(x => x * x)(1, 5)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)

def factorial(n: Int) = product(x => x)(1, n)

factorial(5)
factorial(6)

def genOverInterval(combine: (Int, Int) => Int, id: Int, f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) id
  else combine(f(a), genOverInterval(combine, id, f)(a + 1, b))

def sum_gen(f: Int => Int)(a: Int, b:Int) = genOverInterval((x, y) => x + y, 0, f)(a, b)

sum_gen(x => x * x)(1, 5)
