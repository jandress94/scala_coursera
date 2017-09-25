// higher-order functions
// the below is the equivalent of \Sum\limits_{i = 1}^{b} f(i)
def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

def sumInts(a: Int, b: Int) = sum(id, a, b)
def sumCubes(a: Int, b: Int) = sum(cube, a, b)
def sumFactorials(a: Int, b: Int) = sum(fact, a, b)

def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x
def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)

sumInts(0, 5)
sumCubes(1, 3)
sumFactorials(0, 6)

def sumCubesAnonFunc(a: Int, b: Int) = sum(x => x * x * x, a, b)

sumCubes(5, 13)
sumCubesAnonFunc(5, 13)

// tail-recursive version
def sum_tailRec(f: Int => Int, a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

def sumInts_tailRec(a: Int, b: Int) = sum_tailRec(x => x, a, b)

sumInts(1, 100)
sumInts_tailRec(1, 100)