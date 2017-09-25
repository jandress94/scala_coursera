def square(x: Double) = x * x
square(2)
square(5 + 4)
square(square(4))

def sumOfSquares(x: Double, y: Double) : Double = square(x) + square(y)

def loop: Int = loop