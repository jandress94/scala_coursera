// loops
def power (x: Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  while (i > 0) { r = r * x; i = i - 1 }
  r
}

power(1.414, 6)

// loops can be modeled with functions
def myWhile(condition: => Boolean)(command: => Unit): Unit =
  if (condition) {
    command
    myWhile(condition)(command)
  } else ()

def myRepeatUntil(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (condition) () else myRepeatUntil(command)(condition)
}

// for loop cannot be modeled by a higher-order function
for (i <- 1 until 3; j <- "abc") println(i + " " + j)