def abs(x: Int) = if (x >= 0) x else -x

abs(4)
abs(-5)

// difference between def and val
def loop: Int = loop
def rename_loop = loop
//The line below goes into infinite loop
//val evaluate_loop = loop

// ensuring short-circuit evaluation
def and(x: Boolean, y: => Boolean) = if (x) y else false
def or(x: Boolean, y: => Boolean) = if (x) true else y

def loopBoolean : Boolean = loopBoolean

and(true, true)
and(true, false)
and(false, true)
and(false, false)
and(false, loopBoolean)

or(true, true)
or(true, false)
or(false, true)
or(false, false)
or(true, loopBoolean)