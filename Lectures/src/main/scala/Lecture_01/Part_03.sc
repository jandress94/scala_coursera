def loop : Int = loop

def constOne_callByValue(x: Int) = 1
def constOne_callByName(x: => Int) = 1

//Terminates
println(constOne_callByName(loop))
//Doesn't terminate
println(constOne_callByValue(loop))