// discrete event simulator for circuits
import Lecture_03.Part_040506._

object sim extends Circuits with Parameters
import sim._

val in1, in2, sum, carry = new Wire


halfAdder(in1, in2, sum, carry)
probe("Sum", sum)
probe("Carry", carry)


in1 setSignal true
run()

in2 setSignal true
run()

in1 setSignal false
run()