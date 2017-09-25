import Lecture_03.Rational

new Rational(1, 2)

def error(msg: String) = throw new Error(msg)

val x = null

val y:String = x

//value types can't be null, only reference types can be
//val z: Int = null

//lowest common ancestor of int and boolean is anyval
if (true) 1 else false