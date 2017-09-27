abstract class JSON
case class JSeq (elems: List[JSON]) extends JSON
case class JObj (bindings: Map[String, JSON]) extends JSON
case class JNum (num: Double) extends JSON
case class JStr (str: String) extends JSON
case class JBool (b: Boolean) extends JSON
case object JNull extends JSON

val data = JObj(Map(
  "firstName" -> JStr("Jim"),
  "lastName" -> JStr("Andress"),
  "address" -> JObj(Map(
    "street" -> JStr("123 Main St"),
    "state" -> JStr("GA"),
    "postalCode" -> JNum(30306)
  )),
  "phoneNums" -> JSeq(List(
    JObj(Map(
      "type" -> JStr("home"), "num" -> JStr("123-456-7890")
    )),
    JObj(Map(
      "type" -> JStr("cell"), "num" -> JStr("123-456-0987")
    ))
  ))
))

def show(json: JSON): String = json match {
  case JSeq(elems) => "[" + (elems map show mkString ", ") + "]"
  case JObj(bindings) =>
    val assoc = bindings map {
      case (key, value) => "\"" + key + "\": " + show(value)
    }
    "{" + (assoc mkString ", ") + "}"
  case JNum(num) => num.toString
  case JStr(str) => '"' + str + '"'
  case JBool(b) => b.toString
  case JNull => "null"
}

show(data)




val f: String => String = { case "ping" => "pong" }
f("ping")
//f("abc")

val g: PartialFunction[String, String] = { case "ping" => "pong" }
g.isDefinedAt("ping")
g.isDefinedAt("abc")


val f2: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: y :: rest => "two"
}
f2.isDefinedAt(List(1, 2, 3))

val f3: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: rest =>
    rest match {
      case Nil => "two"
    }
}

f3.isDefinedAt(List(1, 2, 3))