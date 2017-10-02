// functional reactive programming
import Lecture_04.frp._

class BankAccount {
  val balance = new Var(0)

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      val b = balance()
      balance() = b + amount
    }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance()) {
      val b = balance()
      balance() = b - amount
    } else throw new Error("Insufficient funds")
}

def consolidated(accts: List[BankAccount]): Signal[Int] =
  new Signal(accts.map(_.balance()).sum)

val a = new BankAccount()
val b = new BankAccount()
val c = consolidated(List(a, b))
c()

a deposit 50
c()

b deposit 10
c()

a withdraw 25
c()