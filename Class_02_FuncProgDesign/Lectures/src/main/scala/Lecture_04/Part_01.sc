// pub/sub

trait Publisher {

  private var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber

  def unsubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber

  def publish(): Unit =
    subscribers.foreach(_.handler(this))
}

trait Subscriber {
  def handler(publisher: Publisher): Unit
}


class BankAccount extends Publisher {
  private var balance = 0

  def deposit(amount: Int): Unit =
    if (amount > 0) {
      balance = balance + amount
      publish()
    }

  def withdraw(amount: Int): Unit =
    if (0 < amount && amount <= balance) {
      balance -= amount
      publish()
    } else throw new Error("Insufficient funds")

  def currentBalance: Int = balance
}

class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute()

  private def compute(): Unit =
    total = observed.map(_.currentBalance).sum

  override def handler(publisher: Publisher): Unit = compute()

  def totalBalance = total
}

val a = new BankAccount
val b = new BankAccount
val c = new Consolidator(List(a, b))
c.totalBalance

a deposit 20
c.totalBalance

b deposit 30
c.totalBalance

a withdraw 15
c.totalBalance
