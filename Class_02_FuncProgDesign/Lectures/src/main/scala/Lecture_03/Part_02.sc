class BankAccount {
  private var balance = 0

  def deposit(amount: Int): Unit = {
    if (amount > 0) balance += amount
  }

  def withdraw(amount: Int): Int =
    if (0 < amount && amount <= balance) {
      balance = balance - amount
      balance
    } else throw new Error("insufficient funds")
}

// The lines below show y and z aren't operationally equivalent because a difference can be seen when two are passed in vs. just one

val x = new BankAccount
val y = new BankAccount
val z = new BankAccount

def testOpEquiv(a: BankAccount, b: BankAccount) = {
  a deposit 30
  b withdraw 20
}

testOpEquiv(x, x)
//testOpEquiv(y, z)


// however, the lines give evidence that y and z are the same

val x1 = new BankAccount
val y1 = new BankAccount
val z1 = y1

testOpEquiv(x1, x1)
testOpEquiv(y1, z1)


// substitution model no longer works, because using the substitution model, the second example would reduce to the first
