package fpfinal.model

import cats._
import cats.implicits._

class DebtByPayee private (val debtByPayee: Map[Person, Money]) {
  def debtForPayee(person: Person): Option[Money] = debtByPayee.get(person)
  def allPayees(): List[Person] = debtByPayee.keySet.toList
}

object DebtByPayee {
  def fromExpense(expense: Expense): DebtByPayee =
    new DebtByPayee({
      expense.participants
        .map(p => Map(p -> expense.amountByParticipant))
        .combineAll
    })

  implicit val monoidDebtByPayee: Monoid[DebtByPayee] =
    Monoid[Map[Person, Money]].imap(x => new DebtByPayee(x))(_.debtByPayee)

  implicit val showDebtByPayee: Show[DebtByPayee] = Show.show { d =>
    d.allPayees()
      .foldMap(payee =>
        s"- ${payee.show}: ${d.debtForPayee(payee).getOrElse(Money.zero).show}\n"
      )
  }
}
