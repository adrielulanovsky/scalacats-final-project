package fpfinal.model

import cats._
import cats.data._
import cats.implicits._
import fpfinal.app.Configuration.IsValid
import fpfinal.common.Validations._

import scala.collection.immutable.SortedSet

class Expense private (
    val payer: Person,
    val amount: Money,
    val participants: NonEmptySet[Person]
) {
  def amountByParticipant: Money =
    amount
      .divideBy(participants.length + 1)
      .get // safe get because divisor is never 0
  override def toString: String =
    s"Expense(${payer.show}, ${amount.show}, ${participants.show}"
}

object Expense {
  def unsafeCreate(
      payer: Person,
      amount: Money,
      participants: List[Person]
  ): Expense =
    new Expense(
      payer,
      amount,
      NonEmptySet.fromSetUnsafe(SortedSet.from(participants))
    )

  def create(
      payer: Person,
      amount: Money,
      participants: List[Person]
  ): IsValid[Expense] = {
    (
      nonEmptySet(participants),
      Validated.condNec(
        !participants.contains(payer),
        payer,
        "payer cannot be included in participants"
      )
    ).mapN { (ps, p) =>
      new Expense(p, amount, ps)
    }
  }

  implicit def eqExpense(implicit
      eqPerson: Eq[Person],
      eqMoney: Eq[Money],
      eqParticipants: Eq[List[Person]]
  ): Eq[Expense] =
    Eq.instance((e1, e2) =>
      e1.payer === e2.payer && e1.amount === e2.amount && e1.participants === e2.participants
    )
}
