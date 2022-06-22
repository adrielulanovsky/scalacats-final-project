package fpfinal

import cats.data.{NonEmptyChain, State}
import cats.implicits._
import fpfinal.app.AppState
import fpfinal.app.Configuration.IsValid
import fpfinal.common.IO
import fpfinal.common.IO.{Done, FlatMap, More}
import fpfinal.model._
import fpfinal.service.ExpenseService.{ExpenseOp, ExpenseState}
import fpfinal.service.PersonService.{PersonOp, PersonState}
import org.scalacheck.{Arbitrary, Gen}

trait Generators {

  /**
    * TODO #3a: implement an arbitrary of Person.
    *
    * You can use Person.unsafeCreate as long as you take
    * care of only producing valid values (check out
    * the constraints in Person.create) .
    */

  /*
  *     * - The name should not be empty
  * - The name should only contain letters
  * - The name should be at most 32 chars long

  * */
  implicit val personArb: Arbitrary[Person] = Arbitrary {
    for {
      nameSize <- Gen.choose(1, 32)
      name <- Gen.stringOfN(nameSize, Gen.alphaChar)
    } yield Person.unsafeCreate(name)
  }

  implicit val moneyArb: Arbitrary[Money] = Arbitrary {
    Gen.choose(1, 1e9.toInt).map(Money.unsafeCreate)
  }

  /**
    * TODO #3b: Use the provided arbitraries and the Expense.unsafeCreate method
    * to create an instance of Arbitrary[Expense]
    */

    /*
    *     * - The participants list should not be empty
    * - The payer should not be included in the participants

    * */
  implicit def expenseArb(implicit
      arbPerson: Arbitrary[Person],
      arbMoney: Arbitrary[Money]
  ): Arbitrary[Expense] = Arbitrary {
      for {
        payer <- arbPerson.arbitrary
        amount <- arbMoney.arbitrary
        participants <- Gen.nonEmptyListOf(arbPerson.arbitrary)
        participantsWithoutPayer = participants.filterNot(_ === payer)
      } yield Expense.unsafeCreate(payer, amount, participantsWithoutPayer)

  }

  implicit val payeeDebtArb: Arbitrary[DebtByPayee] = Arbitrary {
    Gen
      .listOf(expenseArb.arbitrary)
      .map(_.map(DebtByPayee.fromExpense).combineAll)
  }

  implicit val payerDebtArb: Arbitrary[DebtByPayer] = Arbitrary {
    Gen
      .listOf(expenseArb.arbitrary)
      .map(_.map(DebtByPayer.fromExpense).combineAll)
  }

  implicit def functionArb[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary {
      arbA.arbitrary.map(a => (_: A) => a)
    }

  implicit def personStateArb(implicit
      personArb: Arbitrary[Person]
  ): Arbitrary[PersonState] =
    Arbitrary {
      Gen
        .mapOf[String, Person](personArb.arbitrary.map(p => (p.name, p)))
        .map(PersonState.apply)
    }

  implicit def ioArb[A](implicit arbA: Arbitrary[A]): Arbitrary[IO[A]] = {
    val doneGen: Gen[Done[A]] = arbA.arbitrary.map(Done.apply)
    val moreGen: Gen[More[A]] = arbA.arbitrary.map(a => More(() => Done(a)))
    val flatMapGen: Gen[FlatMap[A, A]] =
      for {
        fa <- Gen.oneOf(doneGen, moreGen)
        ta <- Gen.oneOf(doneGen, moreGen)
      } yield FlatMap(ta, (_: A) => fa)

    Arbitrary(Gen.oneOf(doneGen, moreGen, flatMapGen))
  }

  implicit def expenseStateArb(implicit
      expenseArb: Arbitrary[Expense]
  ): Arbitrary[ExpenseState] =
    Arbitrary {
      Gen.listOf(expenseArb.arbitrary).map(ExpenseState.apply)
    }

  implicit def appStateArb(implicit
      arbPersonState: Arbitrary[PersonState],
      arbExpenseState: Arbitrary[ExpenseState]
  ): Arbitrary[AppState] =
    Arbitrary {
      for {
        ps <- arbPersonState.arbitrary
        es <- arbExpenseState.arbitrary
      } yield AppState(es, ps)
    }

  /**
    * TODO #3c: implement an arbitrary of PersonOp[A].
    *
    * One possible implementation is to create a State
    * whose run function ignores the current state and just
    * sets the state and value to random values.
    */
  implicit def personOpArb[A](implicit
      arbA: Arbitrary[A],
      arbPersonState: Arbitrary[PersonState]
  ): Arbitrary[PersonOp[A]] = Arbitrary {
    for {
      a <- arbA.arbitrary
      personState <- arbPersonState.arbitrary
    } yield {State[PersonState, A]{ _ => (personState, a) }}
  }

  implicit def isValidArb[A](implicit
      arbA: Arbitrary[A]
  ): Arbitrary[IsValid[A]] = {
    val validGen: Gen[IsValid[A]] = arbA.arbitrary.map(_.validNec[String])
    val invalidGen: Gen[IsValid[A]] = Gen
      .nonEmptyListOf(Arbitrary.arbitrary[String])
      .map(xs => NonEmptyChain.fromSeq(xs).get.invalid[A])
    Arbitrary(Gen.oneOf(validGen, invalidGen))
  }

  implicit def expenseOpGen[A](implicit
      arbA: Arbitrary[A],
      expenseStateArb: Arbitrary[ExpenseState]
  ): Arbitrary[ExpenseOp[A]] =
    Arbitrary {
      for {
        a <- arbA.arbitrary
        es <- expenseStateArb.arbitrary
      } yield State((_: ExpenseState) => (es, a))
    }
}
