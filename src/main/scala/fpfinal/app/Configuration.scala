package fpfinal.app

import cats._
import cats.data._
import cats.implicits._
import fpfinal.service.{
  ExpenseService,
  LiveExpenseService,
  LivePersonService,
  PersonService
}

object Configuration {
  type IsValid[A] = Validated[NonEmptyChain[String], A]
  case class AppState()
  type Error = String
  type ErrorOr[A] = Either[Error, A]
  type St[A] = StateT[ErrorOr, AppState, A]
  type SuccessMsg = String
  type Environment = ExpenseService
    with PersonService
    with Console
    with Controller
  val liveEnv: Environment = new LiveExpenseService
    with LivePersonService
    with LiveConsole
    with LiveController
  type AppOp[A] = ReaderT[St, Environment, A]
  def readEnv: AppOp[Environment] = ReaderT.ask[St, Environment]
}