package fpfinal.app

import fpfinal.app.Configuration.{AppOp, Environment, St, SuccessMsg, readEnv}
import cats._
import cats.implicits._
import Syntax._

import scala.annotation.tailrec

object App {
  val ME = MonadError[AppOp, String]

  def printOptions: AppOp[Unit] = {
    def mkOptionsString(commands: List[Command]): String = {
      val header: String = "Please select an option: "
      val commandsList: List[String] = commands.zipWithIndex.map {
        case (c, i) => s"($i) ${c.show}"
      }
      (header :: commandsList).mkString("\n")
    }

    for {
      env <- readEnv
      allCommands = env.controller.getAllCommands
      options = mkOptionsString(allCommands.toList)
      _ <- env.console.printLine(options).toAppOp
    } yield ()
  }

  def readCommand(): AppOp[Int] = {
    for {
      env <- readEnv
      option <- env.console.readLine("Your option: ").toAppOp
      cmdNumber <- ME.fromOption(option.toIntOption, "Invalid option selected")
    } yield cmdNumber
  }

  def run(): AppOp[Unit] = {
    def loop: AppOp[Unit] =
      for {
        env <- readEnv
        _ <- printOptions
        commandNumber <- readCommand()
        command = env.controller.getCommandByNumber(commandNumber)
        successMsg <- command.fold(
          ME.raiseError[SuccessMsg]("Command not found")
        )(_.execute())
        _ <- env.console.printLine(successMsg).toAppOp
        _ <- if (command.exists(_.isExit)) ME.unit else loop
      } yield ()

    ME.handleErrorWith(loop) { errorMessage =>
      readEnv.map(_.console.printLine(errorMessage)) >> run()
    }
  }
}
