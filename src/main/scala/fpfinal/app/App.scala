package fpfinal.app

import cats._
import cats.implicits._
import fpfinal.app.Configuration.{AppOp, readEnv}
import fpfinal.app.Syntax._

object App {
  val ME = MonadError[AppOp, String]

  def run(): AppOp[Unit] = {
    def printOptions: AppOp[Unit] = {
      def mkOptionsString(commands: List[Command]): String = {
        val header: String = "Please select an option: \n"
        val commandsList: List[String] = commands.zipWithIndex.map {
          case (c, i) => s"($i) ${c.show}"
        }
        (header :: commandsList).mkString("\n") + "\n"
      }

      for {
        env <- readEnv
        allCommands = env.controller.getAllCommands
        options = mkOptionsString(allCommands.toList)
        _ <- env.console.printLine(options).toAppOp
      } yield ()
    }

    def readCommandNumber(): AppOp[Int] = {
      for {
        env <- readEnv
        option <- env.console.readLine("Your option: ").toAppOp
        cmdNumber <-
          ME.fromOption(option.toIntOption, "Invalid option selected")
      } yield cmdNumber
    }

    def executeCommand: AppOp[Boolean] =
      for {
        env <- readEnv
        _ <- printOptions
        commandNumber <- readCommandNumber()
        command <- ME.fromOption(
          env.controller.getCommandByNumber(commandNumber),
          "Command not found"
        )
        _ <- env.console.printLine("").toAppOp
        successMsg <- command.execute()
        _ <- env.console.printLine(s"\n$successMsg\n", Console.Success).toAppOp
      } yield command.isExit

    def executeCommandWithRecovery: AppOp[Boolean] =
      ME.handleErrorWith(executeCommand)(error =>
        readEnv.flatMap(env =>
          env.console.printLine(s"\n$error\n", Console.Error).as(false).toAppOp
        )
      )

    ME.iterateUntil(executeCommandWithRecovery)(identity).void
  }
}
