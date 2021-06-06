package example

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Repl {

  @tailrec
  def repl(ctx: Interpreter): Unit = {
    print("scheme> ")
    val input: String = readLine()
    if(input == "exit") return
    val ans = Parser(input)
    ans match {
      case Left(ex) =>
        println(ex)
        repl(ctx)
      case Right(expr) =>
        val res = try {
          ctx(expr)
        } catch {
          case e: InterpreterException => e
        }
        res match {
          case newCtx: Interpreter => repl(newCtx)
          case value: Value =>
            println(value)
            repl(ctx)
          case e: InterpreterException =>
            println(e)
            repl(ctx)
        }

    }
  }

  def main(args: Array[String]): Unit = {
    println("Welcome to Scheme REPL")
    repl(new Interpreter())
  }
}
