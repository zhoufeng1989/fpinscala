import part13.IO
import scala.io.StdIn.readLine

def Readline: IO[String] = IO {readLine}

def PrintLine(msg: String): IO[Unit] = IO {println(msg)}

def converter: IO[Unit] = for {
  _ <- PrintLine("Input a number")
  d <- Readline.map(_.toDouble)
  _ <- PrintLine(d.toString())
} yield ()

converter.run
