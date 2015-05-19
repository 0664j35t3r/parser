import scala.util.parsing.combinator.JavaTokenParsers

sealed trait Expression1a
case class String1a(str: String) extends Expression1a // wie in c++ das vererben

sealed trait Expression1b // 2+3i// -4-5*i// +4-i// i// 4
case class String1b(str: String) extends Expression1b

/**
 *  Parser definition using the Scala Parser Combinator Library
 *
class ExpParser1a extends JavaTokenParsers {
  def expression: Parser[Expression1a] = string1a // | mult | add

  private val string1a : Parser[String1a] =
    "a(cc|b)+a*".r ^^ {str => String1a(str.toString)}
}

object ParseExpression1a extends ExpParser1a {

  // The apply method is used to overload the () syntax
  // You can use it like this: ParseExpression(stringToParse)
  def apply(s: String): ParseResult[Expression1a] = {
    parseAll(expression, s)
  }
}

class ExpParser1b extends JavaTokenParsers {
  //An expression is either a number, a mult or a add
  // | the result is either the left or the right result, the left parser is tried first
  def expression: Parser[Expression1b] = string1b

  private val string1b : Parser[String1b] =
    ("((\\+|-)?([1-9][0-9]*|0|-[1-9][0-9]*))?" +
     "((\\+|-)?([1-9][0-9]*|0|-[1-9][0-9]*)?i)?").r ^^ {str => String1b(str.toString)}
}

object ParseExpression1b extends ExpParser1b {

  // The apply method is used to overload the () syntax
  // You can use it like this: ParseExpression(stringToParse)
  def apply(s: String): ParseResult[Expression1b] = {
    parseAll(expression, s)
  }
}

  /**
   *  Main object with a method to interpret an Expression and a main method to run the program
   */
  object SWPbsp7 {
    def interpret1a(expression: Expression1a): String = expression match {
      case String1a(str) => str
    }

    def interpret1b(expression: Expression1b): String = expression match {
      case String1b(str) => str
    }

    def autotest7a : Unit =
    {
      var loop : Int = 0
      val teststrings : Array[String] = Array("accbaaa","accccbcc","abbbbccaaaaa")
      while (loop < teststrings.length)
      {
        println("Testing # "+loop+": "+teststrings(loop))
        val input = teststrings(loop)
        val expression = ParseExpression1a(input).get // get returns the Expression if the 
parsing was successful, otherwise an exception is thrown
      val resultValue = interpret1a(expression)
        println(resultValue)
        loop += 1
      }
    }

    def autotest7b : Unit =
    {
      var loop : Int = 0
      val teststrings : Array[String] = Array("4+5i","-5+i","i","-i","5","-5","-5-i","4567-234234i","+i")
      while (loop < teststrings.length)
      {
        println("Testing # "+loop+": "+teststrings(loop))
        val input = teststrings(loop)
        val expression = ParseExpression1b(input).get 
        val resultValue = interpret1b(expression)
        println(resultValue)
        loop += 1
      }
    }

    def main(args: Array[String]): Unit = {
      args(0) match
      {
        case "7a" =>
          args(1) match
          {
            case "auto" =>
              autotest7a
            case _ =>
              val input = args(1)
              val expression = ParseExpression1a(input).get
              val resultValue = interpret1a(expression)
              print(resultValue)
          }
        case "7b" =>
          args(1) match
          {
            case "auto" =>
              autotest7b
            case _ =>
              val input = args(1)
              val expression = ParseExpression1b(input).get
              val resultValue = interpret1b(expression)
              print(resultValue)
          }
      }
    }
  }
