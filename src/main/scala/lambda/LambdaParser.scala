package lambda

import scala.util.parsing.combinator._

class LambdaParser extends RegexParsers {

  def varName: Parser[String] = """[a-z|A-Z]+""".r

  def variable(ctx: List[String]): Parser[Var] = varName ^? ({ case name if ctx.contains(name) => Var(ctx.indexOf(name)) }, { name => s"no var in scope with name $name" })

  def hole: Parser[Hole] = ("""\d+""".r <~ "?") ^? ({ case num => Hole(num.toInt) }, { num => s"needs to be a num: $num" })

  def scope(v: String, ctx: List[String]): Parser[LambdaExp] = (("." ~> exp(v :: ctx))) ^^ { case bod => bod }

  def lam(ctx: List[String]): Parser[Lam] = (("λ" ~> varName) >> { v => scope(v, ctx) }) ^^ { scope => Lam(scope) }

  def app(ctx: List[String]): Parser[App] = (exp(ctx) ~ exp(ctx)) ^^ { case f ~ a => App(f, a) }

  def exp(ctx: List[String]): Parser[LambdaExp] = ("{" ~> exp(ctx) <~ "}") | ("[" ~> exp(ctx) <~ "]") | ("(" ~> exp(ctx) <~ ")") | ("(" ~> app(ctx) <~ ")") |
    variable(ctx) | lam(ctx) | hole

}
object LambdaParser extends LambdaParser {

  implicit class SimpleCCParserHelper(val sc: StringContext) extends AnyVal {
    def lam(args: Any*): LambdaExp = parse(exp(List()), sc.standardInterpolator({ x => x }, args)) match {
      case Success(matched, _) => matched
      //      case Failure(msg, _)     => println("FAILURE: " + msg)
      //      case Error(msg, _)       => println("ERROR: " + msg)
    }
  }

}