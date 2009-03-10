package scala_thrift

object ThriftIDL extends Parser {
  def parse(input: String) =
    phrase(Document)(new lexical.Scanner(input)) match {
      case Success(result, _) => Some(result)
      case _ => None
    }
}