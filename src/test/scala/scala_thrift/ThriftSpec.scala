package scala_thrift

import org.specs._
import org.specs.runner._

class ThriftSpecTest extends JUnit4(ThriftSpec)
object ThriftSpec extends Specification {
  "The Thrift IDL parser" should {
    val p = new Parser
    import p._

    def parseExpression[T](expr: String, p: Parser[T]) = {
      phrase(p)(new lexical.Scanner(expr))
    }

    "parse int constants" in {
      parseExpression("1234", IntConstant) must haveClass[Success[String]]
      parseExpression("-1234", IntConstant) must haveClass[Success[String]]
      parseExpression("+1234", IntConstant) must haveClass[Success[String]]
    }
    
    "parse double constants" in {
      parseExpression("1.23", DoubleConstant) must haveClass[Success[String]]
      parseExpression("-1.23", DoubleConstant) must haveClass[Success[String]]
      parseExpression("1e10", DoubleConstant) must haveClass[Success[String]]
    }

    "parse single quoted string literals" in {
      parseExpression("'string lit'", Literal) must haveClass[Success[String]]
    }

    "parse double quoted string literals" in {
      parseExpression("\"string lit\"", Literal) must haveClass[Success[String]]
    }

    "parse list constants" in {
      parseExpression("[1, -2.0e3, 3, wee, 'waldo']", ConstList) must haveClass[Success[String]]
    }
    
    "parse map constants" in {
      parseExpression("{'jorge': 23, 'tony': 24.5}", ConstMap) must haveClass[Success[String]]
    }

    "parse all base types" in {
      for (t <- baseTypes) {
        parseExpression(t, BaseType) must haveClass[Success[String]]
      }
    }

    "parse list types" in {
      for (t <- baseTypes) {
        parseExpression("list<" + t + ">", ListType) must haveClass[Success[String]]
      }
    }
    
    "parse set types" in {
      for (t <- baseTypes) {
        parseExpression("set<" + t + ">", SetType) must haveClass[Success[String]]
      }
    }
    
    "parse map types" in {
      for (t1 <- baseTypes; t2 <- baseTypes) {
        parseExpression("map<" + t1 + ", " + t2 + ">", MapType) must haveClass[Success[String]]
      }
    }
    
    "parse typedefs" in {
      for (t <- baseTypes) {
        val name = "Wee"
        parseExpression("typedef " + t + " " + name, Typedef) must haveClass[Success[String]]
      }
    }
    
    "parse enum definitions" in {
      parseExpression("""
        enum Operation {
          ADD = 1,
          SUBTRACT = 2,
          MULTIPLY = 3,
          DIVIDE = 4
        }
      """, Enum) must haveClass[Success[String]]
    }
  }
}
