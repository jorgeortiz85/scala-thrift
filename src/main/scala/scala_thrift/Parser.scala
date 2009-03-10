package scala_thrift

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

class Parser extends StdTokenParsers with ImplicitConversions {
  type Tokens = Lexer
  val lexical = new Tokens

  val namespaceScopes = List("*", "cpp", "java", "py", "perl", "rb", "cocoa", "cshapr", "php")
  lexical.reserved ++= List("namespace", "cpp_namespace", "cpp_include", "cpp_type", "java_package",
    "cocoa_prefix", "csharp_namespace", "php_namespace", "py_module", "perl_package",
    "ruby_namespace", "smalltalk.category", "smalltalk_category", "smalltalk.prefix", 
    "smalltalk_prefix", "xsd_all", "xsd_optional", "xsd_nillable", "xsd_attrs", "include", "void",
    "senum", "map", "list", "set", "async", "typedef", "struct", "exception", "extends", "throws",
    "service", "enum", "const", "required", "optional", "abstract", "and", "args", "as", "assert",
    "break", "case", "class", "continue", "declare", "def", "default", "del", "delete", "do",
    "elif", "else", "elseif", "except", "exec", "false", "finally", "float", "for", "foreach",
    "function", "global", "goto", "if", "implements", "import", "in", "inline", "instanceof",
    "interface", "is", "lambda", "native", "new", "not", "or", "pass", "public", "print",
    "private", "protected", "raise", "return", "sizeof", "static", "switch", "synchronized", "this",
    "throw", "transient", "true", "try", "unsigned", "var", "virtual", "volatile", "while", "with",
    "union", "yield") ++ BaseType.map.keySet
  lexical.delimiters ++= List("{", "}", "[", "]", "<", ">", "(", ")", ",", ":", ";", "=")

  def document        = rep(header) ~ rep(definition)
  def header          = include | cppInclude | namespace
  def include         = "include" ~ literal
  def cppInclude      = "cpp_include" ~ literal
  def namespace       = ("namespace" ~ ((namespaceScope ~ identifier) |
                                        ("smalltalk.category" ~ stIdentifier) |
                                        ("smalltalk.prefix" ~ identifier))) |
                        ("php_namespace" ~ literal) |
                        ("xsd_namespace" ~ literal)
  def namespaceScope  = accept("namespace scope", {
    case lexical.Identifier(s) if namespaceScopes.contains(s) => 
  })
  def definition      = const | typedef | enum | senum | struct | exception | service
  def const           = "const" ~ fieldType ~ identifier ~ "=" ~ constValue ~ opt(listSeparator)
  def typedef         = "typedef" ~ definitionType ~ identifier
  def enum            = "enum" ~ identifier ~ "{" ~ rep(identifier ~ opt("=" ~ intConstant) ~ opt(listSeparator)) ~ "}"
  def senum           = "senum" ~ identifier ~ "{" ~ rep(literal ~ opt(listSeparator)) ~ "}"
  def struct          = "struct" ~ identifier ~ opt("xsd_all") ~ "{" ~> rep(field) <~ "}"
  def exception       = "exception" ~ identifier ~ "{" ~ rep(field) ~ "}"
  def service         = "service" ~ identifier ~ opt("extends" ~ identifier) ~ functionList
  def functionList    = "{" ~ rep(function) ~ "}"
  def field: Parser[Any] =
    opt(fieldID) ~ opt(fieldReq) ~ fieldType ~ identifier ~ opt("=" ~ constValue) ~ xsdFieldOptions ~ opt(listSeparator)
  def fieldID         = intConstant ~ ":"
  def fieldReq        = "required" | "optional"
  def xsdFieldOptions = opt("xsd_optional") ~ opt("xsd_nillable") ~ opt(xsdAttrs)
  def xsdAttrs        = "xsd_attrs" ~ "{" ~ rep(field) ~ "}"
  def function        = opt("async") ~ functionType ~ identifier ~ "(" ~ rep(field) ~ ")" ~ opt(throws) ~ opt(listSeparator)
  def functionType    = fieldType | "void"
  def throws          = "throws" ~ "(" ~ rep(field) ~ ")"
  def fieldType: Parser[FieldType] =
    (identifier | baseType | containerType) ^^ {
      case tpe: FieldType => tpe
      case Identifier(n) => ReferenceType(n)
    }
  def definitionType: Parser[DefinitionType] =
    baseType | containerType
  def baseType: Parser[BaseType] =
    accept("base type", {
      case lexical.Keyword(n) if BaseType.map.contains(n) => BaseType.map(n)
    })
  def containerType: Parser[ContainerType] =
    mapType | setType | listType
  def mapType: Parser[MapType] =
    ("map" ~> opt(cppType) ~ keyType ~ valueType) ^^ {
      case cpp ~ ktpe ~ vtpe => MapType(ktpe, vtpe, cpp)
    }
  def keyType: Parser[FieldType] =
    "<" ~> fieldType <~ ","
  def valueType: Parser[FieldType] =
    fieldType <~ ">"
  def setType: Parser[SetType] =
    "set" ~> opt(cppType) ~ ("<" ~> fieldType <~ ">") ^^ {
      case cpp ~ tpe => SetType(tpe, cpp)
    }
  def listType: Parser[ListType] =
    "list" ~> ("<" ~> fieldType <~ ">") ~ opt(cppType) ^^ {
      case tpe ~ cpp => ListType(tpe, cpp)
    }
  def cppType: Parser[String] =
    "cpp_type" ~> literal ^^ (_.string)
  def constValue: Parser[ConstValue] =
    intConstant | doubleConstant | literal | identifier | constList | constMap
  def constList: Parser[ConstList] =
    "[" ~> rep(constValue <~ opt(listSeparator)) <~ "]" ^^ (ConstList.apply _ )
  def constMap: Parser[ConstMap] =
    "{" ~> rep((constValue <~ ":") ~ constValue <~ opt(listSeparator)) <~ "}" ^^ (x =>
      ConstMap(Map.empty ++ x.map {
        case key ~ value => (key, value)
      }))
  def listSeparator   = "," | ";"
  def intConstant: Parser[IntConstant] =
    accept("int constant", {
      case lexical.NumericLit(n) if !n.contains(".") && !n.contains("e") &&
                                    !n.contains("E") && n.exists(_.isDigit) => IntConstant(n)
    })
  def doubleConstant: Parser[DoubleConstant] =
    accept("double constant", {
      case lexical.NumericLit(n) => DoubleConstant(n)
    })
  def literal: Parser[StringLiteral] =
    accept("string literal", {
      case lexical.StringLit(s) => StringLiteral(s)
    })
  def identifier: Parser[Identifier] =
    accept("identifier", {
      case lexical.Identifier(s) if !s.contains("-") => Identifier(s)
    })
  def stIdentifier: Parser[Identifier] =
    accept("smalltalk identifier", { 
      case lexical.Identifier(s) => Identifier(s)
    })
}

