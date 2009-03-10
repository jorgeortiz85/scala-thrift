package scala_thrift

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

class Parser extends StdTokenParsers with ImplicitConversions {
  type Tokens = Lexer
  val lexical = new Tokens

  val baseTypes = List("bool", "byte", "i16", "i32", "i64", "double", "string", "binary", "slist")
  lexical.reserved ++= List("include", "cpp_include", "namespace", "smalltalk.category",
    "smalltalk.prefix", "php_namespace", "xsd_namespace", "cpp", "java", "py", "perl", "rb",
    "cocoa", "csharp", "const", "typedef", "enum", "senum", "struct", "xsd_all", "exception", "service",
    "extends", "required", "optional", "xsd_optional", "xsd_nillable", "xsd_attrs", "async",
    "void", "throws", "map", "set", "list", "cpp_type") ++ baseTypes
  lexical.delimiters ++= List("{", "}", "[", "]", "<", ">", "(", ")", ",", ":", ";", "=")

  def Document        = rep(Header) ~ rep(Definition)
  def Header          = Include | CppInclude | Namespace
  def Include         = "include" ~ Literal
  def CppInclude      = "cpp_include" ~ Literal
  def Namespace       = ("namespace" ~ ((NamespaceScope ~ Identifier) |
                                        ("smalltalk.category" ~ STIdentifier) |
                                        ("smalltalk.prefix" ~ Identifier))) |
                        ("php_namespace" ~ Literal) |
                        ("xsd_namespace" ~ Literal)
  def NamespaceScope  = "*" | "cpp" | "java" | "py" | "perl" | "rb" | "cocoa" | "csharp"
  def Definition      = Typedef
  // def Definition      = Const | Typedef | Enum | Senum | Struct | Exception | Service
  // def Const           = "const" ~ FieldType ~ Identifier ~ "=" ~ ConstValue ~ opt(ListSeparator)
  def Typedef         = "typedef" ~ DefinitionType ~ Identifier
  def Enum            = "enum" ~ Identifier ~ "{" ~ repsep(Identifier ~ opt("=" ~ IntConstant), ListSeparator) ~ "}"
  // def Senum           = "senum" ~ Identifier ~ SenumDecl
  // def SenumDecl       = "{" ~ repsep(Literal, ListSeparator) ~ "}"
  // def Struct          = "struct" ~ Identifier ~ "{" ~> repsep(UField, ListSeparator) <~ "}"
  // def Exception       = "exception" ~ Identifier ~ "{" ~ rep(Field) ~ "}"
  // def Service         = "service" ~ Identifier ~ opt("extends" ~ Identifier) ~ FunctionList
  // def FunctionList    = "{" ~ repsep(Function, ListSeparator) ~ "}"
  // def UField          = opt(FieldID) ~ opt(FieldReq) ~ FieldType ~ Identifier ~ opt("=" ~ ConstValue) ~ XsdFieldOptions
  // def Field: Parser[Any]           = UField ~ opt(ListSeparator)
  // def FieldID         = IntConstant ~ ":"
  // def FieldReq        = "required" | "optional"
  // def XsdFieldOptions = opt("xsd_optional") ~ opt("xsd_nillable") ~ opt(XsdAttrs)
  // def XsdAttrs        = "xsd_attrs" ~ "{" ~ rep(Field) ~ "}"
  // def Function        = opt("async") ~ FunctionType ~ Identifier ~ "(" ~ rep(Field) ~ ")" ~ opt(Throws) ~ opt(ListSeparator)
  // def FunctionType    = FieldType | "void"
  // def Throws          = "throws" ~ "(" ~ rep(Field) ~ ")"
  def FieldType: Parser[Any]       = Identifier  | BaseType | ContainerType
  def DefinitionType  = BaseType | ContainerType
  def BaseType        = accept("base type", { case lexical.Keyword(n) if baseTypes.contains(n) => n })
  def ContainerType   = MapType | SetType | ListType
  def MapType         = "map" ~ opt(CppType) ~ "<" ~ FieldType ~ "," ~ FieldType ~ ">"
  def SetType         = "set" ~ opt(CppType) ~ "<" ~ FieldType ~ ">"
  def ListType        = "list" ~ "<" ~ FieldType ~ ">" ~ opt(CppType)
  def CppType         = "cpp_type" ~ Literal
  def ConstValue: Parser[Any]      = IntConstant | DoubleConstant | Literal | Identifier | ConstList // | ConstMap
  def ConstList       = "[" ~ repsep(ConstValue, ListSeparator) ~ "]"
  def ConstMap        = "{" ~ repsep(ConstValue ~ ":" ~ ConstValue, ListSeparator) ~ "}"
  def ListSeparator   = "," | ";"
  def IntConstant     = accept("int constant", {
    case lexical.NumericLit(n) if !n.contains(".") && !n.contains("e") &&
                                  !n.contains("E") && n.exists(_.isDigit) => n
  })
  def DoubleConstant  = accept("double constant", { case lexical.NumericLit(n) => n })
  def Literal         = accept("string literal", { case lexical.StringLit(s) => s })
  def Identifier      = accept("identifier", { case lexical.Identifier(s) if !s.contains("-") => s })
  def STIdentifier    = accept("smalltalk identifier", { case lexical.Identifier(s) => s })
}

