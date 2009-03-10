package scala_thrift

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._

class Parser extends StdTokenParsers with ImplicitConversions {
  type Tokens = Lexer
  val lexical = new Tokens

  val namespaceScopes = List("*", "cpp", "java", "py", "perl", "rb", "cocoa", "cshapr", "php")
  val baseTypes = List("bool", "byte", "i16", "i32", "i64", "double", "string", "binary", "slist")
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
    "union", "yield") ++ baseTypes
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
  def field: Parser[Any] = opt(fieldID) ~ opt(fieldReq) ~ fieldType ~ identifier ~ opt("=" ~ constValue) ~ xsdFieldOptions ~ opt(listSeparator)
  def fieldID         = intConstant ~ ":"
  def fieldReq        = "required" | "optional"
  def xsdFieldOptions = opt("xsd_optional") ~ opt("xsd_nillable") ~ opt(xsdAttrs)
  def xsdAttrs        = "xsd_attrs" ~ "{" ~ rep(field) ~ "}"
  def function        = opt("async") ~ functionType ~ identifier ~ "(" ~ rep(field) ~ ")" ~ opt(throws) ~ opt(listSeparator)
  def functionType    = fieldType | "void"
  def throws          = "throws" ~ "(" ~ rep(field) ~ ")"
  def fieldType: Parser[Any]       = identifier  | baseType | containerType
  def definitionType  = baseType | containerType
  def baseType        = accept("base type", { case lexical.Keyword(n) if baseTypes.contains(n) => n })
  def containerType   = mapType | setType | listType
  def mapType         = "map" ~ opt(cppType) ~ "<" ~ fieldType ~ "," ~ fieldType ~ ">"
  def setType         = "set" ~ opt(cppType) ~ "<" ~ fieldType ~ ">"
  def listType        = "list" ~ "<" ~ fieldType ~ ">" ~ opt(cppType)
  def cppType         = "cpp_type" ~ literal
  def constValue: Parser[Any]      = intConstant | doubleConstant | literal | identifier | constList | constMap
  def constList       = "[" ~ rep(constValue ~ opt(listSeparator)) ~ "]"
  def constMap        = "{" ~ rep(constValue ~ ":" ~ constValue ~ opt(listSeparator)) ~ "}"
  def listSeparator   = "," | ";"
  def intConstant     = accept("int constant", {
    case lexical.NumericLit(n) if !n.contains(".") && !n.contains("e") &&
                                  !n.contains("E") && n.exists(_.isDigit) => n
  })
  def doubleConstant  = accept("double constant", { case lexical.NumericLit(n) => n })
  def literal         = accept("string literal", { case lexical.StringLit(s) => s })
  def identifier      = accept("identifier", { case lexical.Identifier(s) if !s.contains("-") => s })
  def stIdentifier    = accept("smalltalk identifier", { case lexical.Identifier(s) => s })
}

