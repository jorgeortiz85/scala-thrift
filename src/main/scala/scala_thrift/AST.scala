package scala_thrift

case class Document(headers: List[Header], defs: List[Definition])

abstract class Header
case class Include(file: String) extends Header
case class CppInclude(file: String) extends Header
case class Namespace(scope: String, name: String) extends Header

abstract class Definition(name: String)
case class Const(name: String, tpe: FieldType, value: ConstValue) extends Definition(name)
case class Typedef(name: String, tpe: DefinitionType) extends Definition(name)
case class Enum(name: String, values: List[EnumValue]) extends Definition(name)
case class EnumValue(name: String, var value: Int)
case class Senum(name: String, values: List[String]) extends Definition(name)
case class Struct(name: String, fields: List[Field], xsdAll: Boolean) extends Definition(name)
case class Exception(name: String, fields: List[Field]) extends Definition(name)
case class Service(name: String, parent: Option[String], functions: List[Function]) extends Definition(name)

case class Field(var id: Int, name: String, tpe: FieldType, default: Option[ConstValue], required: Boolean, optional: Boolean) {
  def xsdOptional = false
  def xsdNillable = false
  def xsdAttrs: List[Field] = Nil

  assert(!(required && optional))
}

case class Function(name: String, tpe: FunctionType, args: List[Field], async: Boolean, throws: List[Field])

abstract class FunctionType
case object Void extends FunctionType
abstract class FieldType extends FunctionType
abstract class DefinitionType extends FieldType
abstract class BaseType extends DefinitionType
case object TBool extends BaseType
case object TByte extends BaseType
case object TI16 extends BaseType
case object TI32 extends BaseType
case object TI64 extends BaseType
case object TDouble extends BaseType
case object TString extends BaseType
case object TBinary extends BaseType
case object TSList extends BaseType
abstract class ContainerType(cppType: Option[String]) extends DefinitionType
case class MapType(keyType: FieldType, valueType: FieldType, cppType: Option[String]) extends ContainerType(cppType)
case class SetType(tpe: FieldType, cppType: Option[String]) extends ContainerType(cppType)
case class ListType(tpe: FieldType, cppType: Option[String]) extends ContainerType(cppType)
case class ReferenceType(name: String) extends FieldType

object BaseType {
  val map = Map(
    "bool" -> TBool
   ,"byte" -> TByte
   ,"i16" -> TI16
   ,"i32" -> TI32
   ,"i64" -> TI64
   ,"double" -> TDouble
   ,"string" -> TString
   ,"binary" -> TBinary
   ,"slist" -> TSList
  )
}

abstract class ConstValue
case class IntConstant(value: String) extends ConstValue
case class DoubleConstant(value: String) extends ConstValue
case class ConstList(elems: List[ConstValue]) extends ConstValue
case class ConstMap(elems: Map[ConstValue, ConstValue]) extends ConstValue
case class StringLiteral(string: String) extends ConstValue
case class Identifier(name: String) extends ConstValue