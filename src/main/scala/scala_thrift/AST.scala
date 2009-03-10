package scala_thrift

case class Document(headers: List[Header], defs: List[Definition])

abstract class Header
case class Include(file: String) extends Header
case class CppInclude(file: String) extends Header
case class Namespace(scope: String, name: String) extends Header

abstract case class Definition(name: String)
case class Const(name: String, tpe: FieldType, value: ConstValue) extends Definition(name)
case class Typedef(name: String, tpe: DefinitionType) extends Definition(name)
case class Enum(name: String, values: List[EnumValue]) extends Definition(name)
case class EnumValue(name: String, value: Int)
case class Senum(name: String, values: List[String]) extends Definition(name)
case class Struct(name: String, fields: List[Field]) extends Definition(name) {
  def xsdAll = false
}
case class Exception(name: String, fields: List[Field]) extends Definition(name)
case class Service(name: String, parent: Option[String], functions: List[Function]) extends Definition(name)

case class Field(id: Int, name: String, tpe: FieldType) {
  def defaultValue: Option[ConstValue] = None
  def isRequired = false
  def isOptional = false
  def xsdOptional = false
  def xsdNillable = false
  def xsdAttrs: List[Field] = Nil
  
  assert(!(isRequired && isOptional))
}

case class Function(name: String, tpe: FunctionType, args: List[Field]) {
  def isAsync = false
  def throws: List[Field] = Nil
}

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
abstract class ContainerType extends DefinitionType {
  def cppType: Option[String] = None
}
case class MapType(keyType: FieldType, valueType: FieldType) extends ContainerType
case class SetType(tpe: FieldType) extends ContainerType
case class ListType(tpe: FieldType) extends ContainerType
case class ReferenceType(name: String) extends FieldType

abstract class ConstValue
case class IntConstant(s: String) extends ConstValue
case class DoubleConstant(s: String) extends ConstValue
case class ConstList(elems: List[ConstValue]) extends ConstValue
case class ConstMap(elems: Map[ConstValue, ConstValue]) extends ConstValue
case class StringLiteral(s: String) extends ConstValue
case class Identifier(name: String) extends ConstValue