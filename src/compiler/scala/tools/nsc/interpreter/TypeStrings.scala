/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import java.lang.{ reflect => r }
import scala.reflect.NameTransformer
import typechecker.DestructureTypes


/** A more principled system for turning types into strings.
 */
trait StructuredTypeStrings extends DestructureTypes {
  val global: Global
  import global._

  case class LabelAndType(label: String, typeName: String) { }
  object LabelAndType {
    val empty = LabelAndType("", "")
  }
  case class Grouping(ldelim: String, mdelim: String, rdelim: String, labels: Boolean) {
    def join(elems: String*): String = (
      if (elems.isEmpty) ""
      else elems.mkString(ldelim, mdelim, rdelim)
    )
  }
  val NoGrouping      = Grouping("", "", "", false)
  val ListGrouping    = Grouping("(", ", ", ")", false)
  val ProductGrouping = Grouping("(", ", ", ")", true)
  val ParamGrouping   = Grouping("(", ", ", ")", true)
  val BlockGrouping   = Grouping(" { ", "; ", "}", false)

  private implicit def lowerName(n: Name): String = "" + n
  private def str(level: Int)(body: => String): String = "  " * level + body
  private def block(level: Int, grouping: Grouping)(name: String, nodes: List[TypeNode]): String = {
    val l1 = str(level)(name + grouping.ldelim)
    val l2 = nodes.map(_ show level + 1)
    val l3 = str(level)(grouping.rdelim)

    l1 +: l2 :+ l3 mkString "\n"
  }
  private def maybeBlock(level: Int, grouping: Grouping)(name: String, nodes: List[TypeNode]): String = {
    val threshold = 70

    val try1 = str(level)(name + grouping.join(nodes map (_.show(0, grouping.labels)): _*))
    if (try1.length < threshold) try1
    else block(level, grouping)(name, nodes)
  }
  private def shortClass(x: Any) = {
    if (opt.debug) {
      val name   = (x.getClass.getName split '.').last
      val isAnon = name.reverse takeWhile (_ != '$') forall (_.isDigit)
      val str    = if (isAnon) name else (name split '$').last

      " // " + str
    }
    else ""
  }

  sealed abstract class TypeNode {
    def grouping: Grouping
    def nodes: List[TypeNode]

    def show(indent: Int, showLabel: Boolean): String = maybeBlock(indent, grouping)(mkPrefix(showLabel), nodes)
    def show(indent: Int): String = show(indent, true)
    def show(): String = show(0)

    def withLabel(l: String): this.type = modifyNameInfo(_.copy(label = l))
    def withType(t: String): this.type  = modifyNameInfo(_.copy(typeName = t))

    def label       = nameInfo.label
    def typeName    = nameInfo.typeName

    protected def mkPrefix(showLabel: Boolean) = {
      val pre = if (showLabel && label != "") label + " = " else ""
      pre + typeName
    }
    override def toString = show() // + "(toString)"
    private var nameInfo: LabelAndType = LabelAndType.empty
    private def modifyNameInfo(f: LabelAndType => LabelAndType): this.type = {
      nameInfo = f(nameInfo)
      this
    }
  }
  case class TypeAtom[T](atom: T) extends TypeNode {
    def grouping = NoGrouping
    def nodes = Nil
    override protected def mkPrefix(showLabel: Boolean) =
      super.mkPrefix(showLabel) + atom + shortClass(atom)
  }
  case class TypeProduct(nodes: List[TypeNode]) extends TypeNode {
    def grouping: Grouping = ProductGrouping
    def emptyTypeName = ""
    override def typeName = if (nodes.isEmpty) emptyTypeName else super.typeName
  }

  /** For a NullaryMethod, in = TypeEmpty; for MethodType(Nil, _) in = TypeNil */
  class NullaryFunction(out: TypeNode) extends TypeProduct(List(out)) {
    override def typeName = "NullaryMethodType"
  }
  class MonoFunction(in: TypeNode, out: TypeNode) extends TypeProduct(List(in, out)) {
    override def typeName = "MethodType"
  }
  class PolyFunction(in: TypeNode, out: TypeNode) extends TypeProduct(List(in, out)) {
    override def typeName = "PolyType"
  }

  class TypeList(nodes: List[TypeNode]) extends TypeProduct(nodes) {
    override def grouping = ListGrouping
    override def emptyTypeName = "Nil"
    override def typeName = "List"
  }
  class TypeScope(nodes: List[TypeNode]) extends TypeProduct(nodes) {
    override def grouping = BlockGrouping
    override def typeName = "Scope"
    override def emptyTypeName = "EmptyScope"
  }

  object TypeEmpty extends TypeNode {
    override def grouping = NoGrouping
    override def nodes = Nil
    override def label = ""
    override def typeName = ""
    override def show(indent: Int, showLabel: Boolean) = ""
  }

  object intoNodes extends DestructureType[TypeNode] {
    def withLabel(node: TypeNode, label: String): TypeNode   = node withLabel label
    def withType(node: TypeNode, typeName: String): TypeNode = node withType typeName

    def wrapEmpty                             = TypeEmpty
    def wrapSequence(nodes: List[TypeNode])   = new TypeList(nodes)
    def wrapProduct(nodes: List[TypeNode])    = new TypeProduct(nodes)
    def wrapPoly(in: TypeNode, out: TypeNode) = new PolyFunction(in, out)
    def wrapMono(in: TypeNode, out: TypeNode) = if (in == wrapEmpty) new NullaryFunction(out) else new MonoFunction(in, out)
    def wrapAtom[U](value: U)                 = new TypeAtom(value)
  }

  def show(tp: Type): String = intoNodes(tp).show
}

