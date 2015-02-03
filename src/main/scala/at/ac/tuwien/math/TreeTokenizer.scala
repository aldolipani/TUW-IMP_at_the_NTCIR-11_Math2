package at.ac.tuwien.math

/**
 * Created by aldo on 5/21/14.
 */

object TreeTokenizer {

  //lazy val mathML:scala.xml.Node = scala.xml.XML.loadString("""<apply><plus/><apply><times/><ci>a</ci><apply><power/><ci>x</ci><cn>2</cn></apply></apply><apply><times/><ci>b</ci><ci>x</ci></apply><ci>c</ci></apply>""")
  //*********************************************************************************************
  // Tokenization

  def cleanXMLString(str: String): String =
    "([a-z]+\\:)?[a-zA-Z-]+=\"(?:(?!\").)+\"".r.replaceAllIn(str, "").replaceAll("[\\n ]", "")

  case class Node(name: String, value: String, children: List[Node], xml: scala.xml.Node)

  // convert XML into a tree
  def fromXML2Tree(xml: scala.xml.Node): Node = {
    if (xml.child.isEmpty || xml.child.head.label == "#PCDATA")
      new Node(xml.label, xml.text, Nil, xml)
    else
      new Node(xml.label, "", xml.child.map(fromXML2Tree(_)).toList, xml)
  }


  // <annotation-xml><apply><geq/><ci>k</ci><cn>1</cn></apply></annotation-xml>
  // List(apply, geq-ci-cn)
  def tokenize1(tree: Node): List[String] =
    List(tree.children.map(_.name).mkString("-")) ::: tree.children.filter(_.children.nonEmpty).map(tokenize1(_)).flatten

  def tokenizeSubgroup(tree: Node): List[scala.xml.Node] =
    List(tree.xml) ::: tree.children.filter(e => e.name == "apply" /*|| e.name == "ci" || e.name == "cn"*/).map(tokenizeSubgroup(_)).flatten

  /*
  eq-(ci-apply)-(times-apply-ci-apply)
  ci-(csymbol-ci-ci),
             times-(root-apply)-ci-(csymbol-apply-apply),
                    root-(times-cn-ci),
                                    csymbol-(plus-cn-apply)-(divide-cn-ci),
                                             plus-cn-(divide-ci-cn)
                                             */
  def tokenize12(tree: Node): List[String] = {
    (if (tree.children.size > 1 && !tree.children.forall(_.children.isEmpty)) {
      List(tree.children.map(c => {
        if (c.children.nonEmpty)
          "(" + c.children.map(_.name).mkString("-") + ")"
        else
          c.name
      }).mkString("-"))
    } else Nil) ::: tree.children.filter(_.children.nonEmpty).map(tokenize12(_)).flatten
  }

  // <annotation-xml><apply><geq/><ci>k</ci><cn>1</cn></apply></annotation-xml>
  // List(apply, geq-"k"-"1")
  def tokenize2(tree: Node): List[String] = {
    List(tree.children.map(e => if (e.value.nonEmpty) "\"" + e.value + "\"" else e.name).mkString("-")) ::: tree.children.filter(_.children.nonEmpty).map(tokenize2(_)).flatten
  }

  /*
  eq-("normal-~"-apply)-(times-apply-"normal-Γ"-apply)
      "normal-~"-("subscript"-"C"-"d")
                         times-(root-apply)-"normal-Γ"-("superscript"-apply-apply)
                                root-(times-"4"-"π"),
                                                        "superscript"-(plus-"1"-apply)-(divide-"1"-"d"),
                                                                       plus-"1"-(divide-"d"-"2")*/
  def tokenize22(tree: Node): List[String] = {
    (if (tree.children.size > 1 && !tree.children.forall(_.children.isEmpty)) {
      List(tree.children.map(c => {
        if (c.children.nonEmpty)
          "(" + c.children.map(e => if (e.value.nonEmpty) "\"" + e.value + "\"" else e.name).mkString("-") + ")"
        else {
          if (c.value.nonEmpty) "\"" + c.value + "\"" else c.name
        }
      }).mkString("-"))
    } else Nil) ::: tree.children.filter(_.children.nonEmpty).map(tokenize22(_)).flatten
    //List(tree.children.filter(_.children.nonEmpty).map(c =>
    //  "("+ c.children.map(_.name).mkString("-") + ")").mkString("-")).filterNot(_.isEmpty) :::
    //  tree.children.flatMap(_.children).filter(c => c.children.nonEmpty).map(tokenize12(_)).flatten
  }

  // <annotation-xml><apply><geq/><ci>k</ci><cn>1</cn></apply></annotation-xml>
  // List("k", "1")
  def tokenizeLiteral(tree: Node): List[String] = {
    tree.children.filter(_.value.nonEmpty).map("\"" + _.value + "\"") ::: tree.children.filter(_.children.nonEmpty).map(tokenizeLiteral(_)).flatten
  }

  def formulaTokenizerL2(xml: scala.xml.Node): List[String] = {
    val tree: Node = fromXML2Tree(xml)
    tokenize12(tree) ::: tokenize22(tree) // ::: tokenize2(tree) ::: tokenizeLiteral(tree)
  }


  def formulaTokenizerL1(xml: scala.xml.Node): List[String] = {
    val tree: Node = fromXML2Tree(xml)
    tokenize1(tree) ::: tokenize2(tree)
  }

  def formulaTokenizerLiteral(xml: scala.xml.Node): List[String] = {
    val tree: Node = fromXML2Tree(xml)
    tokenizeLiteral(tree)
  }

  def formulaTokenizerSubgroup(xml: scala.xml.Node): List[scala.xml.Node] = {
    val tree: Node = fromXML2Tree(xml)
    tokenizeSubgroup(tree)
  }

  //**********************************************************************************************

  def toVector(tokens: List[String]): Map[String, Int] = tokens.groupBy(_.toString).mapValues(_.size)
}
