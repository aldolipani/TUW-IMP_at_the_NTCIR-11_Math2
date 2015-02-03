package at.ac.tuwien.utility

import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}

import at.ac.tuwien.math.TreeTokenizer
import net.sourceforge.jeuclid.context.{LayoutContextImpl, Parameter}
import net.sourceforge.jeuclid.converter.Converter
import net.sourceforge.jeuclid.{MathMLParserSupport, MutableLayoutContext}

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node}

/**
 * Created by aldo on 6/11/14.
 */
object Output {

  def readTemplate(file: File): String = scala.io.Source.fromFile(file).mkString

  def writeOutput(file: File, str: String) = {
    val p = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8"));
    p.write(str)
    p.close()
  }

  def convertMathML2PNG(id: String, mathML: String) = {
    val file: File = new File("results/images", id + ".png")
    if (!file.exists()) {
      val doc = MathMLParserSupport.parseString(mathML)

      val params: MutableLayoutContext = new LayoutContextImpl(
        LayoutContextImpl.getDefaultLayoutContext)
      params.setParameter(Parameter.MATHSIZE, 25f)
      try {
        Converter.getInstance.convert(doc, file, "image/png", params)
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }
  }


  def printMathTemplate4(out: File, title: String, query: (List[Node], String), tr1: String, r1: List[(String, scala.xml.Node)], tr2: String, r2: List[(String, scala.xml.Node)], tr3: String, r3: List[(String, scala.xml.Node)], tr4: String, r4: List[(String, List[(String, Float, String)])]) {
    val template = readTemplate(Resources.MATH_TEMPLATE)

    val esR1 = r1.filter(_ != null).map(e => {
      val pngName = (e._2 \ "@id").text
      convertMathML2PNG(pngName, e._2.toString)
      val img = <img src={"images/" + pngName + ".png"}></img>
      MathTemplate.ITEM.replace(MathTemplate.VOID, e._1 + "</br>" + img.toString)
    }).mkString
    val liR1 = MathTemplate.LIST.replace(MathTemplate.VOID, esR1)
    val esR2 = r2.map(e => {
      val pngName = (e._2 \ "@id").text
      convertMathML2PNG(pngName, e._2.toString)
      val img = <img src={"images/" + pngName + ".png"}></img>
      MathTemplate.ITEM.replace(MathTemplate.VOID, e._1 + "</br>" + img.toString)
    }).mkString
    val liR2 = MathTemplate.LIST.replace(MathTemplate.VOID, esR2)
    val esR3 = r3.map(e => {
      val pngName = (e._2 \ "@id").text
      convertMathML2PNG(pngName, e._2.toString)
      val img = <img src={"images/" + pngName + ".png"}></img>
      MathTemplate.ITEM.replace(MathTemplate.VOID, e._1 + "</br>" + img.toString)
    }).mkString
    val liR3 = MathTemplate.LIST.replace(MathTemplate.VOID, esR3)
    val esR4 = r4.map(e => {
      val pngs = (for (p <- e._2) yield {
        val xml = scala.xml.XML.loadString(p._3)
        val pngName = (xml \ "@id").text
        convertMathML2PNG(pngName, p._3)
        p._1 + "</br>" + p._2 + "</br>" + (<img src={"images/" + pngName + ".png"}></img>).toString
      }).mkString("</br>")
      MathTemplate.ITEM.replace(MathTemplate.VOID, e._1 + "</br>" + pngs)
    }).mkString
    val liR4 = MathTemplate.LIST.replace(MathTemplate.VOID, esR4)

    val res = template
      .replace(MathTemplate.TITLE, title)
      .replace(MathTemplate.QUERY, {
      //val pngName = out.getName
      //convertMathML2PNG(pngName, query._1.toString)
      //val img = <img src={"images/"+pngName+".png"}></img>
      val queryStr = query._1.map(_.toString()).mkString("-")
      MathTemplate.ITEM.replace(MathTemplate.VOID, queryStr + " keywords:" + query._2)
    })
      .replace(MathTemplate.TR1, tr1)
      .replace(MathTemplate.R1, liR1)
      .replace(MathTemplate.TR2, tr2)
      .replace(MathTemplate.R2, liR2)
      .replace(MathTemplate.TR3, tr3)
      .replace(MathTemplate.R3, liR3)
      .replace(MathTemplate.TR4, tr4)
      .replace(MathTemplate.R4, liR4)
    writeOutput(out, res)
  }

  def printXMLTextResult(out: File, label: String, query: (String, String, List[Node]), results: List[(String, Float, Node)]) = {
    val res =
      <result>
        <name>
          {label}
        </name>
        <query>
          <id>
            {query._1}
          </id>
          <content>
            {query._2}
          </content>
          <original>
            {for (i <- 0 until query._3.size) yield {
            <elem>
              {query._3(i)}
            </elem>
          }}
          </original>
        </query>
        <list>
          {for (result <- results) yield {
          <document>
            <id>
              {result._1}
            </id>
            <score>
              {result._2}
            </score>
            <original>
              {result._3}
            </original>
          </document>
        }}
        </list>
      </result>

    writeOutput(out, res.toString)
  }


  def printXMLFormulaResult(out: File, label: String, query: (String, String, List[Node]), results: List[(String, Float, Node)]) = {

    object MathmaticaRulesCSymbolTransform extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case <csymbol>
          {v}
          </csymbol> => {
          /*println(v.text);*/ <csymbol>
            {v.text.replaceAllLiterally("-", "\\[Dash]")}
          </csymbol>
        }
        case other => other
      }
    }

    object MathmaticaRulesCSymbol extends RuleTransformer(MathmaticaRulesCSymbolTransform)

    object MathmaticaRulesTransform extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case sn@Elem(_, _, _, _, _*) => MathmaticaRulesCSymbol(sn)
        case other => other
      }
    }

    object MathmaticaRules extends RuleTransformer(MathmaticaRulesTransform)

    def replacingValuesForMathematica(xml: Node): Node = {
      (MathmaticaRules transform xml).head
    }
    val res =
      <result>
        <name>
          {label}
        </name>
        <query>
          <id>
            {query._1}
          </id>
          <content>
            {query._2}
          </content>
          <original>
            {for (i <- 0 until query._3.size) yield {
            <elem>
              {query._3(i)}
            </elem>
          }}
          </original>
        </query>
        <list>
          {for (result <- results) yield {
          <document>
            <id>
              {result._1}
            </id>
            <score>
              {result._2}
            </score>
            <original>
              {result._3}
            </original>
            <subforumulas>
              {val annotationSeqXML = (result._3 \\ "annotation-xml")
            if (annotationSeqXML.nonEmpty) {
              val annotationXML = annotationSeqXML.head
              //println(annotationXML)
              val subformulas = TreeTokenizer.formulaTokenizerSubgroup(annotationXML).drop(1)
              //println(subformulas.size, subformulas)
              for (subformula <- subformulas) yield {
                <subformula>
                  <math id={(result._3 \ "@id").text} class="ltx_Math" xmlns="http://www.w3.org/1998/Math/MathML">
                    <semantics>
                      <annotation-xml>
                        {replacingValuesForMathematica(subformula)}
                      </annotation-xml>
                    </semantics>
                  </math>
                </subformula>
              }
            }}
            </subforumulas>
          </document>
        }}
        </list>
      </result>

    writeOutput(out, res.toString)
  }

  def printMath2NTCIR(groupID: String, runID: String, results: Map[String, List[(String, Float)]]) = {
    val formatter = new java.text.DecimalFormat("#.##########")
    val runTag = groupID + "_" + runID
    val out = new File("results/", runTag)
    //println(results.keys.toList)
    val sortedResults = results.keys.toList.sortBy(_.split("-")(2).toFloat)
    val res =
      (for (topic <- sortedResults) yield {
        val sortedList = results.get(topic).get.sortBy(-_._2)
        (for (i <- 0 until Math.min(sortedList.length, 1000)) yield {
          s"$topic 1 ${sortedList(i)._1} ${i + 1} ${formatter.format(sortedList(i)._2)} $runTag"
        }).mkString("\n")
      }).mkString("\n") + "\n"

    writeOutput(out, res)
  }

  def printMath2NTCIRWiki(groupID: String, runID: String, results: Map[String, List[(String, Float)]]) = {
    val formatter = new java.text.DecimalFormat("#.##########")
    val runTag = groupID + "_" + runID
    val out = new File("results/", runTag)
    val sortedResults = if (results.keys.head.contains("-"))
      results.keys.toList.sortBy(_.split("-")(2).toFloat)
    else
      results.keys.toList.sortBy(_.toFloat)

    val res =
      (for (topic <- sortedResults) yield {
        val sortedList = results.get(topic).get.sortBy(-_._2)
        (for (i <- 0 until Math.min(sortedList.length, 1000)) yield {
          s"$topic,${sortedList(i)._1}"
        }).mkString("\n")
      }).mkString("\n") + "\n"

    writeOutput(out, "queryId,formulaId\n" + res)
  }

  def printMathQRelsTemplate(out: File, title: String, r: List[(String, Int, List[(String, String)])]) {
    val template = readTemplate(Resources.MATH_QRELS_TEMPLATE)
    val esR = r.map(e => {
      val pngs = if (e._3 != null)
        (for (p <- e._3) yield {
          try {
            val xml = scala.xml.XML.loadString(p._2)
            val pngName = (xml \ "@id").text

            convertMathML2PNG(pngName, p._2)
            "<tr><td>" + p._1 + "</td><td>" + (<img src={"images/" + pngName + ".png"}></img>).toString + "</td></tr>"
          } catch {
            case e: Exception => "<tr><td>" + p._1 + "</td><td>" + e.getMessage + "</td></tr>"
          }
        }).mkString
      else null

      val tablePngs =
        if (e._3 != null) {
          MathTemplate.TABLE.replace(MathTemplate.VOID, pngs.mkString)
        } else {
          "Document doesn't exist!"
        }
      MathTemplate.COL.replace(
        MathTemplate.VOID,
        MathTemplate.PANEL
          .replace(MathTemplate.TITLE, e._1)
          .replace(MathTemplate.CONTENT, tablePngs)
          .replace(MathTemplate.STYLE_HEADING, "background-color: " + (if (e._2 == 0) MathTemplate.RED_GRADING(0) else MathTemplate.GREEN_GRADING(e._2 - 1)) + "!important"))
    })

    val liR = esR.grouped(2).map(l => MathTemplate.ROW.replace(MathTemplate.VOID, l.mkString)).mkString

    val res = template
      .replace(MathTemplate.TITLE, title)
      .replace(MathTemplate.R, liR)
    writeOutput(out, res)
  }
}

