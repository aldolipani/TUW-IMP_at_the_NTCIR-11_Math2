package at.ac.tuwien.math

import java.io.File

import at.ac.tuwien.utility.{Output, Profile}
import org.apache.lucene.analysis.core.{KeywordAnalyzer, WhitespaceAnalyzer}
import org.apache.lucene.index._
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.queryparser.flexible.standard.QueryParserUtil
import org.apache.lucene.search._
import org.apache.lucene.search.similarities.{BM25Similarity, Similarity}
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version

import scala.xml.{Node, NodeSeq}

/**
 * Created by aldo on 5/21/14.
 */
object Search {

  type ID = String
  type Formulae = List[String]
  type Keywords = List[String]
  type Topic = (Formulae, Keywords)
  type Topics = Map[ID, Topic]

  class FormulaParser(val queries: Topics) {
    def topicIDs = queries.keys

    def size = queries.size

    def isWithKeywords = queries.values.forall(_._2.nonEmpty)

    def get(id: ID) = queries.get(id)
  }

  object FormulaParser {
    def parse(filePath: String) = {
      val xmlFile = scala.xml.XML.loadFile(filePath)

      val ids = (xmlFile
        \\ "num")
        .map(m => m.text).toList

      val formulae = (xmlFile
        \\ "topic")
        .map(t => (t \\ "math")
        .map(m => m.toString()).toList).toList

      val keywords = (xmlFile
        \\ "topic")
        .map(t => (t \\ "keyword")
        .map(m => m.text).toList).toList

      new FormulaParser(ids.zip(formulae.zip(keywords).map(fk => new Topic(fk._1, fk._2))).toMap)
    }
  }

  class Expansions(val topicKeywordExpansions: Map[String, Map[String, List[String]]]) {
    def size = topicKeywordExpansions.size

    def getAllExpansions(topicId: ID) = topicKeywordExpansions.get(topicId).get.values.flatten.map(e => e.split(",").map(_.trim)).flatten
  }

  object ExpansionParser {

    def parse(filePath: String) = {
      val xmlFile = scala.xml.XML.loadFile(filePath)

      def getExpansionMap(node: NodeSeq): Map[String, List[String]] = {
        (node \\ "expansion").map(el =>
          ((el \ "keyterm").text ->
            ((el \ "synonym").text.trim.split(",").union(
              (el \ "hypernym").text.trim.split(",")).union(
                (el \ "hyponym").text.trim.split(","))).toList)).toMap
      }

      val synset: Map[String, Map[String, List[String]]] =
        (xmlFile \\ "topic").map(t =>
          ((t \ "num").text -> getExpansionMap(t))).toMap

      new Expansions(synset)
    }
  }

  lazy val confRuns = Map(
    "FLA" -> Map(
      'SMWE -> BooleanClause.Occur.SHOULD,
      'SL -> BooleanClause.Occur.MUST,
      'SNL -> BooleanClause.Occur.SHOULD,
      'N -> false),
    "FLASM" -> Map(
      'SMWE -> BooleanClause.Occur.SHOULD,
      'SL -> BooleanClause.Occur.MUST,
      'SNL -> BooleanClause.Occur.MUST,
      'N -> false),
    "FLASL" -> Map(
      'SMWE -> BooleanClause.Occur.MUST,
      'SL -> BooleanClause.Occur.MUST,
      'SNL -> BooleanClause.Occur.SHOULD,
      'N -> false),
    "FLAN" -> Map(
      'SMWE -> BooleanClause.Occur.SHOULD,
      'SL -> BooleanClause.Occur.MUST,
      'SNL -> BooleanClause.Occur.SHOULD,
      'N -> true))

  def search(nameRun: String, topicsXML: String, formulaIndex: String, textIndex: String = null, expansionsXML: String = null): Unit = Profile.time {
    println(nameRun)
    val filterExt = "\\.[x]?html"

    val confRun =
      if (confRuns.contains(nameRun))
        confRuns.get(nameRun).get
      else
        confRuns.values.head

    // Parameters
    val booleanClauseLiteralsL1L2 = confRun.get('SL).get.asInstanceOf[BooleanClause.Occur]
    val booleanClauseNoNLiteralsL1L2 = confRun.get('SNL).get.asInstanceOf[BooleanClause.Occur]
    val booleanClauseText = confRun.get('SMWE).get.asInstanceOf[BooleanClause.Occur]
    val isNormalized = confRun.get('N).get.asInstanceOf[Boolean]

    println("Configuration--------------------------------------------------------------")
    println("SMWE:\t" + toString(booleanClauseText))
    println("SL:  \t" + toString(booleanClauseLiteralsL1L2))
    println("SNL: \t" + toString(booleanClauseNoNLiteralsL1L2))
    println("N:   \t" + toString(isNormalized))

    println("Files----------------------------------------------------------------------")
    println("Topics:       \t" + topicsXML)
    println("Formula Index:\t" + formulaIndex)
    println("Text Index:   \t" + textIndex)

    println("Parsing--------------------------------------------------------------------")
    val topics = FormulaParser.parse(topicsXML)
    println("Topics size:     \t" + topics.size)
    println("Keywords?:       \t" + toString(topics.isWithKeywords))
    val expansions = if (topics.isWithKeywords && expansionsXML != null) {
      val expansions = ExpansionParser.parse(expansionsXML)
      println("Expansions size: \t" + expansions.size)
      expansions
    } else null


    //    val xmlFormulaGroupped = xmlFormula.grouped(10).toList

    /* xmlFormulaGroupped.map(list => {
       val res = (0 until list.size).map(i => {
         println("TOPIC " + (xmlFormulaGroupped.indexOf(list) * 10 + (i + 1)))
         doSearch((xmlFormulaGroupped.indexOf(list) * 10 + i), xmlFormula((xmlFormulaGroupped.indexOf(list) * 10 + i))) //, keywords(i))
       }).toMap
       Output.printMath2NTCIRWiki("TUW-IMP", nameRUN + "_" + xmlFormulaGroupped.indexOf(list), res)
     }).toList*/


    def doSearch(id: ID, topic: Topic, isWithKeywords: Boolean, expansions: Expansions = null) = {
      println("TOPIC:\t" + id)

      val xmlFormulae: List[Node] = topic._1.map(formula => {
        val a = (<semantics>
          {(scala.xml.XML.loadString(formula) \\ "semantics")
            .head.child.filterNot(n => n.label == "#PCDATA" || n.label == "annotation-xml").head}
        </semantics>).toString()
        scala.xml.XML.loadString(TreeTokenizer.cleanXMLString(a))
      })
      val keywords = if (topics.isWithKeywords) topic._2 else null

      def generateQuery(tokenizer: (Node) => List[String], field: String, booleanClauseLiteralsL1L2: BooleanClause.Occur, booleanClauseNoNLiteralsL1L2: BooleanClause.Occur) = {
        var size = 0
        val bQ = new BooleanQuery
        // Map(eq-apply-apply -> 2, times-"i"-"d" -> 2, apply -> 4, times-ci-ci -> 4, neq-ci-apply -> 2)
        val tokens = xmlFormulae.map(tokenizer(_)).map(TreeTokenizer.toVector(_))

        tokens.map(m => {
          size += m.values.sum / tokens.size
          val bF = new BooleanQuery // Query for the single formula
          m.keys.filter(_.nonEmpty).map(t => {
            val bC = if (t.contains("\""))
              booleanClauseLiteralsL1L2
            else booleanClauseNoNLiteralsL1L2

            if (t.contains("qvar")) {
              val vQ = new BooleanQuery
              val tS = (0 until t.split("qvar").size).map(i => List("cn", "apply", "ci", "csymbol", "cerror")).flatten.combinations(("@" + t + "@").split("qvar").size - 1).toList
              val lt = for (ts <- tS) yield
                ts.foldLeft(t)((acc, tr) => acc.replaceFirst("qvar", tr))
              for (i <- (0 until m.get(t).get))
                (lt.map(nt => vQ.add(new TermQuery(new Term(field, QueryParserUtil.escape(nt))), BooleanClause.Occur.SHOULD))) // For all the combination of QVAR (MUST BE SHOULD)
              bF.add(vQ, bC)
            } else {
              for (i <- (0 until m.get(t).get))
                (bF.add(new TermQuery(new Term(field, QueryParserUtil.escape(t))), bC))
            }
          })
          bQ.add(bF, BooleanClause.Occur.SHOULD)
        })
        (bQ.toString(), size)
      }
      def generateQueryLiterals(tokenizer: (Node) => List[String], field: String, strict: Boolean) = {
        var size: Int = 0
        val bQ = new BooleanQuery
        val tokens = xmlFormulae.map(TreeTokenizer.formulaTokenizerLiteral(_)).map(TreeTokenizer.toVector(_)) // Map(eq-apply-apply -> 2, times-"i"-"d" -> 2, apply -> 4, times-ci-ci -> 4, neq-ci-apply -> 2)
        tokens.map(m => {
          size += m.values.sum / tokens.size
          val bF = new BooleanQuery
          m.keys.map(t => {
            for (i <- (0 until m.get(t).get))
              bF.add(new TermQuery(new Term(field, QueryParserUtil.escape(t))), {
                if (!strict || t.matches("\"\\d+\"")) BooleanClause.Occur.SHOULD else BooleanClause.Occur.MUST // to check
              })
          })
          if (m.keys.nonEmpty)
            bQ.add(bF, BooleanClause.Occur.SHOULD)
        })
        (bQ.toString(), size)
      }

      val queryLi = generateQueryLiterals(TreeTokenizer.formulaTokenizerLiteral, "contentLi", true)
      println("Li: " + queryLi._1 + "\n\tSize:\t" + queryLi._2)
      val queryL1 = generateQuery(TreeTokenizer.formulaTokenizerL1, "contentL1", booleanClauseLiteralsL1L2, booleanClauseNoNLiteralsL1L2)
      println("L1: " + queryL1._1 + "\n\tSize:\t" + queryL1._2)
      val queryL2 = generateQuery(TreeTokenizer.formulaTokenizerL2, "contentL2", booleanClauseLiteralsL1L2, booleanClauseNoNLiteralsL1L2)
      println("L2: " + queryL2._1 + "\n\tSize:\t" + queryL2._2)

      val querySLi = generateQueryLiterals(TreeTokenizer.formulaTokenizerLiteral, "contentLi", false)
      println("SLi: " + querySLi._1 + "\n\tSize:\t" + querySLi._2)
      val querySL1 = generateQuery(TreeTokenizer.formulaTokenizerL1, "contentL1", BooleanClause.Occur.SHOULD, BooleanClause.Occur.SHOULD)
      println("SL1: " + querySL1._1 + "\n\tSize:\t" + querySL1._2)
      val querySL2 = generateQuery(TreeTokenizer.formulaTokenizerL2, "contentL2", BooleanClause.Occur.SHOULD, BooleanClause.Occur.SHOULD)
      println("SL2: " + querySL2._1 + "\n\tSize:\t" + querySL2._2)

      var sizeTS = 0
      val queryTS = if (isWithKeywords) {
        val bQ = new BooleanQuery
        val tokens = keywords.map(QueryParserUtil.escape(_))
        tokens.map(t => {
          if (!t.contains(" "))
            bQ.add(new TermQuery(new Term("content", t)), BooleanClause.Occur.SHOULD) //should be must
          else {
            val ph = new PhraseQuery
            ph.setSlop(5)
            for (to <- t.split(" ")) (ph.add(new Term("content", to)))
            bQ.add(ph, booleanClauseText)
          }
        })

        if (expansions != null) {
          val ss = expansions.getAllExpansions(id).map(s => QueryParserUtil.escape(s.trim)) //.map(ks => if (ks.contains(" ")) "\"" + ks + "\"" else ks)
          val ssQ = new BooleanQuery
          ss.map(t => {
            if (t.nonEmpty)
              if (!t.contains(" "))
                ssQ.add(new TermQuery(new Term("content", t)), BooleanClause.Occur.SHOULD)
              else {
                val ph = new PhraseQuery
                ph.setSlop(5)
                for (to <- t.split(" ")) (ph.add(new Term("content", to)))
                ssQ.add(ph, BooleanClause.Occur.SHOULD)
              }
          })
          sizeTS = tokens.size + ss.size
          if (ss.size > 0)
            bQ.add(ssQ, BooleanClause.Occur.SHOULD) // synset add in or
        }

        val res = bQ.toString()
        println("TS: " + res + "\n\tSize:" + res.split(":").size)
        res
      } else ""

      val analyzerFormula = new KeywordAnalyzer
      val whiteSpaceAnalyzer = new WhitespaceAnalyzer(Version.LUCENE_46)
      val englishMinimalAnalyzer = new EnglishMinimalAnalyzer

      // Main search
      val qLi = if (queryLi._2 != 0) try {
        new QueryParser(Version.LUCENE_46, "contentLi", whiteSpaceAnalyzer).parse(queryLi._1)
      } catch {
        case e: Exception => {
          e.printStackTrace()
          null
        }
      } else null
      val qL1 = if (queryL1._2 != 0) try {
        new QueryParser(Version.LUCENE_46, "contentL1", whiteSpaceAnalyzer).parse(queryL1._1)
      } catch {
        case e: Exception => {
          e.printStackTrace()
          null
        }
      } else null
      val qL2 = if (queryL2._2 != 0) try {
        new QueryParser(Version.LUCENE_46, "contentL2", whiteSpaceAnalyzer).parse(queryL2._1)
      } catch {
        case e: Exception => {
          e.printStackTrace()
          null
        }
      } else null
      val qTS = if (!queryTS.isEmpty) try {
        new QueryParser(Version.LUCENE_46, "content", englishMinimalAnalyzer).parse(queryTS)
      } catch {
        case e: Exception => {
          e.printStackTrace()
          null
        }
      } else null

      // Cushion search to reach the 1000 retrieved documents
      val qSLi = if (querySLi._2 != 0) try {
        new QueryParser(Version.LUCENE_46, "contentLi", whiteSpaceAnalyzer).parse(querySLi._1)
      } catch {
        case e: Exception => {
          e.printStackTrace()
          null
        }
      } else null
      val qSL1 = if (querySL1._2 != 0) try {
        new QueryParser(Version.LUCENE_46, "contentL1", whiteSpaceAnalyzer).parse(querySL1._1)
      } catch {
        case e: Exception => {
          e.printStackTrace()
          null
        }
      } else null
      val qSL2 = if (querySL2._2 != 0) try {
        new QueryParser(Version.LUCENE_46, "contentL2", whiteSpaceAnalyzer).parse(querySL2._1)
      } catch {
        case e: Exception => {
          e.printStackTrace()
          null
        }
      } else null

      def searchFormulaByID(label: String, query: String, partialID: String, indexPath: String, similarity: Similarity): List[(String, Float, String)] = {
        val index = FSDirectory.open(new File(indexPath))
        val reader = DirectoryReader.open(index)
        val searcher = new IndexSearcher(reader)
        if (similarity != null)
          searcher.setSimilarity(similarity)

        val nquery = if (!query.isEmpty)
          new QueryParser(Version.LUCENE_46, "content" + label, analyzerFormula).parse("+(" + query + ")" + " +id:" + QueryParserUtil.escape(partialID) + "*")
        else
          new QueryParser(Version.LUCENE_46, "content" + label, analyzerFormula).parse(" +id:" + QueryParserUtil.escape(partialID) + "*")
        val collector = TopScoreDocCollector.create(20, true)
        searcher.search(nquery, collector)
        val hits = collector.topDocs().scoreDocs

        //println(label + ": found " + hits.length + " hits.")
        val res = (for (i <- 0 until hits.length) yield {
          val docId = hits(i).doc
          val score = hits(i).score
          val d = searcher.doc(docId)

          (d.get("id"), score, d.get("content" + label))
        }).toList //.filter(f => {
        /*val mathMatchs = "<annotation-xml(?:(?!<annotation-xml).)*</annotation-xml>".r.findFirstMatchIn(f._3)
        if (mathMatchs.nonEmpty) {
          val xmlstr: String = {
            val mClean = TreeTokenizer.cleanXMLString(mathMatchs.get.toString)
            mClean
          }
          val nf3 = scala.xml.XML.loadString(TreeTokenizer.cleanXMLString(xmlstr))
          val tokensLi = TreeTokenizer.formulaTokenizerLiteral(nf3)
          //val tokensL1 = TreeTokenizer.formulaTokenizerL1(nf3)

          tokensLi.size >= queryLi.split("content:").size // && tokensL1.size >= queryL1.split("content:").size*/

        //} else false
        //println(f._3 + "##################")

        /*   if(f._3.isEmpty) false
           else
           {
             f._3.split(" ").size >= queryLi.split("content:").size
           }
         }*/
        //)
        reader.close
        res
      }

      def searchFormula(label: String, query: Query, indexPath: String, similarity: Similarity, nhits: Int): List[(String, Float, String)] = {
        val index = FSDirectory.open(new File(indexPath))
        val reader = DirectoryReader.open(index)
        val searcher = new IndexSearcher(reader)
        if (similarity != null)
          searcher.setSimilarity(similarity)

        val collector = TopScoreDocCollector.create(nhits, true)
        searcher.search(query, collector)
        val hits = collector.topDocs().scoreDocs

        println(label + ": found " + hits.length + " hits.")
        val res = (for (i <- 0 until hits.length) yield {
          val docId = hits(i).doc
          val score = hits(i).score
          val d = searcher.doc(docId)
          //println((d.get("id"), score, d.get("content")))
          (d.get("id"), score, d.get("content" + label))
        }).toList
        reader.close
        res
      }

      def searchText(label: String, query: Query, indexPath: String, similarity: Similarity, h: Int): List[(String, Float, String, List[(String, Float, String)])] = {
        val index = FSDirectory.open(new File(indexPath))
        val reader = DirectoryReader.open(index)
        val searcher = new IndexSearcher(reader)
        if (similarity != null)
          searcher.setSimilarity(similarity)

        val collector = TopScoreDocCollector.create(h, true)
        searcher.search(query, collector)
        val hits = collector.topDocs().scoreDocs

        println(label + ": found " + hits.length + " hits.")
        val res = (for (i <- 0 until hits.length) yield {
          val docId = hits(i).doc
          val score = hits(i).score
          val d = searcher.doc(docId)

          val listFormulas = if (!querySLi._1.isEmpty) {
            searchFormulaByID(
              "Li", querySLi._1,
              d.get("id").split("/").take(3).mkString("/"),
              "/Users/aldo/Experiments/2014-NTCIR/Indices/FormulaTreeIndex", new BM25Similarity)
          } else searchFormulaByID(
            "Li", "",
            d.get("id").split("/").take(3).mkString("/"),
            "/Users/aldo/Experiments/2014-NTCIR/Indices/FormulaTreeIndex", new BM25Similarity)
          (d.get("id"), score, d.get("content"), listFormulas)
        }).toList
        reader.close
        res
      }

      val sim = new BM25Similarity()
      val resLi = if (qLi != null) searchFormula("Li", qLi, "/Users/aldo/Experiments/2014-NTCIR/Indices/FormulaTreeIndex", sim, 1000) else Nil
      val resL1 = if (qL1 != null) searchFormula("L1", qL1, "/Users/aldo/Experiments/2014-NTCIR/Indices/FormulaTreeIndex", sim, 1000) else Nil
      val resL2 = if (qL2 != null) searchFormula("L2", qL2, "/Users/aldo/Experiments/2014-NTCIR/Indices/FormulaTreeIndex", sim, 1000) else Nil
      val resTS = if (qTS != null) searchText("ST", qTS, "/Users/aldo/Experiments/2014-NTCIR/Indices/TextSimple", sim, 100) else Nil

      // Cushion if needed
      lazy val resSLi = if (qSLi != null) searchFormula("SLi", qSLi, "/Users/aldo/Experiments/2014-NTCIR/Indices/FormulaTreeIndex", sim, 2000) else Nil
      lazy val resSL1 = if (qSL1 != null) searchFormula("SL1", qSL1, "/Users/aldo/Experiments/2014-NTCIR/Indices/FormulaTreeIndex", sim, 2000) else Nil
      lazy val resSL2 = if (qSL2 != null) searchFormula("SL2", qSL2, "/Users/aldo/Experiments/2014-NTCIR/Indices/FormulaTreeIndex", sim, 2000) else Nil

      def distNorm(a: Int, b: Int) = Math.abs(a - b)

      // Formula level
      val resTot = {
        //val maxLi = if (resLi.isEmpty) 1 else resLi.map(_._2).max
        //val maxTS = if (resTS.isEmpty) 1 else resTS.map(_._2).max
        val maxL1 = if (resL1.isEmpty) 1 else resL1.map(_._2).max
        //val maxL2 = if (resL2.isEmpty) 1 else resL2.map(_._2).max

        val resText: List[(String, Float, String)] = resTS.map(e => {
          val scoreDoc = e._2 / 10
          e._4.map(f => (f._1, f._2 + scoreDoc, f._3))
        }).flatten.groupBy(_._1).mapValues(l => (l.head._1, l.map(_._2).max, l.head._3)).values.toList.sortBy(-_._2)

        def normalizeList(label: String, res: List[(String, Float, String)]): List[(String, Float, String)] =
          res.map(e => {

            val mathMatchs = "<annotation-xml(?:(?!<annotation-xml).)*</annotation-xml>".r.findFirstMatchIn(e._3)
            if (mathMatchs != None) {
              val matche = mathMatchs.get.toString
              val xmlstr: String = TreeTokenizer.cleanXMLString(matche)
              val nf3 = scala.xml.XML.loadString(TreeTokenizer.cleanXMLString(xmlstr))

              val size =
                if (label == "Li")
                  distNorm(TreeTokenizer.toVector(TreeTokenizer.formulaTokenizerLiteral(nf3)).values.sum, queryL1._2)
                else if (label == "L1")
                  distNorm(TreeTokenizer.toVector(TreeTokenizer.formulaTokenizerL1(nf3)).values.sum, queryL1._2)
                else if (label == "L2")
                  distNorm(TreeTokenizer.toVector(TreeTokenizer.formulaTokenizerL2(nf3)).values.sum, queryL2._2)
                else if (label == "Doc")
                  distNorm(TreeTokenizer.toVector(TreeTokenizer.formulaTokenizerLiteral(nf3)).values.sum, queryLi._2) +
                    distNorm(TreeTokenizer.toVector(TreeTokenizer.formulaTokenizerL1(nf3)).values.sum, queryL1._2) +
                    distNorm(TreeTokenizer.toVector(TreeTokenizer.formulaTokenizerL2(nf3)).values.sum, queryL2._2)
                else
                  1000

              val k = 3
              val a = k / (Math.log(size + 1) + k).asInstanceOf[Float]
              (e._1, e._2 * a, e._3)
            } else {
              println("NONE found")

              val a = 0.5f
              (e._1, e._2 * a / maxL1, e._3)
            }
          })

        def sortResult(l: List[(String, Float, String)]): List[(String, Float, String)] =
          l.sortBy(-_._2)

        def sortResultAndDump(l: List[(String, Float, String)]): List[(String, Float, String)] =
          sortResult(l.map(l => (l._1, l._2 / 100000, l._3)).toList)

        def compactResult(l: List[(String, Float, String)]): List[(String, Float, String)] =
          sortResult(l.groupBy(_._1).mapValues(l => (l.head._1, l.map(_._2).sum, l.head._3)).values.toList)

        def compactResultAndDump(l: List[(String, Float, String)]): List[(String, Float, String)] =
          sortResult(l.groupBy(_._1).mapValues(l => (l.head._1, l.map(_._2).sum / 100000, l.head._3)).values.toList)

        // merge results and join based on formula
        val resForm = sortResultAndDump(
          if (isNormalized) ((normalizeList("L1", resL1) ::: normalizeList("L2", resL2)) ::: normalizeList("Li", resLi))
          else ((resL1 ::: resL2) ::: resLi) ::: resText)

        // val resForm = compactResult(
        //  if (isNormalized) ((normalizeList("L1", resL1) ::: normalizeList("L2", resL2)) ::: normalizeList("Li", resLi)) // ::: normalizeList("Doc", resText)
        //  else ((resL1 ::: resL2) ::: resLi)) // ::: resText)

        // results joining based on document
        val resDoc = compactResult(resForm.map(e => (e._1.split(filterExt).head.split("/").last, e._2, e._3)))
        //val resDoc = compactResult(resForm.map(e => (e._1.split("@")(1), e._2, e._3)))

        // system to reach the 1000 results
        val preRes = compactResultAndDump(resSL1 ::: resSL2 ::: resSLi).map(e => (e._1.split(filterExt).take(1).head.split("/").last, e._2, e._3))
        //val preRes = compactResultAndDump(resSL1 ::: resSL2 ::: resSLi).map(e => (e._1.split("@")(1), e._2, e._3))

        //val res = compactResult(resDoc ::: preRes)
        val res = compactResult(resDoc ::: preRes)
        println("Result size: " + res.size)
        (resForm, res)
      }

      (id -> resTot._2.map(e => (e._1, e._2)))
    }

    println("Search------------------------------------------")
    val res = topics.topicIDs.map(id => {
      doSearch(id, topics.get(id).get, topics.isWithKeywords, expansions)
    }).toMap

    Output.printMath2NTCIR("TUW-IMP", nameRun, res)
  }

  def toString(b: Boolean): String = if (b) "YES" else "NO"

  def toString(clause: BooleanClause.Occur): String = {
    if (clause == BooleanClause.Occur.MUST)
      "MUST"
    else if (clause == BooleanClause.Occur.SHOULD)
      "SHOULD"
    else
      null
  }
}
