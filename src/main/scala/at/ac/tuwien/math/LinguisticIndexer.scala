package at.ac.tuwien.math


/**
 * Created by aldo on 5/21/14.
 */
object LinguisticIndexer extends App {
  /*
    def cleanXMLString(str: String): String =
      "[a-zA-Z-]+=\"(?:(?!\").)+\"".r.replaceAllIn(str, "")

    def toVector(tokens: List[String]): Map[String, Int] = tokens.groupBy(_.toString).mapValues(_.size)

    lazy val tagger: MaxentTagger = new MaxentTagger(Resources.STANFORD_EN_POS_MODEL)

    def posTag(str: String): List[(String, String)] =
      if (str.trim.nonEmpty) {
        try {
          tagger.tagString(str).split("[ ]").map(_.split("_")).map(e => (e(0), e(1))).toList
        } catch {
          case e: Exception => println(e.getMessage)
            Nil
        }
      }
      else
        Nil

    override def main(args: Array[String]): Unit = {
      val fIS = new FileInputStream("/home/aldo/Desktop/NTCIR_2014_dataset_XHTML5.tar.gz")
      val gIS = new GzipCompressorInputStream(fIS)
      val tarIn = new TarArchiveInputStream(gIS)

      def readFileContent(in: TarArchiveInputStream): String = {
        val bytes = new Array[Byte](in.getCurrentEntry.getSize.asInstanceOf[Int])
        in.read(bytes, 0, in.getCurrentEntry.getSize.asInstanceOf[Int])
        new String(bytes, "UTF-8")
      }

      class RawDocument(val id: String, val content: String)
      class Document(val id: String, val text: String)
      class POSDocument(val id: String, val pos: List[(String, String)])


      def getStreamRawDocument(tarIn: TarArchiveInputStream): EphemeralStream[RawDocument] = {
        def next: EphemeralStream[RawDocument] = {
          val nextEntry = tarIn.getNextEntry
          if (nextEntry != null) {
            if (!nextEntry.isDirectory)
              EphemeralStream.cons(new RawDocument(nextEntry.getName, readFileContent(tarIn)), next)
            else
              next
          } else {
            EphemeralStream.emptyEphemeralStream
          }
        }
        next
      }

      /*val removeIt = new RewriteRule {
        override def transform(n: Node): NodeSeq = n match {
          case e: Elem if e.label == "math" => NodeSeq.Empty
          case e => n
        }
      }*/

      //val tran = new RuleTransformer(removeIt)

      val tran = SAXParserImpl.newInstance(null)

      class Handler extends DefaultHandler {
        val sb: StringBuilder = new StringBuilder
        private var keep: Boolean = true
        private var edge: Boolean = false

        override def characters(ch: Array[Char], start: Int, length: Int) =
          if (keep)
            sb.appendAll(ch, start, length)

        def getText: String = sb.toString

        override def startElement(uri: String, localName: String, qName: String, atts: Attributes) {
          if (localName.equalsIgnoreCase("math"))
            if (keep) edge = true
          keep = false
          if (edge) {
            sb.append("MATHFORMULA")
            edge = false
          }
        }

        override def endElement(uri: String, localName: String, qName: String) {
          if (localName.equalsIgnoreCase("math"))
            keep = true
        }
      }

      def parseXML(str: String): List[(String, String)] = {
        val stream: InputStream = new ByteArrayInputStream(str.getBytes(StandardCharsets.UTF_8))
        val p = new Parser
        val h = new Handler
        p.setContentHandler(h)
        p.parse(new InputSource(new StringReader(str)))
        posTag(h.getText)
      }

      def extractPOSDocument(rawDocument: RawDocument): POSDocument = new POSDocument(
      rawDocument.id, {
        //println(tran.transform(scala.xml.XML.loadString(rawDocument.content)).text)
        parseXML(rawDocument.content)
      }
      )

      val rawDocuments = getStreamRawDocument(tarIn)

      val analyzer = new EnglishMinimalAnalyzer
  //    val index = FSDirectory.open(new File("/home/aldo/Indices/math/LingJJN*/
  //index"))
  /*    val index = FSDirectory.open(new File("/home/aldo/Indices/math/LingNNP/index"))
      val config = new IndexWriterConfig(Version.LUCENE_46, analyzer)

      val w = new IndexWriter(index, config)

      def addDoc(w: IndexWriter, id: String, content: String) {
        val doc = new org.apache.lucene.document.Document
        doc.add(new StringField("id", id, Field.Store.YES))
        doc.add(new TextField("content", content, Field.Store.YES))
        w.addDocument(doc)
      }

      def linguisticFilter(filterPattern: String, posDocument: POSDocument): Document = {
        def toFilter(l: List[(String, String)]): String = {
          //println(l.map(_._2).mkString("@"))
          val str = ("^(" + filterPattern + ")").r.findFirstMatchIn(l.map(_._2).mkString("@")).map(_ group 1)
          if (str == None)
            ""
          else
            str.get
        }

        def filter(list: List[(String, String)], acc: List[String]): List[String] = {
          if (list.isEmpty)
            acc
          else {
            val filtered = toFilter(list)
            if (filtered.nonEmpty)
              filter(list.tail, acc :+ list.take(filtered.count(_ == '@') + 1).map(_._1).filterNot(_ == "MATHFORMULA").mkString("_"))
            else
              filter(list.tail, acc)
          }
        }

        new Document(posDocument.id, filter(posDocument.pos, Nil).mkString(" "))
      }

      val documents = rawDocuments.map(d => {
        val posDocument = extractPOSDocument(d)
        new Document(d.id,
  //        linguisticFilter("JJ[RS]?(@NN[SP]*)+", posDocument).text)
          linguisticFilter("NNP", posDocument).text)
      })

      println("start scan")
      for (document <- documents) {
        println("document: " + document.id)
        println("content: " + document.text)
        addDoc(w, document.id, document.text)
      }
      println("end scan")
      w.close();
    }

      */
  /*val tokenStream = analyzer.tokenStream("none", new StringReader(document.text))
  val termAttribute = tokenStream.getAttribute(classOf[CharTermAttribute]);
  tokenStream.reset
  while (tokenStream.incrementToken()) {
  val term = termAttribute.toString
  println(term)
  }*/
}
