package at.ac.tuwien.math

import java.io.{File, FileInputStream}

import at.ac.tuwien.utility.{Profile, Progress}
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.document.{Field, StringField, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version

import scalaz.EphemeralStream

/**
 * Created by aldo on 5/21/14.
 */
object FormulaIndexer {

  class RawDocument(val id: String, val content: String)

  class Document(val id: String, val formulas: List[MathFormula])

  class MathFormula(val id: String, val content: xml.Node)

  class MathFormulaVector(val id: String, val vector: Map[String, Int])

  def index(documentCollectionFile: String, indexPath: String) {
    val fIS = new FileInputStream(documentCollectionFile)
    val gIS = new GzipCompressorInputStream(fIS)
    val tarIn = new TarArchiveInputStream(gIS)

    def readFileContent(in: TarArchiveInputStream): String = {
      val bytes = new Array[Byte](in.getCurrentEntry.getSize.asInstanceOf[Int])
      in.read(bytes, 0, in.getCurrentEntry.getSize.asInstanceOf[Int])
      new String(bytes, "UTF-8")
    }

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

    def extractDocument(rawDocument: RawDocument): Document = new Document(
    rawDocument.id, {
      val content =
        if (rawDocument.id.endsWith(".html")) {
          "<root>" + rawDocument.content.split("<mat").flatMap(_.split("</math")).filter(_.startsWith("h")).map("<mat" + _ + "</math>").mkString("") + "</root>"
        } else {
          if (rawDocument.content.contains("?xml")) rawDocument.content else "<root>" + rawDocument.content + "</root>"
        }
      try {
        val xml = scala.xml.XML.loadString(content)
        xml \\ "math" map { m =>
          val id = (m \ "@id").toString
          val annotation =
            if ((m \\ "annotation-xml").size == 0) {
              println("Error " + rawDocument.id + "@" + id + ": no annotation-xml")
              null
            } else
              (m \\ "annotation-xml").head
          if (annotation != null) {
            val mClean = TreeTokenizer.cleanXMLString(annotation.toString())
            val xmlClean = scala.xml.XML.loadString(mClean)
            new MathFormula(id, xmlClean)
          } else {
            new MathFormula(id, null)
          }
        } toList
      } catch {
        case e: Exception => {
          e.printStackTrace();
          Nil
        }
      }
    })

    val analyzer = new WhitespaceAnalyzer(Version.LUCENE_46)
    if (!new File(indexPath).exists()) new File(indexPath).mkdirs()
    val index = FSDirectory.open(new File(indexPath))
    val config = new IndexWriterConfig(Version.LUCENE_46, analyzer)
    val w = new IndexWriter(index, config)

    def addDoc(w: IndexWriter, id: String, contentLi: String, contentL1: String, contentL2: String) {
      val doc = new org.apache.lucene.document.Document
      doc.add(new StringField("id", id, Field.Store.YES))
      doc.add(new TextField("contentLi", contentLi, Field.Store.YES))
      doc.add(new TextField("contentL1", contentL1, Field.Store.YES))
      doc.add(new TextField("contentL2", contentL2, Field.Store.YES))
      w.addDocument(doc)
    }

    val rawDocuments = getStreamRawDocument(tarIn).filter(d => d.id.endsWith("html") && !d.id.contains("._"))
    val documents = rawDocuments.map(d => extractDocument(d))

    Profile.time(documents.map(d => {
      val id = d.id
      for (f <- d.formulas.toList if (f.content != null)) {
        val vectorLi = TreeTokenizer.toVector(TreeTokenizer.formulaTokenizerLiteral(f.content))
        val vectorL1 = TreeTokenizer.toVector(TreeTokenizer.formulaTokenizerL1(f.content))
        val vectorL2 = TreeTokenizer.toVector(TreeTokenizer.formulaTokenizerL2(f.content))

        Progress.log(addDoc(w, id + "@" + f.id,
          vectorLi.map(e => (0 until e._2).map(s => e._1).mkString(" ")).mkString(" "),
          vectorL1.map(e => (0 until e._2).map(s => e._1).mkString(" ")).mkString(" "),
          vectorL2.map(e => (0 until e._2).map(s => e._1).mkString(" ")).mkString(" ")))
      }
    }).toList)

    w.close()
  }
}
