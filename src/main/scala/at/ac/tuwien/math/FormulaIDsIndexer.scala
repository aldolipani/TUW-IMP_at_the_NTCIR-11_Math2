package math

import java.io.{File, FileInputStream}

import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.{Field, StringField, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version
import at.ac.tuwien.utility.{Profile, Progress}

import scalaz.EphemeralStream


/**
 * Created by aldo on 5/21/14.
 */
object FormulaIDsIndexer extends App {

  override def main(args: Array[String]): Unit = {
    // Parameters
    val collectionPath = ""
    val indexName = ""
    val indexPath = ""
    // End Parameters

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
        } else EphemeralStream.emptyEphemeralStream
      }
      next
    }

    def extractDocument(rawDocument: RawDocument): Document = new Document(
      rawDocument.id.replaceAll(".xhtml", ""),
      try {
        val math = scala.xml.XML.loadString(rawDocument.content) \\ "math"
       // println(rawDocument.id)
        math.map(m => {
          val id = (m \ "@id").text
          new MathFormula(id, m.toString())
        }).toList
      } catch {
        case e: Exception => Nil
      })

    def addDoc(w: IndexWriter, id: String, documentId: String, formulaId: String, content: String) {
      val doc = new org.apache.lucene.document.Document
      doc.add(new StringField("id", id, Field.Store.YES))
      doc.add(new StringField("formulaId", formulaId, Field.Store.YES))
      doc.add(new StringField("documentId", documentId, Field.Store.YES))
      doc.add(new TextField("content", content, Field.Store.YES))
      w.addDocument(doc)
    }

    def combineIDs(documentId: String, formulaId: String): String = documentId + "#" + formulaId

    class RawDocument(val id: String, val content: String)
    class Document(val id: String, val formulas: List[MathFormula])
    class MathFormula(val id: String, val content: String)
    class MathFormulaVector(val id: String, val vector: Map[String, Int])

    val fIS = new FileInputStream(collectionPath)
    val gIS = new GzipCompressorInputStream(fIS)
    val tarIn = new TarArchiveInputStream(gIS)
    val rawDocuments = getStreamRawDocument(tarIn)

    val indexDir = new File(indexPath)
    if (!indexDir.exists) indexDir.mkdirs

    val analyzer = new KeywordAnalyzer
    val index = FSDirectory.open(new File(indexPath))
    val config = new IndexWriterConfig(Version.LUCENE_46, analyzer)
    val w = new IndexWriter(index, config)

    val documents = rawDocuments.map(extractDocument(_))

    Profile.time(
      documents map { document =>
        val documentId = document.id.split( """[\\/]""").last
        //println(documentId)
        if (document.formulas != null)
          for (formula <- document.formulas)
            Progress.log(addDoc(w, document.id+formula.id, documentId, combineIDs(documentId, formula.id), formula.content))
        else
          Progress.log(addDoc(w, document.id+"null", documentId, combineIDs(documentId, "null"), ""))
      } toList)
    w.close
  }
}

