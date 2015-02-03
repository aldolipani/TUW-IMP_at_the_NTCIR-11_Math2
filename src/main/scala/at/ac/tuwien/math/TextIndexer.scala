package at.ac.tuwien.math

import java.io._

import at.ac.tuwien.utility.{Profile, Progress}
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream
import org.apache.lucene.document.{Field, StringField, TextField}
import org.apache.lucene.index.{IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util.Version

import scalaz.EphemeralStream

/**
 * Created by aldo on 5/21/14.
 */
object TextIndexer {

  class RawDocument(val id: String, val content: String)

  class Document(val id: String, val text: String)

  def cleanXMLString(str: String): String =
    "[a-zA-Z-]+=\"(?:(?!\").)+\"".r.replaceAllIn(str, "")

  def toVector(tokens: List[String]): Map[String, Int] = tokens.groupBy(_.toString).mapValues(_.size)

  def index(documentCollectionFile: String, indexPath: String) {
    val fIS = new FileInputStream(documentCollectionFile)
    val gIS = new GzipCompressorInputStream(fIS)
    val tarIn = new TarArchiveInputStream(gIS)

    def readFileContent(in: TarArchiveInputStream): String = {
      val bytes = new Array[Byte](in.getCurrentEntry.getSize.asInstanceOf[Int])
      in.read(bytes, 0, in.getCurrentEntry.getSize.asInstanceOf[Int])
      new String(bytes, "UTF-8")
    }

    def getStreamRawDocuments(tarIn: TarArchiveInputStream): EphemeralStream[RawDocument] = {
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

    def parseXML(str: String): String = scala.xml.XML.loadString(str).text

    def extractDocument(rawDocument: RawDocument): Document = new Document(rawDocument.id, parseXML(rawDocument.content))

    val rawDocuments = getStreamRawDocuments(tarIn)

    val analyzer = new EnglishMinimalAnalyzer
    val index = FSDirectory.open(new File(indexPath))
    val config = new IndexWriterConfig(Version.LUCENE_46, analyzer)

    val w = new IndexWriter(index, config)

    def addDoc(w: IndexWriter, id: String, content: String) {
      val doc = new org.apache.lucene.document.Document
      doc.add(new StringField("id", id, Field.Store.YES))
      doc.add(new TextField("content", content, Field.Store.NO))
      w.addDocument(doc)
    }

    val documents = rawDocuments.map(extractDocument(_))

    Profile.time(documents.map(d => {
      val id = d.id
      Progress.log(addDoc(w, id, d.text))
    }).toList)

    w.close()
  }
}