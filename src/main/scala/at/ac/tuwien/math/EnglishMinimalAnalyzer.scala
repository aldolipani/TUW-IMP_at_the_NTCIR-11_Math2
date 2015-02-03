package at.ac.tuwien.math

import java.io.Reader

import org.apache.lucene.analysis.Analyzer.TokenStreamComponents
import org.apache.lucene.analysis.core.{LowerCaseFilter, StopAnalyzer, StopFilter}
import org.apache.lucene.analysis.en.EnglishMinimalStemFilter
import org.apache.lucene.analysis.standard.StandardTokenizer
import org.apache.lucene.analysis.{Analyzer, TokenStream, Tokenizer}
import org.apache.lucene.util.Version

/**
 * Created by aldo on 6/6/14.
 */
class EnglishMinimalAnalyzer extends Analyzer {

  override def createComponents(s: String, r: Reader): TokenStreamComponents = {
    val source: Tokenizer = new StandardTokenizer(Version.LUCENE_46, r)

    var result: TokenStream = null
    result = new LowerCaseFilter(Version.LUCENE_46, source)
    result = new StopFilter(Version.LUCENE_46, result, StopAnalyzer.ENGLISH_STOP_WORDS_SET)
    result = new EnglishMinimalStemFilter(result)

    new TokenStreamComponents(source, result)
  }
}
