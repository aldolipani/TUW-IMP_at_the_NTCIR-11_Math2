package at.ac.tuwien.math


/**
 * Created by aldo on 17/12/14.
 */
object Main extends App {

  override def main(args: Array[String]): Unit = {

    def parseArgs(args: Array[String]) = {
      def parseArgs(args: List[String], opts: Map[Symbol, String]): Map[Symbol, String] = args match {
        case "--help" :: ts =>
          Map('cmd -> "help")
        case "--cmd" :: "search" :: ts =>
          parseArgs(ts, Map('cmd -> "search"))
        case "--cmd" :: "index-text" :: ts =>
          parseArgs(ts, Map('cmd -> "index-text"))
        case "--cmd" :: "index-formulae" :: ts =>
          parseArgs(ts, Map('cmd -> "index-formulae"))
        case "--formula-index" :: path :: ts =>
          parseArgs(ts, opts + ('formulaIndex -> path))
        case "--text-index" :: path :: ts =>
          parseArgs(ts, opts + ('textIndex -> path))
        case "--name-run" :: name :: ts =>
          parseArgs(ts, opts + ('nameRun -> name))
        case "--topics" :: path :: ts =>
          parseArgs(ts, opts + ('topics -> path))
        case "--test-collection" :: path :: ts =>
          parseArgs(ts, opts + ('testCollection -> path))
        case "--index-folder" :: path :: ts =>
          parseArgs(ts, opts + ('indexFolder -> path))
        case "--expansions" :: path :: ts =>
          parseArgs(ts, opts + ('expansions -> path))
        case nn :: ts => throw new Exception(nn + ": symbol not found")
        case Nil => opts
      }
      parseArgs(args.toList, Map())
    }

    val opts = parseArgs(args)

    opts.get('cmd) match {
      case Some("help") => help()
      case Some("search") => parseSearchCmd(opts)
      case Some("index-formulae") => parseIndexFormulaeCmd(opts)
      case Some("index-text") => parseIndexTextCmd(opts)
      case Some(str) => new Exception(str + " command not found: --help for details")
      case _ => help()
    }
  }

  def parseSearchCmd(opts: Map[Symbol, String]) = {
    try {
      val nameRun = opts.get('nameRun).get
      val topicsXML = opts.get('topics).get
      val formulaIndex = opts.get('formulaIndex).get
      val textIndex = opts.get('textIndex).get
      val expansionsXML = opts.get('expansions).getOrElse(null)
      Search.search(nameRun, topicsXML, formulaIndex, textIndex, expansionsXML)
    } catch {
      case e: NoSuchElementException => {
        e.printStackTrace();
        help()
      }
    }
  }

  def parseIndexTextCmd(opts: Map[Symbol, String]) = {
    try {
      val testCollectionFile = opts.get('testCollection).get
      val indexFolder = opts.get('indexFolder).get
      TextIndexer.index(testCollectionFile, indexFolder)
    } catch {
      case e: NoSuchElementException => {
        e.printStackTrace();
        help()
      }
    }
  }

  def parseIndexFormulaeCmd(opts: Map[Symbol, String]) = {
    try {
      val testCollectionFile = opts.get('testCollection).get
      val indexFolder = opts.get('indexFolder).get
      FormulaIndexer.index(testCollectionFile, indexFolder)
    } catch {
      case e: NoSuchElementException => {
        e.printStackTrace();
        help()
      }
    }
  }

  def help(): Unit = {
    println(
      """
        |Welcome in the system developed by the TUW-IMP team for the NTCIR Math2 challenge!
        |Repository: https://github.com/aldolipani/TUW-IMP_at_the_NTCIR-11_Math2
        |Following the list of available commands:
        |Search:    --cmd search --name-run <name> --topics <path> --expansions <path> --formula-index <path> --text-index <path>
        |Index
        | Text:     --cmd index-text     --test-collection <path> --index-folder <path>
        | Formulae: --cmd index-formulae --test-collection <path> --index-folder <path>
      """.stripMargin)
  }
}
