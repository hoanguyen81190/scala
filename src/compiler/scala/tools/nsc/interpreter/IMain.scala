/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package interpreter

import Predef.{ println => _, _ }
import util.stringFromWriter
import scala.reflect.internal.util._
import java.net.URL
import io.VirtualDirectory
import reporters._
import scala.tools.util.PathResolver
import scala.tools.nsc.util.ScalaClassLoader
import IMain._
import java.lang.Class


class IMain(initialSettings: Settings, protected val out: JPrintWriter) {
  imain =>

  /** Leading with the eagerly evaluated.
   */
  val virtualDirectory: VirtualDirectory            = new VirtualDirectory("(memory)", None) // "directory" for classfiles
  private var _initializeComplete                   = false     // compiler is initialized

  private var _classLoader: AbstractFileClassLoader = null                              // active classloader
  private val _compiler: Global                     = newCompiler(settings, reporter)   // our private compiler

  private val nextReqId = {
    var counter = 0
    () => { counter += 1 ; counter }
  }

  def compilerClasspath: Seq[URL] = (
    if (isInitializeComplete) global.classPath.asURLs
    else new PathResolver(settings).result.asURLs  // the compiler's classpath
    )
  def settings = initialSettings
  def this(settings: Settings) = this(settings, new NewLinePrintWriter(new ConsoleWriter, true))
  def this() = this(new Settings())

  lazy val reporter: ConsoleReporter = new ConsoleReporter(this.settings, Console.in, this.out)

  import reporter.{ printMessage}

  private def _initSources = List(new BatchSourceFile("<init>", "class $repl_$init { }"))


  def isInitializeComplete = _initializeComplete

  /** the public, go through the future compiler */
  lazy val global: Global = {
      new _compiler.Run() compileSources _initSources
      _initializeComplete = true
      _compiler
  }
  @deprecated("Use `global` for access to the compiler instance.", "2.9.0")
  lazy val compiler: global.type = global

  import global._

  object naming extends {
    val global: imain.global.type = imain.global
  } with Naming
  import naming._

  lazy val memberHandlers = new {
    val intp: imain.type = imain
  } with MemberHandlers
  import memberHandlers._


  /** Instantiate a compiler.  Overridable. */
  protected def newCompiler(settings: Settings, reporter: Reporter): Global = {
    settings.outputDirs setSingleOutput virtualDirectory
    settings.exposeEmptyPackage.value = true
    new Global(settings, reporter)
  }

  /** Parent classloader.  Overridable. */
  protected def parentClassLoader: ClassLoader =
    settings.explicitParentLoader.getOrElse( this.getClass.getClassLoader() )

  def classLoader: AbstractFileClassLoader = {
    _classLoader = makeClassLoader()
    _classLoader
  }

  private def makeClassLoader(): AbstractFileClassLoader =
    new AbstractFileClassLoader(virtualDirectory, ScalaClassLoader fromURLs compilerClasspath)


  def compileSourcesKeepingRun(sources: SourceFile*) = {
    val run = new Run()
    reporter.reset()
    run compileSources sources.toList
    (!reporter.hasErrors, run)
  }

  def compileSources(sources: SourceFile*): Boolean =
    compileSourcesKeepingRun(sources: _*)._1

  private def requestFromLine(line: String): Request = {
    val trees = parse(line) match {
      case None         => return null
      case Some(Nil)    => return null
      case Some(trees)  => trees
    }
    new Request(line, trees)
  }

  def interpret(line: String): Boolean = {
    if (global == null) false
    else {
      val req = requestFromLine(line)
        if (req == null || !req.compile) false
        else {
          classLoader.setAsContext()
          val result = req.loadAndRun
          printMessage(result stripSuffix "\n")
          true
        }
    }
  }


  def close() {
    reporter.flush()
  }

  class ReadEvalPrint(lineId: Int) {
    def this() = this(freshLineId())

    val packageName = sessionNames.line + lineId
    val readName    = sessionNames.read
    val evalName    = sessionNames.eval
    val printName   = sessionNames.print
    val resultName  = sessionNames.result

    def packageDecl = "package " + packageName

    def pathTo(name: String)   = packageName + "." + name
    def packaged(code: String) = packageDecl + "\n\n" + code

    def readPath  = pathTo(readName)
    def evalPath  = pathTo(evalName)
    def printPath = pathTo(printName)

    def call(name: String, args: Any*): AnyRef = {
      val m = evalMethod(name)
      m.invoke(evalClass, args.map(_.asInstanceOf[AnyRef]): _*)
    }

    private def load(path: String): Class[_] = {
       Class.forName(path, true, classLoader)
    }

    lazy val evalClass = load(evalPath)

    def compile(source: String): Boolean = compileAndSaveRun("<console>", source)

    def resolvePathToSymbol: Symbol = {
      def getModuleOrClass(path: Name, len: Int): Symbol = {
        val point = path lastPos('.', len - 1)
        val owner =
          if (point > 0) getModuleOrClass(path.toTermName, point)
          else RootClass
        val name = path subName (point + 1, len)
        val sym = owner.info member name
        val MODULE        = 1 << 8
        val result = if (path.isTermName) sym.suchThat(_ hasFlag MODULE) else sym
        result
      }
      val path = newTermNameCached(readPath).toTermName
      val len = path.length
      getModuleOrClass(path, len)
    }

    private def evalMethod(name: String) = evalClass.getMethods filter (_.getName == name) match {
      case Array(method) => method
      case xs            => sys.error("Internal error: eval object " + evalClass + ", " + xs.mkString("\n", "\n", ""))
    }
    private def compileAndSaveRun(label: String, code: String) = {
      val (success, run) = compileSourcesKeepingRun(new BatchSourceFile(label, packaged(code)))
      success
    }
  }

  class Request(val line: String, val trees: List[Tree]) {
    val reqId = nextReqId()
    val lineRep = new ReadEvalPrint()

    private var _originalLine: String = null

    def originalLine = if (_originalLine == null) line else _originalLine

    val handlers: List[MemberHandler] = trees map (memberHandlers chooseHandler _)
    def defHandlers = handlers collect { case x: MemberDefHandler => x }

    def termNames = handlers flatMap (_.definesTerm)
    def typeNames = handlers flatMap (_.definesType)

    def fullPath(vname: String) = (
      lineRep.readPath + ".`%s`".format(vname)
      )

    def fullPath(vname: Name): String = fullPath(vname.toString)

    def toCompute = line
//    def lookupTypeOf(name: Name) = typeOf.getOrElse(name, typeOf(global.encode(name.toString)))

    private object ObjectSourceCode extends CodeAssembler[MemberHandler] {
      val preamble = """
                       |object %s {
                       |%s
                     """.stripMargin.format(lineRep.readName, toCompute)
      val postamble = "\n}"
      val generate = (m: MemberHandler) => m extraCodeToEvaluate Request.this
    }

    private object ResultObjectSourceCode extends CodeAssembler[MemberHandler] {

      val evalResult =
        handlers.last.definesTerm match {
          case Some(vname) =>
            "lazy val %s = %s".format(lineRep.resultName, fullPath(vname))
          case _  => ""
        }

      val preamble = """
                       |object %s {
                       |  %s
                       |  val %s: String = {
                       |    %s
                       |    (""
                     """.stripMargin.format(
        lineRep.evalName, evalResult, lineRep.printName,
        lineRep.readName
      )

      val postamble = """
                        |    )
                        |  }
                        |}
                      """.stripMargin
      val generate = (m: MemberHandler) => m resultExtractionCode Request.this
    }

    lazy val compile: Boolean = {
      lineRep.compile(ObjectSourceCode(handlers)) &&  {
            lineRep compile ResultObjectSourceCode(handlers)
      }
    }

    lazy val resultSymbol = lineRep.resolvePathToSymbol


    private def typeMap[T](f: Type => T) =
      mapFrom[Name, Name, T](termNames ++ typeNames)(x => f(cleanMemberDecl(resultSymbol, x)))

    /** String representations of same. */
    lazy val typeOf         = typeMap[String](tp => afterTyper(tp.toString))


    /** load and run the code using reflection */
    def loadAndRun: String = {
        ("" + (lineRep call sessionNames.print))
    }
  }

  def cleanMemberDecl(owner: Symbol, member: Name): Type = afterTyper {
      owner.info.nonPrivateDecl(member).tpe match {
        case NullaryMethodType(tp) => tp
        case tp                    => tp
      }
  }

  object exprTyper extends {
    val repl: IMain.this.type = imain
  } with ExprTyper { }

  def parse(line: String): Option[List[Tree]] = exprTyper.parse(line)

}

object IMain {


  trait CodeAssembler[T] {
    def preamble: String
    def generate: T => String
    def postamble: String

    def apply(contributors: List[T]): String = stringFromWriter { code =>
      code println preamble
      contributors map generate foreach (code println _)
      code println postamble
    }
  }
}