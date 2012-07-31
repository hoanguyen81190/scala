/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package interpreter

import Predef.{ println => _, _ }
import util.stringFromWriter
import scala.reflect.internal.util._
import io.VirtualDirectory
import reporters._
import IMain._
import java.lang.Class
import java.io.{InputStreamReader, BufferedReader, PrintWriter}

class IMain(initialSettings: Settings, protected val out: PrintWriter) {
  imain =>

  //constructor
  def this(settings: Settings) = this(settings,new PrintWriter(System.out, true))
  def this() = this(new Settings())
  def this(out: PrintWriter) = this(new Settings(), out)

  //load
  val virtualDirectory: VirtualDirectory = new VirtualDirectory("(memory)", None) // "directory" for classfiles

  private var _classLoader: AbstractFileClassLoader = null

  def classLoader: AbstractFileClassLoader = {
    if(_classLoader == null)
      _classLoader = makeClassLoader()
    _classLoader
  }

  private def makeClassLoader(): AbstractFileClassLoader =
    new AbstractFileClassLoader(virtualDirectory)

  //compile
  private val _compiler: Global = newCompiler(settings, reporter)

  private val nextReqId = {
    var counter = 0
    () => { counter += 1 ; counter }
  }

  def settings = initialSettings

  lazy val reporter: ConsoleReporter = new ConsoleReporter(this.settings, new BufferedReader(new InputStreamReader(System.in)), this.out)

  import reporter.{ printMessage}

  lazy val global: Global = {
      new _compiler.Run()
      _compiler
  }
  @deprecated("Use `global` for access to the compiler instance.", "2.9.0")
  lazy val compiler: global.type = global

  import global._

  protected def newCompiler(settings: Settings, reporter: Reporter): Global = {
    settings.outputDirs setSingleOutput virtualDirectory
    settings.exposeEmptyPackage.value = true
    new Global(settings, reporter)
  }

  def compileSourcesKeepingRun(sources: SourceFile*) = {
    val run = new Run()
    reporter.reset()
    run compileSources sources.toList
    !reporter.hasErrors
  }

  def compileSources(sources: SourceFile*): Boolean =
    compileSourcesKeepingRun(sources: _*)

  //parse
  object naming extends {
    val global: imain.global.type = imain.global
  } with Naming
  import naming._

  lazy val memberHandlers = new {
    val intp: imain.type = imain
  } with MemberHandlers
  import memberHandlers._

  private def requestFromLine(line: String): Request = {
    val trees = parse(line) match {
      case None         => return null
      case Some(Nil)    => return null
      case Some(trees)  => trees
    }
    new Request(line, trees)
  }

  object exprTyper extends {
    val repl: IMain.this.type = imain
  } with ExprTyper { }

  def parse(line: String): Option[List[Tree]] = exprTyper.parse(line)

  //interpret
  def interpret(line: String): Boolean = {
    if (global == null) false
    else {
      val req = requestFromLine(line)
      if (req == null || !req.compile) false
      else {
//          classLoader.setAsContext()
          val result = req.loadAndRun
          printMessage(result stripSuffix "\n")
          true
      }
    }
  }

  def close() {
    reporter.flush()
  }

//---------------------------------------------------------------------------------------------------------
  class Request(val line: String, val trees: List[Tree]) {
    val reqId = nextReqId()
    val lineRep = new ReadEvalPrint()

  //handlers
    val handlers: List[MemberHandler] = trees map (memberHandlers chooseHandler _)

  //map type
    def termNames = handlers flatMap (_.definesTerm)
    def typeNames = handlers flatMap (_.definesType)

    def fullPath(vname: String) = (lineRep.readPath + ".`%s`".format(vname))

    def fullPath(vname: Name): String = fullPath(vname.toString)
    lazy val resultSymbol = lineRep.resolvePathToSymbol

    private def typeMap[T](f: Type => T) =
      mapFrom[Name, Name, T](termNames ++ typeNames)(x => f(cleanMemberDecl(resultSymbol, x)))

    lazy val typeOf         = typeMap[String](tp => afterTyper(tp.toString))

    def cleanMemberDecl(owner: Symbol, member: Name): Type = afterTyper {
      owner.info.nonPrivateDecl(member).tpe match {
        case NullaryMethodType(tp) => tp
        case tp                    => tp
      }
    }

  //object to compile
    private object ObjectSourceCode extends CodeAssembler[MemberHandler] {
      val preamble = """
                       |object %s {
                       |%s
                     """.stripMargin.format(lineRep.readName, line)
      val postamble = "\n}"
      val generate = (m: MemberHandler) => m extraCodeToEvaluate Request.this
    }

    private object ResultObjectSourceCode extends CodeAssembler[MemberHandler] {
      val preamble = """
                       |object %s {
                       |  val %s: String = {
                       |    %s
                       |    (""
                     """.stripMargin.format(
        lineRep.evalName, lineRep.printName,
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

  //load
    def loadAndRun: String = {
      ("" + (lineRep call sessionNames.print))
    }
  }

//---------------------------------------------------------------------------------------------------------
  class ReadEvalPrint(lineId: Int) {
    def this() = this(freshLineId())

  //names
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

    def resolvePathToSymbol: Symbol = {
      def getModuleOrClass(path: Name, len: Int): Symbol = {
        val point = path lastPos('.', len - 1)
        val owner =
          if (point > 0) getModuleOrClass(path.toTermName, point)
          else RootClass        //global.definitions.RootClass
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

  //-----------------reflection-----------------
    def call(name: String, args: Any*): AnyRef = {
      val m = evalMethod(name)
      m.invoke(evalClass, args.map(_.asInstanceOf[AnyRef]): _*)
    }

    private def load(path: String): Class[_] = {
       Class.forName(path, true, classLoader)
    }

    lazy val evalClass = load(evalPath)

    private def evalMethod(name: String) = evalClass.getMethods filter (_.getName == name) match {
      case Array(method) => method
      case xs            => sys.error("Internal error: eval object " + evalClass + ", " + xs.mkString("\n", "\n", ""))
    }

  //-----------------reflection-----------------

    def compile(source: String): Boolean = compileSourcesKeepingRun(new BatchSourceFile("<console>", packaged(source)))
  }
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