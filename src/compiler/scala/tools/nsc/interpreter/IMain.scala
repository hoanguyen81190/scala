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
import scala.tools.nsc.io.AbstractFile
import reporters._
import scala.tools.util.PathResolver
import scala.tools.nsc.util.ScalaClassLoader
import ScalaClassLoader.URLClassLoader
import scala.collection.{ mutable }
import IMain._
import java.util.concurrent.Future
import java.io.PrintWriter


/** directory to save .class files to */
private class ReplVirtualDirectory(out: JPrintWriter) extends VirtualDirectory("(memory)", None) {
  private def pp(root: AbstractFile, indentLevel: Int) {
    val spaces = "    " * indentLevel
    out.println(spaces + root.name)
    if (root.isDirectory)
      root.toList sortBy (_.name) foreach (x => pp(x, indentLevel + 1))
  }
  // print the contents hierarchically
  def show() = pp(this, 0)
}


class IMain(initialSettings: Settings, protected val out: JPrintWriter) {
  imain =>

  /** Leading with the eagerly evaluated.
   */
  val virtualDirectory: VirtualDirectory            = new ReplVirtualDirectory(out) // "directory" for classfiles
  private var currentSettings: Settings             = initialSettings
  private var _initializeComplete                   = false     // compiler is initialized
  private var _isInitialized: Future[Boolean]       = null      // set up initialization future

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
  def settings = currentSettings
  def savingSettings[T](fn: Settings => Unit)(body: => T): T = {
    val saved = currentSettings
    currentSettings = saved.copy()
    fn(currentSettings)
    try body
    finally currentSettings = saved
  }

  def this(settings: Settings) = this(settings, new NewLinePrintWriter(new ConsoleWriter, true))
  def this() = this(new Settings())

  lazy val formatting: Formatting = new Formatting {
    val prompt = Properties.shellPromptString
  }
  lazy val reporter: ConsoleReporter = new ConsoleReporter(this.settings, Console.in, this.out)
  import formatting._
  import reporter.{ printMessage}

  private def _initSources = List(new BatchSourceFile("<init>", "class $repl_$init { }"))
  private def _initialize() = {
    try {
      // [Eugene] todo. if this crashes, REPL will hang
      new _compiler.Run() compileSources _initSources
      _initializeComplete = true
      true
    }
    catch AbstractOrMissingHandler()
  }
  private def tquoted(s: String) = "\"\"\"" + s + "\"\"\""

  // argument is a thunk to execute after init is done
  def initialize(postInitSignal: => Unit) {
    synchronized {
      if (_isInitialized == null) {
        _isInitialized = io.spawn {
          try _initialize()
          finally postInitSignal
        }
      }
    }
  }

  def isInitializeComplete = _initializeComplete

  /** the public, go through the future compiler */
  lazy val global: Global = {
    if (isInitializeComplete) _compiler
    else {
      // If init hasn't been called yet you're on your own.
      if (_isInitialized == null) {

        initialize(())
      }
      // blocks until it is ; false means catastrophic failure
      if (_isInitialized.get()) _compiler
      else null
    }
  }
  @deprecated("Use `global` for access to the compiler instance.", "2.9.0")
  lazy val compiler: global.type = global

  import global._
  import definitions.{termMember}

  implicit class ReplTypeOps(tp: Type) {
    def orElse(other: => Type): Type    = if (tp ne NoType) tp else other
    def andAlso(fn: Type => Type): Type = if (tp eq NoType) tp else fn(tp)
  }

  // TODO: If we try to make naming a lazy val, we run into big time
  // scalac unhappiness with what look like cycles.  It has not been easy to
  // reduce, but name resolution clearly takes different paths.
  object naming extends {
    val global: imain.global.type = imain.global
  } with Naming {
    // make sure we don't overwrite their unwisely named res3 etc.
    def freshUserTermName(): TermName = {
      val name = newTermName(freshUserVarName())
      if (definedNameMap contains name) freshUserTermName()
      else name
    }
    def isUserTermName(name: Name) = isUserVarName("" + name)
    def isInternalTermName(name: Name) = isInternalVarName("" + name)
  }
  import naming._

  lazy val memberHandlers = new {
    val intp: imain.type = imain
  } with MemberHandlers
  import memberHandlers._

  /** interpreter settings */
  lazy val isettings = new ISettings(this)

  /** Instantiate a compiler.  Overridable. */
  protected def newCompiler(settings: Settings, reporter: Reporter): ReplGlobal = {
    settings.outputDirs setSingleOutput virtualDirectory
    settings.exposeEmptyPackage.value = true
    new Global(settings, reporter) with ReplGlobal
  }

  /** Parent classloader.  Overridable. */
  protected def parentClassLoader: ClassLoader =
    settings.explicitParentLoader.getOrElse( this.getClass.getClassLoader() )

  def classLoader: AbstractFileClassLoader = {
    _classLoader = makeClassLoader()
    _classLoader
  }
  private class TranslatingClassLoader(parent: ClassLoader) extends AbstractFileClassLoader(virtualDirectory, parent) {
  }
  private def makeClassLoader(): AbstractFileClassLoader =
    new TranslatingClassLoader(parentClassLoader match {
      case null   => ScalaClassLoader fromURLs compilerClasspath
      case p      => new URLClassLoader(compilerClasspath, p)
    })

  def allDefinedNames = definedNameMap.keys.toList.sorted
  def pathToType(id: String): String = pathToName(newTypeName(id))
  def pathToTerm(id: String): String = pathToName(newTermName(id))
  def pathToName(name: Name): String = {
    if (definedNameMap contains name)
      definedNameMap(name) fullPath name
    else name.toString
  }

  def compileSourcesKeepingRun(sources: SourceFile*) = {
    val run = new Run()
    reporter.reset()
    run compileSources sources.toList
    (!reporter.hasErrors, run)
  }

  def compileSources(sources: SourceFile*): Boolean =
    compileSourcesKeepingRun(sources: _*)._1

  def compileString(code: String): Boolean =
    compileSources(new BatchSourceFile("<script>", code))

  private def requestFromLine(line: String): Request = {
    val content = indentCode(line)
    val trees = parse(content) match {
      case None         => return null
      case Some(Nil)    => return null
      case Some(trees)  => trees
    }
    new Request(line, trees)
  }

  // normalize non-public types so we don't see protected aliases like Self
  def normalizeNonPublic(tp: Type) = tp match {
    case TypeRef(_, sym, _) if sym.isAliasType && !sym.isPublic => tp.normalize
    case _                                                      => tp
  }


  def interpret(line: String): IR.Result = {
    def loadAndRunReq(req: Request) = {
      classLoader.setAsContext()
      val result = req.loadAndRun
          printMessage(result stripSuffix "\n")
        IR.Success

    }

    if (global == null) IR.Error
    else {
      val req = requestFromLine(line)
        if (req == null || !req.compile) IR.Error
        else loadAndRunReq(req)
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

    /** handlers for each tree in this request */
    val handlers: List[MemberHandler] = trees map (memberHandlers chooseHandler _)
    def defHandlers = handlers collect { case x: MemberDefHandler => x }

    /** all (public) names defined by these statements */
    val definedNames = handlers flatMap (_.definedNames)

    /** list of names used by this expression */
    val referencedNames: List[Name] = handlers flatMap (_.referencedNames)

    /** def and val names */
    def termNames = handlers flatMap (_.definesTerm)
    def typeNames = handlers flatMap (_.definesType)


    def fullPath(vname: String) = (
      lineRep.readPath + ".`%s`".format(vname)
      )

    def fullFlatName(name: String) =
      lineRep.readPath + nme.NAME_JOIN_STRING + name


    /** Code to access a variable with the specified name */
    def fullPath(vname: Name): String = fullPath(vname.toString)

    /** the line of code to compute */
    def toCompute = line
    def lookupTypeOf(name: Name) = typeOf.getOrElse(name, typeOf(global.encode(name.toString)))
    /** generate the source code for the object that computes this request */
    private object ObjectSourceCode extends CodeAssembler[MemberHandler] {
      def path = pathToTerm("$intp")
      def envLines = {
        if (!isReplPower) Nil // power mode only for now
        // $intp is not bound; punt, but include the line.
        else if (path == "$intp") List(
          "def $line = " + tquoted(originalLine),
          "def $trees = Nil"
        )
        else List(
          "def $line  = " + tquoted(originalLine),
          "def $req = %s.requestForReqId(%s).orNull".format(path, reqId),
          "def $trees = if ($req eq null) Nil else $req.trees".format(lineRep.readName, path, reqId)
        )
      }

      val preamble = """
                       |object %s {
                       |%s%s
                     """.stripMargin.format(lineRep.readName, envLines.map("  " + _ + ";\n").mkString, indentCode(toCompute))
      val postamble = "\n}"
      val generate = (m: MemberHandler) => m extraCodeToEvaluate Request.this
    }

    private object ResultObjectSourceCode extends CodeAssembler[MemberHandler] {

      val evalResult =
        if (!handlers.last.definesValue) ""
        else handlers.last.definesTerm match {
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
    normalizeNonPublic {
      owner.info.nonPrivateDecl(member).tpe match {
        case NullaryMethodType(tp) => tp
        case tp                    => tp
      }
    }
  }

  object exprTyper extends {
    val repl: IMain.this.type = imain
  } with ExprTyper { }

  def parse(line: String): Option[List[Tree]] = exprTyper.parse(line)

  private val prevRequests       = mutable.ListBuffer[Request]()
  private val definedNameMap     = mutable.Map[Name, Request]()

  def prevRequestList     = prevRequests.toList
}

object IMain {
  private def removeLineWrapper(s: String) = s.replaceAll("""\$line\d+[./]\$(read|eval|print)[$.]""", "")
  private def removeIWPackages(s: String)  = s.replaceAll("""\$(iw|read|eval|print)[$.]""", "")
  def stripString(s: String)               = removeIWPackages(removeLineWrapper(s))

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