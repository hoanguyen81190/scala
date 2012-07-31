/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 */

package scala.tools.nsc
package interpreter

import scala.tools.nsc.io.{ AbstractFile }
import util.ScalaClassLoader

/**
 * A class loader that loads files from a {@link scala.tools.nsc.io.AbstractFile}.
 *
 * @author Lex Spoon
 */
//using AssemblyClassLoader in ikvm?

class AbstractFileClassLoader(val root: AbstractFile)
  extends ClassLoader
  with ScalaClassLoader
{
  def this(root: AbstractFile, par: java.lang.ClassLoader) = this(root)
  protected def classNameToPath(name: String): String =
    if (name endsWith ".class") name
    else name.replace('.', '/') + ".class"

  protected def findAbstractFile(name: String): AbstractFile = {
    var file: AbstractFile = root
    val pathParts          = classNameToPath(name) split '/'

    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, true)
      if (file == null)
        return null
    }

    file.lookupName(pathParts.last, false) match {
      case null   => null
      case file   => file
    }
  }

  override def classBytes(name: String): Array[Byte] = findAbstractFile(name) match {
    case null => super.classBytes(name)
    case file => file.toByteArray
  }
  override def findClass(name: String): java.lang.Class[_] = {
    val bytes = classBytes(name)
    if (bytes.length == 0)
      throw new ClassNotFoundException(name)
    else
      defineClass(name, bytes, 0, bytes.length)
  }


}
