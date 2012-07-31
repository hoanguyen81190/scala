/** Testing compact tree printers.
 */
object Test {
  import scala.tools.nsc._
  import interpreter._
  import java.io.{ OutputStream, BufferedReader, StringReader, PrintWriter, Writer, OutputStreamWriter}

  val code = """
    def foo = {
      var q: Boolean = false
      val x = if (true) {
        if (true) {
          if (true) {
            5
          }
          else if (true) {
            5
          } else {
            10
          }
        }
        else 20
      }
      else 30

      (x == 5) || !q || true
    }
  """

  class NullOutputStream extends OutputStream { def write(b: Int) { } }


}
