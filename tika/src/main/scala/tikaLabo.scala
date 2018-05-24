import java.io.{File, FileInputStream}
import java.nio.file.{FileSystems, Files}

import org.apache.tika.Tika
import org.apache.tika.fork.ForkParser
import org.apache.tika.metadata.{Metadata, TikaMetadataKeys}
import org.apache.tika.parser.{AutoDetectParser, ParseContext}
import org.apache.tika.sax.{BodyContentHandler, ToTextContentHandler}

object tikaLabo extends App {
  val filename = "/Volumes/STOCK/Dropbox/encours2015/20141207/bug_tika.doc"
  //val filename = "/Volumes/STOCK/Dropbox/encours2015/20141207/README.doc"
  //val filename = "/Volumes/STOCK/Dropbox/encours2015/20141207/simple.txt"
  //val filename = "/Volumes/STOCK/Dropbox/encours2015/20141207/SEPA.pdf"

  val file     = new File(filename)
  val fileIn   = new FileInputStream(file)
  val mimeType = new Tika().detect(file)

  println(s"start parsing $file of type $mimeType")

  val parser = new ForkParser()
  //val parser = new AutoDetectParser()
  print(parser.toString)
  val handler              = new ToTextContentHandler()
  private val parseContext = new ParseContext()
  val meta                 = new Metadata()
  val handler2             = new BodyContentHandler()
  //meta.set(HttpHeaders.CONTENT_TYPE, ct)
  meta.set(TikaMetadataKeys.RESOURCE_NAME_KEY, file.getName)

  try {
    parser.parse(fileIn, handler, meta, parseContext)
  } catch {
    case e: Throwable ⇒
      println(s"Cannot extract metadata and content for xxx ${e.getMessage}")
  }
  val content = handler.toString() // .trim()
  println(content)
  println(meta)

}
