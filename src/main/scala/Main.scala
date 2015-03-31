import java.io.{PrintWriter, File}

import scala.io.Source

/**
 * Created by zachncst on 3/3/15.
 */
object Main extends App {
  import ReportType._

  val reportType = ReportType.valueOf( args.head )

  if( reportType.isDefined ) {
    args.tail.foreach(arg => {
      val file = new File(arg)
      val reports = ReportMapper.getReports(reportType.get, Source.fromFile(file).getLines())

      val writer = new PrintWriter(new File(s"${file.getName}_parsed.csv"))
      writer.write(ReportMapper.modelsToCSV(reportType.get, reports))
      writer.close()
    })
  } else {
    println( "Main reportType file1 file2...")
    println( "Report must be one of " + ReportType.values.mkString(","))
  }

}
