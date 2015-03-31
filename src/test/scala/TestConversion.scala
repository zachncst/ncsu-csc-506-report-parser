import java.io.File

import org.scalatest._

import scala.io.Source

/**
 * Created by zachncst on 3/3/15.
 */
class TestConversion extends FlatSpec with Matchers {
  import ReportType._

  "A p2 test file" should "create two cache reports" in {
    val url = this.getClass().getClassLoader.getResource("cacheoutputs.txt")
    val file = new File(url.toURI)

    val reports = ReportMapper.getReports(P2, Source.fromFile(file).getLines())

    reports.size should be equals (2)

    println( ReportMapper.modelsToCSV(P2, reports))
  }

  "A p3 file" should "create two directory reports" in {
    val url = this.getClass().getClassLoader.getResource("fbv_8192_8_64.out")
    val file = new File(url.toURI)

    val reports = ReportMapper.getReports(P3, Source.fromFile(file).getLines())

    reports.size should be equals (2)

    println( ReportMapper.modelsToCSV(P3, reports))
  }
}
