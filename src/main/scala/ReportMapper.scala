import scala.annotation.tailrec
import scala.util.matching.Regex
import scalaz._
import Scalaz._


/**
 * Created by zachncst on 3/3/15.
 */
object ReportMapper {
  import ReportType._

  val startNewCache = """^=====\s+.*""".r
  val l1_size = """L1_SIZE:\s+(\d+).*""".r
  val l1_assoc = """L1_ASSOC:\s+(\d+).*""".r
  val l1_blocksize = """L1_BLOCKSIZE:\s+(\d+).*""".r
  val l1_processor = """NUMBER OF PROCESSORS:\s+(\d+).*""".r
  val l1_protocol = """COHERENCE PROTOCOL:\s+([\w\s/]+).*""".r

  val cacheNum = """============ Simulation results \(Cache (\d).*""".r

  val row1 = """1\s[\.\w\s]+:\s+(\d+).*""".r
  val row2 = """2[\.\w\s]+:\s+(\d+).*""".r
  val row3 = """3[\.\w\s]+:\s+(\d+).*""".r
  val row4 = """4[\.\w\s]+:\s+(\d+).*""".r
  val row5 = """5[\.\w\s]+:\s+([\.\d]+)""".r
  val row6 = """6[\.\w\s]+:\s+(\d+).*""".r
  val row7 = """7[\.\w-\s]+:\s+(\d+).*""".r
  val row8 = """8[\.\w-\s]+:\s+(\d+).*""".r
  val row9 = """9[\.\w\s:]+\s+(\d+).*""".r
  val row10 = """10[\.\w\s:]+\s+(\d+).*""".r
  val row11 = """11[\.\w\s:]+\s+(\d+).*""".r
  val row12 = """12[\.\w\s]+:\s+(\d+).*""".r

  def getReports[Report](reportType: ReportType, strIter: Iterator[String]): List[Report] = {
    strIter.foldLeft[List[Report]](List[Report]())((addendum : List[Report], string) => {
      string match {
        case startNewCache() => {
          addendum.::(reportToModel(reportType, strIter)).asInstanceOf[List[Report]]
        }
        case _ => addendum
      }
    }).reverse
  }

  def reportToModel(reportType: ReportType, strIter: Iterator[String]): Report = {
    reportType match {
      case P2 => reportBuilder(strIter, new P2Report(new ReportDetails(), List(new CacheReport())))
      case P3 => reportBuilder(strIter, new P3Report(new ReportDetails(), List(new DirectoryReport())))
    }
  }

  @tailrec
  def reportBuilder(strIter: Iterator[String], addendum: P2Report): Report = {
    val str = strIter.next()
    str match {
      case l1_size(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(l1size = value.toInt)))
      case l1_assoc(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(l1assoc = value.toInt)))
      case l1_blocksize(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(l1blocksize = value.toInt)))
      case l1_processor(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(numberOfProcessors = value.toInt)))
      case l1_protocol(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(protocol = value)))
      case cacheNum(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(cacheNumber = value.toInt) :: addendum.reports.tail))
      }
      case row1(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(reads = value.toInt) :: addendum.reports.tail))
      }
      case row2(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(readMisses = value.toInt) :: addendum.reports.tail))
      }
      case row3(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(writes = value.toInt) :: addendum.reports.tail))
      }
      case row4(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(writeMisses = value.toInt) :: addendum.reports.tail))
      }
      case row6(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(writebacks = value.toInt) :: addendum.reports.tail))
      }
      case row5(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(missRate = value.toDouble) :: addendum.reports.tail))
      }
      case row7(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(memoryTransactions = value.toInt) :: addendum.reports.tail))
      }
      case row8(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(cacheToCacheTransfers = value.toInt) :: addendum.reports.tail))
      }
      case row9(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(busRds = value.toInt) :: addendum.reports.tail))
      }
      case row10(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(busWrs = value.toInt) :: addendum.reports.tail))
      }
      case row11(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(busRdXs = value.toInt) :: addendum.reports.tail))
      }
      case row12(value) => {
        //perform check if last cache
        addendum.reports.size match {
          case addendum.details.numberOfProcessors => addendum.copy(reports = (addendum.reports.head.copy(busUpgs = value.toInt) :: addendum.reports.tail).reverse)
          case _ => reportBuilder(strIter, addendum.copy(reports = new CacheReport() :: addendum.reports.head.copy(busUpgs = value.toInt) :: addendum.reports.tail))
        }
      }
      case _ => {
        reportBuilder(strIter, addendum)
      }
    }
  }

  @tailrec
  def reportBuilder(strIter: Iterator[String], addendum: P3Report): Report = {
    val str = strIter.next()
    str match {
      case l1_size(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(l1size = value.toInt)))
      case l1_assoc(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(l1assoc = value.toInt)))
      case l1_blocksize(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(l1blocksize = value.toInt)))
      case l1_processor(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(numberOfProcessors = value.toInt)))
      case l1_protocol(value) => reportBuilder(strIter, addendum.copy(details = addendum.details.copy(protocol = value)))
      case cacheNum(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(cacheNumber = value.toInt) :: addendum.reports.tail))
      }
      case row1(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(reads = value.toInt) :: addendum.reports.tail))
      }
      case row2(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(readMisses = value.toInt) :: addendum.reports.tail))
      }
      case row3(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(writes = value.toInt) :: addendum.reports.tail))
      }
      case row4(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(writeMisses = value.toInt) :: addendum.reports.tail))
      }
      case row6(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(writebacks = value.toInt) :: addendum.reports.tail))
      }
      case row5(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(missRate = value.toDouble) :: addendum.reports.tail))
      }
      case row7(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(cacheToCacheTransfers = value.toInt) :: addendum.reports.tail))
      }
      case row8(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(signalRds = value.toInt) :: addendum.reports.tail))
      }
      case row9(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(signalRdXs = value.toInt) :: addendum.reports.tail))
      }
      case row10(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(signalUpgs = value.toInt) :: addendum.reports.tail))
      }
      case row11(value) => {
        reportBuilder(strIter, addendum.copy(reports = addendum.reports.head.copy(invalidations = value.toInt) :: addendum.reports.tail))
      }
      case row12(value) => {
        //perform check if last cache
        addendum.reports.size match {
          case addendum.details.numberOfProcessors => {
            addendum.copy(reports = (addendum.reports.head.copy(interventions = value.toInt) :: addendum.reports.tail).reverse)
          }
          case _ => {
            reportBuilder(strIter,
              addendum.copy(reports = new DirectoryReport() :: addendum.reports.head.copy(interventions = value.toInt) :: addendum.reports.tail))
          }
        }
      }
      case _ => {
        reportBuilder(strIter, addendum)
      }
    }
  }

  val p2Header =
    s"""L1_SIZE,L1_ASSOC,L1_BLOCKSIZE,Processors,Protocol,Cache,Reads,Read Misses,
         |Writes,Write Misses,Total Miss Rate,Writebacks,Memory Transactions,Cache-to-Cache Transfers,
         |BusRds Issued,BusWrs Issued, BusRdXs Issued, BusUpgrs Issued""".stripMargin.replaceAll("\n", "") + "\n"

  val p3Header =
    s"""L1_SIZE,L1_ASSOC,L1_BLOCKSIZE,Processors,Protocol,Cache,Reads,Read Misses,
         |Writes,Write Misses,Total Miss Rate,Writebacks,Cache-to-Cache Transfers,
         |SignalRds Issued,SignalRdXs Issued, SignalUpgrs Issued, Invalidations, Interventions""".stripMargin.replaceAll("\n", "") + "\n"

  def modelsToCSV[Report](reportType: ReportType, reportIn: List[Report]): String = {
    reportType match {
      case P2 => {
        val p2Report = reportIn.asInstanceOf[List[P2Report]]
        p2Report.foldLeft(p2Header) {
          case (str, report: P2Report) => {
            str + modelToCSVP2(report)
          }
        }
      }
      case P3 => {
        val p3Report = reportIn.asInstanceOf[List[P3Report]]
        p3Report.foldLeft(p3Header) {
          case (str, report: P3Report) => {
            str + modelToCSVP3(report)
          }
        }
      }
    }
  }

  def modelToCSVP2(report: P2Report): String = {
    val details = report.details
    val reports = report.reports

    reports.foldLeft("")((str, cache) => {
      str + s"""${details.l1size},${details.l1assoc},${details.l1blocksize},${details.numberOfProcessors},${details.protocol},
         |${cache.cacheNumber},${cache.reads},${cache.readMisses},${cache.writes},${cache.writeMisses},${cache.missRate},${cache.writebacks},
         |${cache.memoryTransactions},${cache.cacheToCacheTransfers},${cache.busRds},${cache.busWrs},${cache.busRdXs},
         |${cache.busUpgs}""".stripMargin.replaceAll("\n", "") + "\n"
    })
  }

  def modelToCSVP3(report: P3Report): String = {
    val details = report.details
    val reports = report.reports

    reports.foldLeft("")((str, d) => {
      str + s"""${details.l1size},${details.l1assoc},${details.l1blocksize},${details.numberOfProcessors},${details.protocol},
         |${d.cacheNumber},${d.reads},${d.readMisses},${d.writes},${d.writeMisses},${d.missRate},${d.writebacks},
         |${d.cacheToCacheTransfers},${d.signalRds},${d.signalRdXs},${d.signalUpgs},
         |${d.invalidations},${d.interventions}""".stripMargin.replaceAll("\n", "") + "\n"
    })
  }
}

sealed trait Report

case class P2Report(details: ReportDetails, reports: List[CacheReport]) extends Report

case class P3Report(details: ReportDetails, reports: List[DirectoryReport]) extends Report

case class ReportDetails(l1size: Int = 0, l1assoc: Int = 0, l1blocksize: Int = 0, numberOfProcessors: Int = 0, protocol: String = "",
                         traceFile: String = "")

case class CacheReport(cacheNumber: Int = 0, reads: Int = 0, readMisses: Int = 0, writes: Int = 0, writeMisses: Int = 0,
                       missRate: Double = 0, writebacks: Int = 0, memoryTransactions: Int = 0, cacheToCacheTransfers: Int = 0,
                       busRds: Int = 0, busWrs: Int = 0, busRdXs: Int = 0, busUpgs: Int = 0)


case class DirectoryReport(cacheNumber: Int = 0, reads: Int = 0, readMisses: Int = 0, writes: Int = 0, writeMisses: Int = 0,
                           missRate: Double = 0, writebacks: Int = 0, cacheToCacheTransfers: Int = 0,
                           signalRds: Int = 0, signalRdXs: Int = 0, signalUpgs: Int = 0, invalidations: Int = 0,
                           interventions: Int = 0)

object ReportType extends Enumeration {
  type ReportType = Value
  val P2 = Value("P2")
  val P3 = Value("P3")

  def valueOf(s: String) = values.find(_.toString == s)
}