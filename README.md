# ncsu-csc-506-report-parser
A parser written in Scala that will parse reports from P2 and P3 projects in NCSU CSC 506.

This parser is intended to be used to parse reports from CSC506 Program assignment 2 and 3.

To run the application you will need to have SBT installed or to an IDE to run Scala.

### Using sbt

#### To run a Program 2 parser
```bash
> cd project/directory/
> sbt "run-main Main P2 src/test/resources/cacheoutputs.txt"
[info] Loading project definition from /home/zachncst/Programming/simulatorReportParser/project
[info] Set current project to simulatorReportParser (in build file:/home/zachncst/Programming/simulatorReportParser/)
[info] Running Main P2 src/test/resources/cacheoutputs.txt
[success] Total time: 0 s, completed Mar 31, 2015 12:19:43 AM
```

#### To run a Program 3 parser
```bash
> sbt "run-main Main P3 src/test/resources/fbv_8192_8_64.out"
[info] Loading project definition from /home/zachncst/Programming/simulatorReportParser/project
[info] Set current project to simulatorReportParser (in build file:/home/zachncst/Programming/simulatorReportParser/)
[info] Running Main P3 src/test/resources/fbv_8192_8_64.out
[success] Total time: 0 s, completed Mar 31, 2015 12:21:21 AM
```

***Note:*** Consecutive reports can be appended together in the same file and the parser will parse each one into a csv format. The CSV format is in such a way that reports can be distinguished by row. The csv format can then be easily imported into Excel.
