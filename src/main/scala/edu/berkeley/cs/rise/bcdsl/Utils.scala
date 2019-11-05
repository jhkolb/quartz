package edu.berkeley.cs.rise.bcdsl

import java.io.{File, IOException, PrintWriter}

private[bcdsl] object Utils {
  def writeStringToFile(file: File, body: String): Unit = {
    val writer = new PrintWriter(file, "UTF-8")
    try {
      writer.print(body)
    } catch {
      case e: IOException => println(s"Failed to write to file: ${e.getMessage}")
    } finally {
      writer.close()
    }
  }

  def writeStringToFile(fileName: String, body: String): Unit =
    writeStringToFile(new File(fileName), body)
}
