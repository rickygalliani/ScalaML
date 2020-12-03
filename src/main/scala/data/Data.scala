/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data

import example.{BinaryClassificationExample, Example}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Data {

  private val random = new Random
  random.setSeed(DataSeed)

	def generateRandomBinaryExamples(numExamples: Int,
                                   numDimensions: Int,
                                   positiveFraction: Double,
                                   maxValue: Int): List[BinaryClassificationExample] = {
    (1 to numExamples).toList.map { _ =>
      val X = (1 to numDimensions).toList.map(_ => random.nextDouble * random.nextInt(maxValue))
      val randDouble = random.nextDouble
      val y = if (randDouble <= positiveFraction) 1 else -1
      new BinaryClassificationExample(X, y)
    }
  }

  /**
   * Loads a list of training examples from the local machine
   *
   * @param filePath absolute file path to csv data file
   * @param delimiter string separating columns (default: ',')
   * @return
   */
  def loadCSVExamples(filePath: String, delimiter: String = ","): List[Example] = {
    var examplesBuffer = ArrayBuffer[Example]()
    var index = 0
    val bufferedSource = io.Source.fromFile(filePath)
    for (line <- bufferedSource.getLines.drop(1)) {  // Skip the header line
      val row = line.split(delimiter).map(_.trim).map(_.toDouble).toList
      examplesBuffer += new Example(X = row.dropRight(1), y = row.last)
      index += 1
    }
    bufferedSource.close
    examplesBuffer.toList
  }

}