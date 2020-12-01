/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data

import example.UnitBinaryClassificationExample
import example.BinaryClassificationExample

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Data {

  private val random = new Random
  random.setSeed(22L)

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

  def loadTitanicExamples(filePath: String, trainFraction: Double = 0.8):
  (List[BinaryClassificationExample], List[BinaryClassificationExample]) = {

    var examplesBuffer = ArrayBuffer[BinaryClassificationExample]()
    var index = 0
    val bufferedSource = io.Source.fromFile(filePath).getLines.drop(1)
    for (line <- bufferedSource) {  // Skip the header line
        val row = line.split(",").map(_.trim)
        val upperClass = row(0).toDouble
        val middleClass = row(1).toDouble
        val lowerClass = row(2).toDouble
        val gender = row(3).toDouble
        val age = row(4).toDouble
        val sibSp = row(5).toDouble
        val parch = row(6).toDouble
        val fare = row(7).toDouble
        val cherbourgEmbark = row(8).toDouble
        val queenstownEmbark = row(9).toDouble
        val southamptonEmbark = row(10).toDouble
        val survived = row(11).toInt
        val X = List(
          upperClass,
          middleClass,
          lowerClass,
          gender,
          age,
          sibSp,
          parch,
          fare,
          cherbourgEmbark,
          queenstownEmbark,
          southamptonEmbark
        )
        examplesBuffer += new BinaryClassificationExample(X, y = survived)
        index += 1
    }
    val examples = random.shuffle(examplesBuffer.toList)
    val numExamples = examples.length
    val numTrainExamples = (numExamples * trainFraction).toInt
    val trainExamples = examples.slice(0, numTrainExamples)
    val testExamples = examples.slice(numTrainExamples + 1, numExamples)
    (trainExamples, testExamples)
  }

}