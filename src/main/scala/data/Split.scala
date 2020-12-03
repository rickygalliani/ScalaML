/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data

import example.Example

import scala.util.Random

object Split {

  private val random = new Random
  random.setSeed(SplitSeed)

  def trainTestSplit(examples: List[Example], trainFraction: Double = 0.8): (List[Example], List[Example]) = {
    random.shuffle(examples)
    val numExamples = examples.size
    val numTrainExamples = (numExamples * trainFraction).toInt
    val trainExamples = examples.slice(0, numTrainExamples)
    val testExamples = examples.slice(numTrainExamples + 1, numExamples)
    (trainExamples, testExamples)
  }

}
