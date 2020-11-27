/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

import perceptron.Perceptron
import votedperceptron.VotedPerceptron
import performance.BinaryPerformance
import utility.Utility

object Main extends App {

  // Dataset
  val titanicFilePath = "/Users/pjgalliani/Code/scalaml/Titanic.csv"
  val (trainExamples, testExamples) = Utility.loadTitanicExamples(filePath = titanicFilePath)
  val trainX = trainExamples.map(ex => ex.X)
  val testX = testExamples.map(ex => ex.X)
  val trainY = trainExamples.map(ex => ex.y)
  val testY = testExamples.map(ex => ex.y)

  println(
s"""=== Dataset ===
Dimension: ${trainExamples.head.X.length}
Train Examples: ${trainExamples.length}
  Positive Examples: ${trainY.filter(_ == 1).sum}
  Negative Examples: ${trainY.filter(_ == 1).sum - trainY.sum}
Test Examples: ${testExamples.length}
  Positive Examples: ${testY.filter(_ == 1).sum}
  Negative Examples: ${testY.filter(_ == 1).sum - testY.sum}
""")

  // Train perceptron.Perceptron
  val perceptron = new Perceptron(maxEpochs = 1000)
  perceptron.train(trainExamples)
  // Predict with perceptron.Perceptron
  val pTrainPredictions = perceptron.predictBatch(trainX)
  val pTestPredictions = perceptron.predictBatch(testX)
  // Compute Precision, Recall for train and test sets
  val pTrainMetrics = BinaryPerformance.computeMetrics(pTrainPredictions, trainY)
  val pTestMetrics = BinaryPerformance.computeMetrics(pTestPredictions, testY)
  println("=== perceptron.Perceptron ===")
  println("= Train =")
  println(pTrainMetrics.report)
  println("= Test =")
  println(pTestMetrics.report)

  // Train VotedPerceptron
  val votedPerceptron = new VotedPerceptron(maxEpochs = 5000)
  votedPerceptron.train(trainExamples)
  // Predict with VotedPerceptron
  val vpTrainPredictions = votedPerceptron.predictBatch(trainX)
  val vpTestPredictions = votedPerceptron.predictBatch(testX)
  // Compute Precision, Recall for train and test sets
  val vpTrainMetrics = BinaryPerformance.computeMetrics(vpTrainPredictions, trainY)
  val vpTestMetrics = BinaryPerformance.computeMetrics(vpTestPredictions, testY)
  println("=== Voted perceptron.Perceptron ===")
  println("= Train =")
  println(vpTrainMetrics.report)
  println("= Test =")
  println(vpTestMetrics.report)

}
