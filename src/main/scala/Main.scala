/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

import Config.LogLevelSeed
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.scala.Logging
import model.perceptron.Perceptron
import performance.BinaryPerformance
import data.Data
import model.logisticregression.LogisticRegression
import model.votedperceptron.VotedPerceptron

object Main extends App with Logging {

  val main: Level = Level.forName("main", LogLevelSeed)

  // Dataset
  val titanicFilePath = "/Users/pjgalliani/Code/scalaml/Titanic.csv"
  val (trainExamples, testExamples) = Data.loadTitanicExamples(filePath = titanicFilePath)
  val trainX = trainExamples.map(ex => ex.X)
  val testX = testExamples.map(ex => ex.X)
  val trainY = trainExamples.map(ex => ex.y)
  val testY = testExamples.map(ex => ex.y)

  logger(main,
s"""
=== Dataset ===
Features: ${trainExamples.head.X.length}
Train Examples: ${trainExamples.length}
  Positive Examples: ${trainY.filter(_ == 1).sum.toInt}
  Negative Examples: ${(trainY.filter(_ == 1).sum - trainY.sum.toInt)}
Test Examples: ${testExamples.length}
  Positive Examples: ${testY.filter(_ == 1).sum.toInt}
  Negative Examples: ${(testY.filter(_ == 1).sum - testY.sum).toInt}
""")

//  // Train Perceptron
//  val perceptron = new Perceptron()
//  perceptron.train(trainExamples)
//  val pTrainPredictions = perceptron.predictBatch(trainX)
//  val pTestPredictions = perceptron.predictBatch(testX)
//  val pTrainMetrics = BinaryPerformance.computeMetrics(pTrainPredictions, trainY, negativeClass = -1)
//  val pTestMetrics = BinaryPerformance.computeMetrics(pTestPredictions, testY, negativeClass = -1)
//  logger(main, s"""\n=== Perceptron ===\n= Train =\n${pTrainMetrics.report}\n= Test =\n${pTestMetrics.report}""")
//
//  // Train VotedPerceptron
//  val votedPerceptron = new VotedPerceptron()
//  votedPerceptron.train(trainExamples)
//  val vpTrainPredictions = votedPerceptron.predictBatch(trainX)
//  val vpTestPredictions = votedPerceptron.predictBatch(testX)
//  val vpTrainMetrics = BinaryPerformance.computeMetrics(vpTrainPredictions, trainY, negativeClass = -1)
//  val vpTestMetrics = BinaryPerformance.computeMetrics(vpTestPredictions, testY, negativeClass = -1)
//  logger(main, s"""\n=== VotedPerceptron ===\n= Train =\n${vpTrainMetrics.report}\n= Test =\n${vpTestMetrics.report}""")

  // Train LogisticRegression
  val logisticRegression = new LogisticRegression()
  logisticRegression.train(trainExamples)
  val lrTrainPredictions = logisticRegression.predictBatch(trainX).map(p => if (p > 0.6) 1.0 else 0.0)
  val lrTestPredictions = logisticRegression.predictBatch(testX).map(p => if (p > 0.6) 1.0 else 0.0)
  val lrTrainMetrics = BinaryPerformance.computeMetrics(lrTrainPredictions, trainY)
  val lrTestMetrics = BinaryPerformance.computeMetrics(lrTestPredictions, testY)
  logger(
    main, s"""\n=== LogisticRegression ===\n= Train =\n${lrTrainMetrics.report}\n= Test =\n${lrTestMetrics.report}"""
  )

}
