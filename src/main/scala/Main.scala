/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

import Config.LogLevelSeed
import data.Data.loadCSVExamples
import data.Split.trainTestSplit
import model.classification.decisiontree.DecisionTreeClassifier
import model.classification.decisiontree.logisticregression.LogisticRegression
import model.classification.perceptron.Perceptron
import model.classification.votedperceptron.VotedPerceptron
import org.apache.logging.log4j.Level
import org.apache.logging.log4j.scala.Logging
import performance.BinaryPerformance.computeMetrics

object Main extends App with Logging {

  val main: Level = Level.forName("main", LogLevelSeed)

  val datasets = Seq(
//    "/Users/pjgalliani/Code/scalaml/Titanic.csv",
    "/Users/pjgalliani/Code/scalaml/NBA5YearNoName.csv",
  )
  
  datasets.foreach { filePath =>
    // Dataset
    val examples = loadCSVExamples(filePath)
    val (trainExamples, testExamples) = trainTestSplit(examples)
    val trainX = trainExamples.map(ex => ex.X)
    val testX = testExamples.map(ex => ex.X)
    val trainY = trainExamples.map(ex => ex.y)
    val testY = testExamples.map(ex => ex.y)

    logger(main,
      s"""
=== Dataset ($filePath) ===
Features: ${trainExamples.head.X.length}
Train Examples: ${trainExamples.length}
  Positive Examples: ${trainY.filter(_ == 1).sum.toInt}
  Negative Examples: ${(trainY.size - trainY.filter(_ == 1).sum).toInt}
Test Examples: ${testExamples.length}
  Positive Examples: ${testY.filter(_ == 1).sum.toInt}
  Negative Examples: ${(testY.size - testY.filter(_ == 1).sum).toInt}
""")

//    // Perceptron
//    val perceptron = new Perceptron(verbose = true)
//    perceptron.train(trainExamples)
//    val pTrPredictions = perceptron.predictBatch(trainX)
//    val pTsPredictions = perceptron.predictBatch(testX)
//    val pTrainMetrics = computeMetrics(pTrPredictions, trainY, negativeClassPrediction = -1)
//    val pTestMetrics = computeMetrics(pTsPredictions, testY, negativeClassPrediction = -1)
//    logger(
//      main, s"""\n=== Perceptron ===\n= Train =\n${pTrainMetrics.report}\n= Test =\n${pTestMetrics.report}"""
//    )
//
//    // VotedPerceptron
//    val votedPerceptron = new VotedPerceptron(verbose = true)
//    votedPerceptron.train(trainExamples)
//    val vpTrPredictions = votedPerceptron.predictBatch(trainX)
//    val vpTsPredictions = votedPerceptron.predictBatch(testX)
//    val vpTrainMetrics = computeMetrics(vpTrPredictions, trainY, negativeClassPrediction = -1)
//    val vpTestMetrics = computeMetrics(vpTsPredictions, testY, negativeClassPrediction = -1)
//    logger(
//      main, s"""\n=== VotedPerceptron ===\n= Train =\n${vpTrainMetrics.report}\n= Test =\n${vpTestMetrics.report}"""
//    )
//
//    // LogisticRegression
//    val logisticRegression = new LogisticRegression(verbose = true)
//    logisticRegression.train(trainExamples)
//    val lrTrPredictions = logisticRegression.predictBatch(trainX).map(p => if (p > 0.6) 1.0 else 0.0)
//    val lrTsPredictions = logisticRegression.predictBatch(testX).map(p => if (p > 0.6) 1.0 else 0.0)
//    val lrTrainMetrics = computeMetrics(lrTrPredictions, trainY)
//    val lrTestMetrics = computeMetrics(lrTsPredictions, testY)
//    logger(
//      main, s"""\n=== LogisticRegression ===\n= Train =\n${lrTrainMetrics.report}\n= Test =\n${lrTestMetrics.report}"""
//    )

    // DecisionTreeClassifier
    val decisionTreeClassifier = new DecisionTreeClassifier(verbose = true)
    decisionTreeClassifier.train(trainExamples)
    val dtcTrPredictions = decisionTreeClassifier.predictBatch(trainX).map(p => if (p > 0.6) 1.0 else 0.0)
    val dtcTsPredictions = decisionTreeClassifier.predictBatch(testX).map(p => if (p > 0.6) 1.0 else 0.0)
    val dtcTrainMetrics = computeMetrics(dtcTrPredictions, trainY)
    val dtcTestMetrics = computeMetrics(dtcTsPredictions, testY)
    logger(
      main,
      s"""
         |\n=== DecisionTreeClassifier ===\n= Train =\n${dtcTrainMetrics.report}\n= Test =\n${dtcTestMetrics.report}
         |""".stripMargin
    )

  }

}
