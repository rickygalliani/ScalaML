import Utility.generateRandomBinaryExamples
import BinaryPerformance.computeMetrics

object Main extends App {

  // Create training set, test set
  val Examples = 10000
  val TrainFraction = 0.8
  val NumTrainExamples = (Examples * 0.8).toInt
  val NumTestExamples = Examples - NumTrainExamples
  val Dimensions = 5
  val PositiveFraction = 0.6
  val trainExamples = generateRandomBinaryExamples(NumTrainExamples, Dimensions, PositiveFraction)
  val testExamples = generateRandomBinaryExamples(NumTestExamples, Dimensions, PositiveFraction)
  val trainX = trainExamples.map(ex => ex.X)
  val trainY = trainExamples.map(ex => ex.y)
  val testX = testExamples.map(ex => ex.X)
  val testY = testExamples.map(ex => ex.y)

  // Train Perceptron
  val perceptron = new Perceptron()
  perceptron.train(trainExamples)
  // Predict with Perceptron
  val pTrainPredictions = perceptron.predictBatch(trainX)
  val pTestPredictions = perceptron.predictBatch(testX)
  // Compute Precision, Recall for train and test sets
  val pTrainMetrics = BinaryPerformance.computeMetrics(pTrainPredictions, trainY)
  val pTestMetrics = BinaryPerformance.computeMetrics(pTestPredictions, testY)
  println("=== Perceptron ===")
  println("= Train =")
  println(pTrainMetrics.report)
  println("= Test =")
  println(pTestMetrics.report)

  // Train VotedPerceptron
  val votedPerceptron = new VotedPerceptron(maxEpochs = 100)
  votedPerceptron.train(trainExamples)
  // Predict with VotedPerceptron
  val vpTrainPredictions = votedPerceptron.predictBatch(trainX)
  val vpTestPredictions = perceptron.predictBatch(testX)
  // Compute Precision, Recall for train and test sets
  val vpTrainMetrics = BinaryPerformance.computeMetrics(vpTrainPredictions, trainY)
  val vpTestMetrics = BinaryPerformance.computeMetrics(vpTestPredictions, testY)
  println("=== Voted Perceptron ===")
  println("= Train =")
  println(vpTrainMetrics.report)
  println("= Test =")
  println(vpTestMetrics.report)

}
