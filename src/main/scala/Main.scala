import Utility.generateRandomBinaryExamples
import Performance.{computePrecision, computeRecall}

object Main extends App {

  // Create training set, test set
  val Examples = 1000
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
  val pTrainPrecision = Performance.computePrecision(pTrainPredictions, trainY)
  val pTrainRecall = Performance.computeRecall(pTrainPredictions, trainY)
  val pTestPrecision = Performance.computePrecision(pTestPredictions, testY)
  val pTestRecall = Performance.computeRecall(pTestPredictions, testY)
  println("== Perceptron ==")
  println(s"Train Precision: $pTrainPrecision")
  println(s"Test Precision: $pTestPrecision\n")
  println(s"Train Recall: $pTrainRecall")
  println(s"Test Recall: $pTestRecall\n")

  // Train VotedPerceptron
  val votedPerceptron = new VotedPerceptron()
  votedPerceptron.train(trainExamples)
  // Predict with VotedPerceptron
  val vpTrainPredictions = votedPerceptron.predictBatch(trainX)
  val vpTestPredictions = perceptron.predictBatch(testX)
  // Compute Precision, Recall for train and test sets
  val vpTrainPrecision = Performance.computePrecision(vpTrainPredictions, trainY)
  val vpTrainRecall = Performance.computeRecall(vpTrainPredictions, trainY)
  val vpTestPrecision = Performance.computePrecision(vpTestPredictions, testY)
  val vpTestRecall = Performance.computeRecall(vpTestPredictions, testY)
  println("== Voted Perceptron ==")
  println(s"Train Precision: $vpTrainPrecision")
  println(s"Test Precision: $vpTestPrecision\n")
  println(s"Train Recall: $vpTrainRecall")
  println(s"Test Recall: $vpTestRecall\n")

}
