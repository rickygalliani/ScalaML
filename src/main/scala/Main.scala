import Utility._

object Main extends App {

  // Create training set, test set
  val Examples = 1000
  val TrainFraction = 0.8
  val TrainExamples = (Examples * 0.8).toInt
  val TestExamples = Examples - TrainExamples
  val Dimensions = 5
  val PositiveFraction = 0.6
  val trainExamples = generateRandomBinaryExamples(TrainExamples, Dimensions, PositiveFraction)
  val testExamples = generateRandomBinaryExamples(TestExamples, Dimensions, PositiveFraction)

  // Train Perceptron
  val perceptron = new Perceptron()
  perceptron.train(trainExamples)
  val perceptronTrainPrecision = perceptron.computePrecision(trainExamples)
  val perceptronTrainRecall = perceptron.computeRecall(trainExamples)
  val perceptronTestPrecision = perceptron.computePrecision(testExamples)
  val perceptronTestRecall = perceptron.computeRecall(testExamples)
  println("== Perceptron ==")
  println(s"Train Precision: $perceptronTrainPrecision")
  println(s"Test Precision: $perceptronTestPrecision")
  println(s"Train Recall: $perceptronTrainRecall")
  println(s"Test Recall: $perceptronTestRecall")

  // Train VotedPerceptron
  val votedPerceptron = new VotedPerceptron()
  votedPerceptron.train(trainExamples)
  val votedPerceptronTrainPrecision = votedPerceptron.computePrecision(trainExamples)
  val votedPerceptronTrainRecall = votedPerceptron.computeRecall(trainExamples)
  val votedPerceptronTestPrecision = votedPerceptron.computePrecision(testExamples)
  val votedPerceptronTestRecall = votedPerceptron.computeRecall(testExamples)
  println("== Voted Perceptron ==")
  println(s"Train Precision: $votedPerceptronTrainPrecision")
  println(s"Test Precision: $votedPerceptronTestPrecision")
  println(s"Train Recall: $votedPerceptronTrainRecall")
  println(s"Test Recall: $votedPerceptronTestRecall")

  // Measure ROC and PR

}
