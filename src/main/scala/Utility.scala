import scala.util.Random

object Utility {

  private val random = new Random

	def generateRandomBinaryExamples(numExamples: Int,
                                   numDimensions: Int,
                                   positiveFraction: Double,
                                   maxValue: Int = 10000): List[Example] = {
    (1 to numExamples).toList.map { i =>
      val X = (1 to numDimensions).toList.map(_ => random.nextDouble * random.nextInt(maxValue))
      val randDouble = random.nextDouble
      val y = if (randDouble <= positiveFraction) 1 else -1
      new Example(X, y)
    }
  }

}