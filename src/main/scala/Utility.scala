import scala.collection.mutable.ArrayBuffer
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

  def loadTitanicExamples(filePath: String,
                          trainFraction: Double = 0.8): (List[Example], List[Example]) = {

    var examplesBuffer = ArrayBuffer[Example]() 
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
        examplesBuffer += Example(X, y = survived)
        index += 1
    }
    Random.setSeed(22L)
    val examples = Random.shuffle(examplesBuffer.toList)
    val numExamples = examples.length
    val numTrainExamples = (numExamples * trainFraction).toInt
    val trainExamples = examples.slice(0, numTrainExamples)
    val testExamples = examples.slice(numTrainExamples + 1, numExamples)
    (trainExamples, testExamples)
  }

}