package numberVision

object classifier {
  val sizex = 160 // width of digits (do not edit)
  val sizey = 160 // height of digits (do not edit)
  val m = sizex * sizey // length of digit array (=25600) (do not edit)

  def train(digits: Array[Array[Double]],
            labels: Array[Int]): (Array[Double]) => Int = {
    val features = digits.map(feature.get(_))

    def classifyDigit(digit: Array[Double]): Int = {
      // return the very best guess as to whether 'digit' is a 0 or 1
      val trainingData = features.zip(labels)
      //Find nearest neighbour from the data for the digit
      val input = feature.get(digit)

      def euclideanDistance(trainedData: Array[Double], dataToCompare: Array[Double]): Double = {
        def square(n: Double): Double = {
          n * n
        }

        //Read the dimensions
        val dimensions = dataToCompare.size
        //Under the square root
        var sum = 0.0
        for (n <- 0 until dimensions) {
          sum += square(trainedData(n) - dataToCompare(n))
        }
        val distance = math.sqrt(sum)
        distance
      }

      trainingData.map(i => (euclideanDistance(i._1, input), i._2)).minBy(_._1)._2
    }

    classifyDigit // return the classifier
  }
}

object feature {
  val h = 4
  val sizex = classifier.sizex/h
  val sizey = classifier.sizey/h
  val m = sizex*sizey

  def get(digit: Array[Double]) = {
    val down = new Array[Double](m)
    var i = 0
    while(i < m) {
      down(i) = digit((((i/(sizex))*sizex*h*h)+h*(i%(sizex))))
      i = i+1
    }
    val average = down.sum/m
    val step = down.map(v => if(v >= 0.8*average) { 0.0 } else { 1.0 })
    val total = step.sum
    val barx = math.round((0 until m).map(j => step(j)*(j%sizex)).sum/total).toInt
    val bary = math.round((0 until m).map(j => step(j)*(j/sizex)).sum/total).toInt
    val vec = new Array[Double](m)
    i = 0
    while(i < sizey) {
      var j = 0
      val bi = (i+bary+sizey+sizey/2)%sizey
      while(j < sizex) {
        val bj = (j+barx+sizex+sizex/2)%sizex
        vec(i*sizex+j) = 1.0-step(bi*sizex+bj)
        j = j+1
      }
      i = i+1
    }
    vec
  }
}
