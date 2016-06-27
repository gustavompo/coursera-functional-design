import scala.util.Random

object ForGen{
  class IntGen{
    val rand = new Random()
    def map[U](f:Int=>U):U = {
      f(generate)
    }
    def generate:Int = rand.nextInt
  }

  val intGen = new IntGen
  val C = intGen.map(x=>x)

  lazy val b = (x:Int) => { println(x); 1 }
}