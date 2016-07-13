import scala.util.Random

object Generators{

  trait Generator2[+T]{
    self =>

    def generate: T

    def map[U](f:T=>U): Generator2[U] = new Generator2[U]{
      def generate = f (self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }

  }

  val booleans2 = for( x <- integers ) yield x > 0

  val lists = for( x <- integers ) yield  x match {
    case 0 =>
  }


  trait Generator[+T] {
    def generate : T
  }

  val integers = new Generator2[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }

  val booleans = new Generator2[Boolean] {
    def generate = integers.generate % 2 == 0
  }

  val pairs = new Generator2[(Int, Int)]{
    def generate = (integers.generate, integers.generate)
  }

  val someInt = for( a <- integers ) yield a


}