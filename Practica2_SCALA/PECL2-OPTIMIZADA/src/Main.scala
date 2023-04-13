import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
object Main {
  def main(args: Array[String]): Unit = {
    val game = new Game()
    game.run()
  }
}