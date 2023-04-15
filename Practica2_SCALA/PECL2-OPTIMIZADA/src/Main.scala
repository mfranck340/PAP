import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
object Main {
  def main(args: Array[String]): Unit = {
    val game = new Game()                   //Creamos el objeto game
    game.run()                              //Llamamos a run() para iniciar el juego
  }
}