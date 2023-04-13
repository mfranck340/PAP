import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
object Main {
  def main(args: Array[String]): Unit = {
    val game = new Game()
    game.run()

    /*val tablero = List(
      1, 2, 1, 4, 1, 6, 1, 2, 3, 1,
      1, 2, 3, 4, 5, 6, 1, 2, 3, 5,
      1, 2, 1, 4, 1, 6, 1, 2, 3, 1,
      1, 2, 1, 4, 1, 6, 1, 2, 3, 4,
      1, 2, 1, 1, 1, 6, 1, 2, 3, 1,
      1, 2, 1, 1, 1, 2, 1, 2, 3, 4,
      1, 1, 1, 1, 1, 1, 1, 2, 3, 4,
      1, 1, 1, 1, 1, 1, 1, 2, 3, 4,
      1, 1, 1, 1, 1, 1, 1, 2, 3, 4,
      1, 1, 1, 1, 1, 1, 1, 2, 3, 1)

    val tab = new Tablero

    val inicio = System.currentTimeMillis()
    tab.mostrarTablero(tab.eliminarFichas2(tablero, 22, 10, 10, 1), 10)
    val fin = System.currentTimeMillis()
    println(s"Tiempo de ejecución: ${fin - inicio} ms")


    val inicio2 = System.currentTimeMillis()
    tab.mostrarTablero(tab.eliminarFichas(tablero, 1, 99, 22, 10), 10)
    val fin2 = System.currentTimeMillis()
    println(s"Tiempo de ejecución: ${fin2 - inicio2} ms")*/



  }
}