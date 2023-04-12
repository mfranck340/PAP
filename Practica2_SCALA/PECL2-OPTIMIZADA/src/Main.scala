import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
object Main {
  def main(args: Array[String]): Unit = {
    val game = new Game()
    game.run()

    /*val myList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    // Conexión paralela en la lista
    val myParallelList = myList.par
    // Aplicar una operación en paralelo
    val result = myParallelList.map(x => x * 2)
    println(result)

    def sumaUno(n: Int): Int = {
      n + 1
    }

    // Creamos una lista
    val lista = List(1, 2, 3, 4, 5)

    // Utilizamos una función anónima dentro del método map
    val listaTransformada1 = lista.map(n => n * 2)

    // Utilizamos la función definida previamente dentro del método map
    val listaTransformada2 = lista.map(sumaUno)

    // Imprimimos las listas transformadas
    println(listaTransformada1) // Imprime: List(2, 4, 6, 8, 10)
    println(listaTransformada2) // Imprime: List(2, 3, 4, 5, 6)*/

  }
}