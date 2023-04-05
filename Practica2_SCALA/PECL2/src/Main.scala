object Main {

  val rand = new scala.util.Random(System.currentTimeMillis())

  def main(args: Array[String]) {

    println("\n- CUNDY CROSH SOGA -\n")
    println("Aqui se va a llamar a la clase game")
    println("En game va a estar toda la logica")
    println("Aqui solo vamos a inicializar game")
    println("De momento estaba haciendo pruebas")

    //A lo mejor aqui se pregunta por las variables al usuario
    //Para despues pasarselas a game
    //Pero creo que lo suyo ser hacerlo en game

    val m = List(
      1, 1, 1, 1, 1, 23, 20, 25,

      1, 1, 98, 8, 42, 55, 10, 21,

      44, 1, 98, 10, 82, 94, 89, 7,

      92, 1, 1, 1, 0, 59, 44, 42,

      94, 1, 25, 90, 0, 30, 43, 12,

      50, 49, 77, 93, 0, 85, 80, 52,

      60, 74, 47, 17, 0, 47, 82, 71,

      29, 53, 46, 82, 0, 1, 1, 1)

    println()
    //println(buscarCamino(m, 0, 63, 8, 8, 1))
    //val (a, b) = sustituirFicha(m, 3 * 8 + 4, 1, 4)
    //mostrarTableroAux(a, 8)
    //println("\n" + b)

    val game = new Game()
    game.run()

    //--------------------------------------------------------------------
    //println("Pruebas xd")
    /*val rand = new scala.util.Random
    val a = rand.nextInt(10)
    println(a)
    val b = rand.between(1, 4)
    println(b)*/
  }
}