object Main {

  def main(args: Array[String]) {

    println("\n- CUNDY CROSH SOGA -\n")
    println("Aqui se va a llamar a la clase game")
    println("En game va a estar toda la logica")
    println("Aqui solo vamos a inicializar game")
    println("De momento estaba haciendo pruebas")

    //A lo mejor aqui se pregunta por las variables al usuario
    //Para despues pasarselas a game
    //Pero creo que lo suyo ser hacerlo en game

    val game = new Game()

    game.run()


    //--------------------------------------------------------------------
    //println("Pruebas xd")
    /*val rand = new scala.util.Random
    val a = rand.nextInt(10)
    println(a)
    val b = rand.between(1, 4)
    println(b)*/

    /*val tablero = new Tablero(5, 5, 6)
    println(tablero.getContenido())

    val x = List(1, 2, 3, 4)
    val y = List(5, 6, 7, 8)
    val z = concatenarListas(x, y)

    print("Mostrar tablero inicial")
    tablero.mostrarTablero()*/
  }
}