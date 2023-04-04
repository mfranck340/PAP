class Game {

  private val vidas = 5
  private val rand = new scala.util.Random
  private val tab = new Tablero

  def run(): Unit = {
    println("\n- CUNDY CROSH SOGA -\n")
    /*println("Introduce el numero de columnas del tablero: ")
    val col = scala.io.StdIn.readInt()
    println("Introduce el numero de filas del tablero: ")
    val fil = scala.io.StdIn.readInt()
    println("Introduce la dificultad del juego (1 - facil / 2 - dificil): ")*/
    //val dif = if (scala.io.StdIn.readChar() == '1')  4 else 6
    /*println("Introduce el modo de juego (a - automatico / m - manual): ")
    val modo = scala.io.StdIn.readChar()*/

    //Variables de prueba
    val col = 10
    val fil = 10
    val dif = 4
    val mod = 'a'
    val tablero = tab.inicializarTablero(fil * col)

    println("- START GAME :) - ")

    tab.mostrarTablero(tablero, col)
    runAux(col, fil, tablero, dif, mod, vidas)

    println("\n- GAME OVER :( -\n")
  }

  //Termina cuando vidas = 0
  private def runAux(col:Int, fil:Int, tablero:List[Int], dif:Int, mod:Char, vidas:Int): Unit = {
    //aqui un match para las vidas
    vidas match {
      case 0 => None
      case _ =>
        mod match {
          case 'a' => {
            val x = rand.nextInt(col)
            val y = rand.nextInt(fil)
            println(s"\nCoordenadas (${x}, ${y})")
          }
          case 'm' => {
            println("\nIntroduce la columna: ")
            val x = scala.io.StdIn.readInt()
            println("Introduce la fila: ")
            val y = scala.io.StdIn.readInt()
            println(s"Coordenadas (${x}, ${y})")
          }
        }

        val prueba3 = tab.actualizarTablero(tablero, col, dif)
        tab.mostrarTablero(prueba3, col)


      //pedir coordenadas al usuario
      //metodos de bajar y generar en una sola linea creo
      //o creo que habria que hacer una recursividad para ejecutar esto n veces para saber cuando dejar de bajar fichas
      //el resultado anterior se pasa a la recursividad y asi el tablero es el nuevo
    }
  }
}
