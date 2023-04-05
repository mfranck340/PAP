class Game {

  private val vidas = 5
  private val rand = new scala.util.Random(System.currentTimeMillis())
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
    val col = 6
    val fil = 6
    val dif = 4
    val mod = 'a'
    val tablero = tab.inicializarTablero(fil * col)

    println("- START GAME :) - ")

    tab.mostrarTablero(tablero, col)
    runAux(col, fil, tab.actualizarTablero(tablero, col, dif), dif, mod, vidas)

    println("\n- GAME OVER :( -\n")
  }

  //Termina cuando vidas = 0
  private def runAux(col:Int, fil:Int, tablero:List[Int], dif:Int, mod:Char, vidas:Int): Unit = {
    //aqui un match para las vidas
    println(s"\nVIDAS: ${vidas}")
    vidas match {
      case 0 => None
      case _ =>
        tab.mostrarTablero(tablero, col)
        mod match {
          case 'a' => {
            val x = rand.nextInt(col)
            val y = rand.nextInt(fil)
            println(s"\nCoordenadas (${x}, ${y})")
            val (tabAux, restar) = tab.interactuarConTablero(tablero, x, y, col, dif)
            runAux(col, fil, tabAux, dif, mod, if (restar) vidas - 1 else vidas)
          }
          case 'm' => {
            println("\nIntroduce la columna: ")
            val x = scala.io.StdIn.readInt()
            println("Introduce la fila: ")
            val y = scala.io.StdIn.readInt()
            println(s"Coordenadas (${x}, ${y})")
            val (tabAux, restar) = tab.interactuarConTablero(tablero, x, y, col, dif)
            runAux(col, fil, tabAux, dif, mod, if (restar) vidas - 1 else vidas)
          }
        }
    }
  }
}
