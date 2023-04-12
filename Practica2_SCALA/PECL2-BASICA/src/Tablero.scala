import scala.annotation.tailrec

class Tablero() {
  private val rand = new scala.util.Random(System.currentTimeMillis())

  //Función para crear un tablero (lista con tantas posiciones como indique celdas) con todas las fichas de aire
  def inicializarTablero(celdas:Int): List[Int] = {
    celdas match {
      case 0 => Nil                                                                       //En el caso base, devolvemos la lista vacía
      case _ => 0 :: inicializarTablero(celdas - 1)                                       //Realizamos la recursividad concatenando un 0 al inicio
    }
  }

  //Función para actualizar el tablero si se han eliminado fichas
  def actualizarTablero(tablero: List[Int], col: Int, dif: Int): List[Int] = {
    actualizarTableroAux(tablero, col, dif, comprobarTablero(tablero))                    //Llamamos a actualizarTableroAux() comprobando si se han eliminado fichas
  }

  //Función recursiva para actualizar el tablero si se han eliminado fichas
  @tailrec
  private def actualizarTableroAux(tablero: List[Int], col: Int, dif: Int, continuar: Boolean): List[Int] = {
    if (continuar) {                                                                      //Si hay bloques de aire, bajamos las fichas y generamos nuevas en lo alto
      mostrarTablero(tablero, col)                                                        //Mostramos el tablero
      val tabAux = generarFichas(bajarFichas(tablero, col, lengthCustom(tablero) - 1), dif, col)
      actualizarTableroAux(tabAux, col, dif, comprobarTablero(tabAux))                    //Hacemos recursividad comprobando si quedan bloques de aire en el nuevo tablero
    } else {
      tablero                                                                             //Si no hay bloques de aire, devolvemos el mismo tablero
    }
  }

  //Función que realiza cambios en el tablero dependiendo de las coordenadas introducidas
  def interactuarConTablero(tablero:List[Int], cordX:Int, cordY:Int, col:Int, dif:Int): (List[Int], Boolean) = {
    val (tabAux, vidas) = realizarMovimiento(tablero, cordX, cordY, col, dif)             //Realizamos el movimiento con las coordenadas
    (actualizarTablero(tabAux, col, dif), vidas)                                          //Devolvemos el tablero actualizado junto con las vidas que tiene el usuario tras realizar el movimiento
  }

  //Función que realiza el movimiento adecuado dependiendo del elemento que ha sido pulsado
  private def realizarMovimiento(tablero:List[Int], cordX:Int, cordY:Int, col:Int, dif:Int): (List[Int], Boolean) = {
    val elem = getElem(cordY * col + cordX, tablero)                                      //Obtenemos el elemento que se encuentra en la posición del tablero que se ha pulsado
    elem match {
      case _ if elem < 7 =>                                                               //Si el elemento es menor que 7, es una ficha normal
        sustituirFicha(eliminarFichas(tablero, elem, lengthCustom(tablero) - 1, cordY * col + cordX, col), cordY * col + cordX, elem, dif)
      case 8 =>                                                                           //Si el elemento es 8, hacemos explotar la bomba
        (activarBomba(tablero, cordY * col + cordX, col, rand.nextInt(2)), false)
      case 9 =>                                                                           //Si el elemento es 9, hacemos estallar la TNT
        (activarTnt(tablero, cordY * col + cordX, col), false)
      case _ =>                                                                           //En cualquier otro caso, activamos el rompecabezas
        (insertar(0, cordY * col + cordX, activarRompe(tablero, elem % 10)), false)
    }
  }

  //Función para activar el rompecabezas
  private def activarRompe(tablero:List[Int], elem:Int): List[Int] = {
    activarRompeAux(tablero, elem, lengthCustom(tablero) - 1)
  }

  @tailrec
  private def activarRompeAux(tablero:List[Int], elem:Int, pos:Int): List[Int] = {
    pos match {
      case -1 => tablero
      case _ =>
        if (getElem(pos, tablero) == elem)
          activarRompeAux(insertar(0, pos, tablero), elem, pos - 1)
        else
          activarRompeAux(tablero, elem, pos - 1)
    }
  }

  private def activarTnt(tablero:List[Int], posObj:Int, col:Int): List[Int] = {
    activarTntAux(tablero, posObj, col, lengthCustom(tablero) - 1)
  }

  @tailrec
  private def activarTntAux(tablero:List[Int], posObj:Int, col:Int, pos:Int): List[Int] = {
    pos match {
      case -1 => tablero
      case _ =>
        if (math.sqrt(math.pow(posObj % col - pos % col, 2) + math.pow(posObj / col - pos / col, 2)) <= 4)
          activarTntAux(insertar(0, pos, tablero), posObj, col, pos - 1)
        else
          activarTntAux(tablero, posObj, col, pos - 1)
    }
  }

  private def activarBomba(tablero:List[Int], pos:Int, col:Int, opcion:Int): List[Int] = {
    opcion match {
      case 0 => eliminarFila(tablero, (pos/col) * col, col)
      case _ => eliminarColumna(tablero, pos % col, col, lengthCustom(tablero) / col)
    }
  }

  @tailrec
  private def eliminarFila(tablero:List[Int], pos:Int, col:Int): List[Int] = {
    if (col == 0)
      tablero
    else
      eliminarFila(insertar(0, pos, tablero), pos + 1, col - 1)
  }

  @tailrec
  private def eliminarColumna(tablero:List[Int], pos:Int, col:Int, fil:Int): List[Int] = {
    if (fil == 0)
      tablero
    else
      eliminarColumna(insertar(0, pos, tablero), pos + col, col, fil - 1)
  }

  private def sustituirFicha(tablero: List[Int], pos: Int, elem: Int, dif: Int): (List[Int], Boolean) = {
    val fichas = contarFichasEliminadas(tablero)
    if (fichas == 1)
      (insertar(elem, pos, tablero), true)
    else if (fichas == 5)
      (insertar(8, pos, tablero), false)
    else if (fichas == 6)
      (insertar(9, pos, tablero), false)
    else if (fichas >= 7)
      (insertar(10 + rand.between(1, dif), pos, tablero), false)
    else
      (tablero, false)
  }

  @tailrec
  private def eliminarFichas(tablero:List[Int], elem:Int, pos:Int, posFin:Int, col:Int): List[Int] = {
    pos match {
      case -1 => tablero
      case _ =>
        if (getElem(pos, tablero) == elem && buscarCamino(tablero, pos, posFin, col, lengthCustom(tablero) / col, elem))
          eliminarFichas(insertar(0, pos, tablero), elem, pos - 1, posFin, col)

        else
          eliminarFichas(tablero, elem, pos - 1, posFin, col)
    }
  }

  private def buscarCamino(tablero: List[Int], posIni: Int, posFin: Int, col: Int, fil: Int, elem: Int): Boolean = {
    if (posIni == posFin) true

    else {
      val tabAux = insertar(-1, posIni, tablero)
      if ((posIni + 1) % col != 0) {
        if (getElem(posIni + 1, tablero) == elem) {
          if (buscarCamino(tabAux, posIni + 1, posFin, col, fil, elem)) return true
        }
        else if (getElem(posIni + 1, tablero) == 0) {
          return true
        }
      }

      if (posIni % col != 0) {
        if (getElem(posIni - 1, tablero) == elem) {
          if (buscarCamino(tabAux, posIni - 1, posFin, col, fil, elem)) return true
        }
        else if (getElem(posIni - 1, tablero) == 0) {
          return true
        }
      }

      if (posIni >= col) {
        if (getElem(posIni - col, tablero) == elem) {
          if (buscarCamino(tabAux, posIni - col, posFin, col, fil, elem)) return true
        }
        else if (getElem(posIni - col, tablero) == 0) {
          return true
        }
      }

      if (posIni < col * (fil - 1))
        if (getElem(posIni + col, tablero) == elem) {
          if (buscarCamino(tabAux, posIni + col, posFin, col, fil, elem)) return true
        }
        else if (getElem(posIni + col, tablero) == 0) {
          return true
        }

      false
    }
  }

  def mostrarTablero(x:List[Int], n:Int): Unit = {
    x match {
      case Nil =>  print("\n" + "---" * (n - 1) + "----")
      case _ =>
        if (lengthCustom(x) % n == 0) {
          print("\n" + "---" * (n - 1) + "----" + "\n|")
        }
        x.head match {
          case 8 => print("B |")
          case 9 => print("T |")
          case _ =>
            if (x.head < 7) print(x.head + " |")
            else print(s"R${x.head % 10}|")
        }
        mostrarTablero(x.tail, n)
    }
  }

  private def lengthCustom(x:List[Int]): Int = {
    x match {
      case Nil => 0
      case _ => 1 + lengthCustom(x.tail)
    }
  }

  private def generarFichas(tablero:List[Int], dif:Int, col:Int): List[Int] = {
    col match {
      case 0 => tablero
      case _ =>
        if (tablero.head == 0)
          rand.between(1, dif + 1) :: generarFichas(tablero.tail, dif, col - 1)
        else
          tablero.head :: generarFichas(tablero.tail, dif, col - 1)
    }
  }

  private def insertar(elem: Int, pos: Int, lista: List[Int]): List[Int] = {
    lista match {
      case Nil => elem :: Nil
      case _ =>
        pos match {
          case 0 => elem :: lista.tail
          case _ => lista.head :: insertar(elem, pos - 1, lista.tail)
        }
    }
  }

  //Funcion para obtener un elemento de la matriz unidimensional
  @tailrec
  private def getElem(index: Int, matriz: List[Int]): Int = {
    index match {
      case 0 => matriz.head
      case _ => getElem(index - 1, matriz.tail)
    }
  }

  @tailrec
  private def bajarFichas(tablero:List[Int], col:Int, pos:Int): List[Int] = {
    if (pos == col - 1)
      tablero
    else {
      if (getElem(pos, tablero) == 0 && pos - col >= 0 && getElem(pos - col, tablero) != 0)
        bajarFichas(insertar(0, pos - col, insertar(getElem(pos - col, tablero), pos, tablero)), col, pos - 1)
      else
        bajarFichas(tablero, col, pos - 1)
    }
  }

  @tailrec
  private def comprobarTablero(tablero:List[Int]): Boolean = {
    tablero match {
      case Nil => false
      case _ =>
        if (tablero.head == 0) true
        else comprobarTablero(tablero.tail)
    }
  }

  private def contarFichasEliminadas(tablero: List[Int]): Int = {
    tablero match {
      case Nil => 0
      case _ =>
        if (tablero.head == 0) 1 + contarFichasEliminadas(tablero.tail)
        else contarFichasEliminadas(tablero.tail)
    }
  }
}