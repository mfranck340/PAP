import scala.annotation.tailrec

class Tablero() {
  private val rand = new scala.util.Random(System.currentTimeMillis())

  def inicializarTablero(celdas:Int): List[Int] = {
     celdas match {
       case 0 => Nil
       case _ => 0 :: inicializarTablero(celdas - 1)
     }
  }

  def mostrarTablero(tablero: List[Int], n: Int): Unit = {        // esto se puede quitar y dejar solo la aux
    mostrarTableroAux(tablero, n)
  }

  def actualizarTablero(tablero: List[Int], col: Int, dif: Int): List[Int] = {
    actualizarTableroAux(tablero, col, dif, true)
  }

  def interactuarConTablero(tablero:List[Int], cordX:Int, cordY:Int, col:Int, dif:Int): (List[Int], Boolean) = {
    val (tabAux, vidas) = realizarMovimiento(tablero, cordX, cordY, col, dif)
    (actualizarTablero(tabAux, col, dif), vidas)
  }

  private def realizarMovimiento(tablero:List[Int], cordX:Int, cordY:Int, col:Int, dif:Int): (List[Int], Boolean) = {
    val elem = getElem(cordY * col + cordX, tablero)
    elem match {
      case _ if elem < 7 =>
        sustituirFicha(eliminarFichas(tablero, elem, lengthCustom(tablero) - 1, cordY * col + cordX, col), cordY * col + cordX, elem, dif)
      //case 8 => activarBomba()
      //case 9 => activarTnt()
      //case _ => activarRompe()
    }
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

  //def activarBomba(): List[Int]

  //def activarTnt(): List[Int]

  //def activarRompe(): List[Int]

  /*private def concatenarListas(x:List[Int], y:List[Int]): List[Int] = {
    x match {
      case Nil => y
      case n :: Nil => n :: y
      case head :: tail => head::concatenarListas(tail, y)
    }
  }*/

  @tailrec
  private def mostrarTableroAux(x:List[Int], n:Int): Unit = {
    x match {
      case Nil =>  print("\n" + "---" * (n - 1) + "----")
      case _ =>
        if (lengthCustom(x) % n == 0) {
          print("\n" + "---" * (n - 1) + "----" + "\n|")
        }
        if (x.head < 7)                                             //se podria cambiar por un match
          print(x.head + " |")
        else if (x.head == 8)
          print("B |")
        else if (x.head == 9)
          print("T |")
        else
            print(s"R${x.head % 10}|")
        mostrarTableroAux(x.tail, n)
    }
  }

  private def lengthCustom(x:List[Int]): Int = {
    x match {
      case Nil => 0
      case _ => 1 + lengthCustom(x.tail)
    }
  }

  /*private def reverseCustom(x: List[Int]): List[Int] = {
    x match {
      case Nil => Nil
      case _ => concatenarListas(reverseCustom(x.tail), x.head :: Nil)
    }
  }*/

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

  @tailrec
  private def actualizarTableroAux(tablero:List[Int], col:Int, dif:Int, continuar:Boolean): List[Int] = {
    if (continuar) {
      val tabAux = generarFichas(bajarFichas(tablero, col, lengthCustom(tablero) - 1), dif, col)
      //mostrarTablero(tabAux, col)
      actualizarTableroAux(tabAux, col, dif, comprobarTablero(tabAux))
    } else {
      tablero
    }
  }

}