object Main {
  def main(args: Array[String]): Unit = {
    val game = new Game(args.toList)                   //Creamos el objeto game
    game.run()                              //Llamamos a run() para iniciar el juego
  }
}