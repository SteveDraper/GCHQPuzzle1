package puzzle

import puzzle.Solver._

import scalaz.syntax.either._

/**
  * Created by sdraper on 12/24/15.
  */
object PuzzleApp extends App {
  val xmasPuzzleDef = Definition(
    List(
      List(7,3,1,1,7),
      List(1,1,2,2,1,1),
      List(1,3,1,3,1,1,3,1),
      List(1,3,1,1,6,1,3,1),
      List(1,3,1,5,2,1,3,1),
      List(1,1,2,1,1),
      List(7,1,1,1,1,1,7),
      List(3,3),
      List(1,2,3,1,1,3,1,1,2),
      List(1,1,3,2,1,1),
      List(4,1,4,2,1,2),
      List(1,1,1,1,1,4,1,3),
      List(2,1,1,1,2,5),
      List(3,2,2,6,3,1),
      List(1,9,1,1,2,1),
      List(2,1,2,2,3,1),
      List(3,1,1,1,1,5,1),
      List(1,2,2,5),
      List(7,1,2,1,1,1,3),
      List(1,1,2,1,2,2,1),
      List(1,3,1,4,5,1),
      List(1,3,1,3,10,2),
      List(1,3,1,1,6,6),
      List(1,1,2,1,1,2),
      List(7,2,1,2,5)
    ),
    List(
      List(7,2,1,1,7),
      List(1,1,2,2,1,1),
      List(1,3,1,3,1,3,1,3,1),
      List(1,3,1,1,5,1,3,1),
      List(1,3,1,1,4,1,3,1),
      List(1,1,1,2,1,1),
      List(7,1,1,1,1,1,7),
      List(1,1,3),
      List(2,1,2,1,8,2,1),
      List(2,2,1,2,1,1,1,2),
      List(1,7,3,2,1),
      List(1,2,3,1,1,1,1,1),
      List(4,1,1,2,6),
      List(3,3,1,1,1,3,1),
      List(1,2,5,2,2),
      List(2,2,1,1,1,1,1,2,1),
      List(1,3,3,2,1,8,1),
      List(6,2,1),
      List(7,1,4,1,1,3),
      List(1,1,1,1,4),
      List(1,3,1,3,7,1),
      List(1,3,1,1,1,2,1,1,4),
      List(1,3,1,4,3,3),
      List(1,1,2,2,2,6,1),
      List(7,1,3,2,1,1)
    ),
    BoardState(Vector.fill(25,25)(Unknown), 0)
      .update(3,3,Occupied)
      .update(3,4,Occupied)
      .update(3,12,Occupied)
      .update(3,13,Occupied)
      .update(3,21,Occupied)
      .update(8,6,Occupied)
      .update(8,7,Occupied)
      .update(8,10,Occupied)
      .update(8,14,Occupied)
      .update(8,15,Occupied)
      .update(8,18,Occupied)
      .update(16,6,Occupied)
      .update(16,11,Occupied)
      .update(16,16,Occupied)
      .update(16,20,Occupied)
      .update(21,3,Occupied)
      .update(21,4,Occupied)
      .update(21,9,Occupied)
      .update(21,10,Occupied)
      .update(21,15,Occupied)
      .update(21,10,Occupied)
      .update(21,21,Occupied)
  )

  def validate(board: BoardState): SolverResult[BoardState] = {
    def toPattern(cells: Seq[CellState]): List[Int] = {
      case class CursorState(runLen: Int, soFar: List[Int])

      def consumeCell(cursor: CursorState, cell: CellState): CursorState = {
        cell match {
          case Occupied => CursorState(cursor.runLen+1, cursor.soFar)
          case _ => {
            if (cursor.runLen == 0) cursor
            else CursorState(0, cursor.runLen :: cursor.soFar)
          }
        }
      }

      val lineCursor = cells.foldLeft(CursorState(0,Nil))(consumeCell)
      val revPattern =
        if ( lineCursor.runLen > 0 ) lineCursor.runLen :: lineCursor.soFar
        else lineCursor.soFar

      revPattern.reverse
    }
    def updateBoard(acc: BoardState, update: CellDetermination) = acc.update(update)

    val rows = board.cells.toList.map(toPattern)
    val boardTranspose =
      (for {
        i <- (0 until board.dimension)
        j <- (0 until board.dimension)
      } yield CellDetermination(j,i,board.cells(i)(j)))
      .foldLeft(board)(updateBoard)
    val columns = boardTranspose.cells.toList.map(toPattern)

    if ( rows == xmasPuzzleDef.rowPatterns && columns == xmasPuzzleDef.columnPatterns)
      board.right
    else
      BadSolutionError("Putative solution failed validation").left
  }

  Stats.noteStart
  val solution = Solver.solve(xmasPuzzleDef) flatMap validate
  Stats.noteEnd

  val renderToHTML = args.contains("-html")
  val outputBody = s"${solution fold(_.message, _.renderBoard(renderToHTML))}\n\n${Stats.render(renderToHTML)}"
  val output = {
    if ( renderToHTML ) {
      val cellSize = 16
      val style =
        s"""<style TYPE="text/css">
          |table {
          |border-collapse: collapse;
          |}
          |td.black {
          |width: ${cellSize}px;
          |height: ${cellSize}px;
          |border: 0px;
          |padding: 0px;
          |margin: 0px;
          |background: black;
          |}
          |td.white {
          |width: ${cellSize}px;
          |height: ${cellSize}px;
          |border: 0px;
          |padding: 0px;
          |margin: 0px;
          |background: white;
          |}
          |</style>
        """.stripMargin
      val header = s"<HTML><HEAD>\n$style\n</HEAD>\n<BODY>\n"
      val footer = "\n</BODY>\n</HTML>"

      s"$header$outputBody$footer"
    }
    else outputBody
  }
  print(output)
}
