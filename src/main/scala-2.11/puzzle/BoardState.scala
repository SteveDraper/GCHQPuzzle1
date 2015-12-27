package puzzle

trait CellState
object Unknown extends CellState
object Occupied extends CellState
object Unoccupied extends CellState
final case class BoardState(cells: Vector[Vector[CellState]], determined: Int) {
  def update(row: Int, column: Int, value: CellState): BoardState = {
    //  Strictly speaking unsafe, but private and a pain to wrap in a disjunct since it
    //  will be performance critical
    val newlyDetermined = cells(row)(column) == Unknown && value != Unknown
    BoardState(
      cells.updated(row, cells(row).updated(column, value)),
      if (newlyDetermined) determined+1 else determined)
  }

  def update(det: CellDetermination): BoardState = update(det.row, det.column, det.value)

  def getCell(row: Int, column: Int) = cells(row)(column)

  val dimension = cells.length

  def renderBoard(renderToHtml: Boolean): String = {
    def renderRow(row: Vector[CellState]) = {
      def renderCell(cell: CellState) = cell match {
        case Unoccupied => if ( renderToHtml ) s"<td class='white'/>" else " "
        case Occupied => if ( renderToHtml ) s"<td class='black'/>" else "X"
        case Unknown => if ( renderToHtml ) s"<td class='white'>?</td>" else "."
      }
      val rowBody = (row map renderCell) mkString

      if (renderToHtml) s"<tr>$rowBody</tr>"
      else rowBody
    }
    val boardBody = (cells.toList map(renderRow)) mkString("","\n","")

    if ( renderToHtml ) s"<table>\n$boardBody\n</table>"
    else boardBody
  }

  Stats.noteDetermnined(determined)
}