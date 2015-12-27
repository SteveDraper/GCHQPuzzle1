package puzzle

/**
  * Created by sdraper on 12/24/15.
  */
trait Constraint

final case class CellDetermination(row: Int, column: Int, value: CellState)
final case class Reification[A <: Constraint](newConstraint: A, determined: Set[CellDetermination])
