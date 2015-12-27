package puzzle

import puzzle.Solver._

import scalaz.std.list._
import scalaz.syntax.traverse._

/**
  * Created by sdraper on 12/24/15.
  */
final case class StateConstraint(board: BoardState, rowConstraints: List[LineConstraint], columnConstraints: List[LineConstraint]) extends Constraint {
  def reifyConstraints(): SolverResult[Reification[StateConstraint]] = {
    def aggregateDetermined[A <: Constraint](l: List[Reification[A]]): Set[CellDetermination] = {
      l.foldLeft(Set[CellDetermination]())((acc: Set[CellDetermination], el: Reification[A]) => el.determined ++ acc)
    }

    val test = rowConstraints(21).reifyConstraints(board)

    for {
      reifiedRowConstraints <- (rowConstraints map(_.reifyConstraints(board))).sequenceU
      reifiedColumnConstraints <- (columnConstraints map(_.reifyConstraints(board))).sequenceU
    } yield Reification(
      StateConstraint(board, reifiedRowConstraints.map(_.newConstraint), reifiedColumnConstraints.map(_.newConstraint)),
      aggregateDetermined(reifiedRowConstraints) ++ aggregateDetermined(reifiedColumnConstraints))
  }
}
