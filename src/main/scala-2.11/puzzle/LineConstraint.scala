package puzzle

import puzzle.Solver._

import scalaz.syntax.either._
import scalaz.std.list._
import scalaz.syntax.traverse._

final case class SegmentConstraint(minStart: Int, maxStart: Int, len: Int, index: Int) {
  val freedom = maxStart - minStart

  def updateMin(newMin: Int) = SegmentConstraint(newMin, maxStart, len, index)
  def updateMax(newMax: Int) = SegmentConstraint(minStart, newMax, len, index)
}

/**
  * Created by sdraper on 12/24/15.
  */
abstract class LineConstraint extends Constraint {
  val lineIndex: Int
  val constraints: Vector[SegmentConstraint]

  def rowOf(index: Int): Int
  def columnOf(index: Int): Int

  def getBoardCell(board: BoardState, index: Int): CellState

  val lineType: String

  def makeNew(index: Int, c: Vector[SegmentConstraint]): LineConstraint

  def reifyConstraints(board: BoardState): SolverResult[Reification[LineConstraint]] = {
    def determinedBy(newConstraint: Vector[SegmentConstraint]) = {
      def addNewSegmentDeterminations(acc: SolverResult[Set[CellDetermination]], seg: SegmentConstraint): SolverResult[Set[CellDetermination]] = {
        def newDeterminations(): SolverResult[Set[CellDetermination]] = {
          if (seg.freedom >= seg.len) acc
          else {
            val determinedOccupied = ((seg.minStart + seg.freedom) until (seg.minStart + seg.len))
              .filter(getBoardCell(board, _) != Occupied)
              .toList
              .map(loc=>{
                if ( getBoardCell(board, loc) == Unknown) CellDetermination(rowOf(loc), columnOf(loc), Occupied).right
                else ConstraintError(s"$lineType $lineIndex had constraint error").left
              }).sequenceU

            val determinedUnoccupied = {
              if (seg.freedom == 0) {
                val rightLoc = seg.minStart + seg.len
                val rightList =
                  if (rightLoc < board.dimension) Set(CellDetermination(rowOf(rightLoc),columnOf(rightLoc),Unoccupied))
                  else Set()
                val leftList =
                  if (seg.minStart > 0) Set(CellDetermination(rowOf(seg.minStart-1), columnOf(seg.minStart-1), Unoccupied))
                  else Set()
                val preLeftList =
                  if (seg.index > 0) {
                    val previousSeg = newConstraint(seg.index-1)
                    if (previousSeg.freedom == 0) {
                      (previousSeg.maxStart+previousSeg.len to seg.minStart-1)
                        .foldLeft(Set[CellDetermination]())((acc:Set[CellDetermination],loc: Int) => acc + CellDetermination(rowOf(loc),columnOf(loc),Unoccupied))
                    }
                    else Set()
                  }
                else Set()
                val postRightList =
                  if (seg.index < newConstraint.length-1) {
                    val nextSeg = newConstraint(seg.index+1)
                    if (nextSeg.freedom == 0) {
                      (seg.maxStart+seg.len to nextSeg.minStart-1)
                        .foldLeft(Set[CellDetermination]())((acc:Set[CellDetermination],loc: Int) => acc + CellDetermination(rowOf(loc),columnOf(loc),Unoccupied))
                    }
                    else Set()
                  }
                  else Set()

                leftList ++ rightList ++ preLeftList ++ postRightList
              }
              else Set()
            }

            for {
              occupiedList <- determinedOccupied
            } yield (determinedUnoccupied ++ occupiedList)
          }
        }

        for {
          curr <- acc
          newlyFound <- newDeterminations
        } yield newlyFound ++ curr
      }

      val noDeterminations: SolverResult[Set[CellDetermination]] = Set().right

      constraints
        .zip(newConstraint)
        .filter(x=>x._1 != x._2)
        .map(_._2)
        .foldLeft(noDeterminations)(addNewSegmentDeterminations)
    }

    for {
      newConstraints <- tightenConstraints(board, constraints)
      result <- {
        if ( newConstraints == constraints ) Reification(this,Set()).right
        else for {
          determined <- determinedBy(newConstraints)
        } yield Reification(makeNew(lineIndex, newConstraints),determined)
      }
    } yield result
  }

  def fitsAt(board: BoardState, loc: Int, seg: SegmentConstraint) = {
    (loc until loc+seg.len).forall(getBoardCell(board, _)!=Unoccupied) &&
      (loc == board.dimension-seg.len || getBoardCell(board, loc+seg.len) != Occupied) &&
      (loc == 0 || getBoardCell(board, loc-1) != Occupied)
  }

  def tightenConstraints(board: BoardState, start: Vector[SegmentConstraint]) = {
    final case class ConstraintCursor(soFar: Vector[SegmentConstraint], nextIndex: Int, possible: Boolean)

    def findFirstFit(tryAt: Int, seg: SegmentConstraint): Option[Int] = {
      if ( tryAt > board.dimension-seg.len || tryAt > seg.maxStart) None
      else {
        if (fitsAt(board, tryAt, seg)) Some(tryAt)
        else findFirstFit(tryAt+1, seg)
      }
    }

    def findLastFit(tryAt: Int, seg: SegmentConstraint): Option[Int] = {
      if ( tryAt < 0 || tryAt < seg.minStart) None
      else {
        if (fitsAt(board, tryAt, seg)) Some(tryAt)
        else findLastFit(tryAt-1, seg)
      }
    }

    def addFirstFit(acc: ConstraintCursor, seg: SegmentConstraint) = {
      if ( acc.possible )
        findFirstFit(Math.max(acc.nextIndex, seg.minStart), seg)
          .fold(ConstraintCursor(Vector(),0,false))(loc=>ConstraintCursor(seg.updateMin(loc)+:acc.soFar,loc+seg.len+1,true))
      else acc
    }

    def addLastFit(acc: ConstraintCursor, seg: SegmentConstraint) = {
      if ( acc.possible )
        findLastFit(Math.min(acc.nextIndex - seg.len + 1, seg.maxStart+seg.len-1), seg)
          .fold(ConstraintCursor(Vector(),0,false))(loc=>ConstraintCursor(seg.updateMax(loc)+:acc.soFar,loc-2,true))
      else acc
    }

    val leftConstraints = start.foldLeft(ConstraintCursor(Vector(),0,true))(addFirstFit)
    val newConstraints = {
      if ( leftConstraints.possible ) {
        leftConstraints.soFar.foldLeft(ConstraintCursor(Vector(),board.dimension-1,true))(addLastFit)
      }
      else leftConstraints
    }
    if ( newConstraints.possible ) {
      newConstraints.soFar.right
    }
    else NoSolutionError(s"$lineType $lineIndex had no solution").left
  }
}

final case class RowConstraint(lineIndex: Int, constraints: Vector[SegmentConstraint]) extends LineConstraint {
  val lineType = "row"

  def rowOf(index: Int) = lineIndex
  def columnOf(index: Int) = index

  def makeNew(index: Int, c: Vector[SegmentConstraint]): LineConstraint = RowConstraint(index, c)

  override def getBoardCell(board: BoardState, index: Int): CellState = {
    board.getCell(lineIndex, index)
  }
}

final case class ColumnConstraint(lineIndex: Int, constraints: Vector[SegmentConstraint]) extends LineConstraint {
  val lineType = "column"

  def rowOf(index: Int) = index
  def columnOf(index: Int) = lineIndex

  def makeNew(index: Int, c: Vector[SegmentConstraint]): LineConstraint = ColumnConstraint(index, c)

  override def getBoardCell(board: BoardState, index: Int): CellState = {
    board.getCell(index, lineIndex)
  }
}
