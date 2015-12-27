package puzzle

import puzzle.Solver.{SolverError, Definition}
import scalaz.{\/-, \/}
import scalaz.syntax.either._

/**
  * Created by sdraper on 12/24/15.
  */
object Solver {


  final case class Definition(rowPatterns: List[List[Int]], columnPatterns: List[List[Int]], startState: BoardState)

  trait SolverError {
    val message: String
  }
  final case class BadInput(message: String) extends SolverError
  final case class NoSolutionError(message: String) extends SolverError
  final case class ConstraintError(message: String) extends SolverError
  final case class BadSolutionError(message: String) extends SolverError
  object NoValidChoiceError extends SolverError {
    val message = "No valid choice"
  }

  type SolverResult[A] = SolverError \/ A

  def solve(puzzle: Definition): SolverResult[BoardState] = {
    for {
      _ <- validateDef(puzzle)
      constraints <- generateInitialConstraints(puzzle)
      result <- findSolution(constraints)
    } yield result
  }

  private def validateDef(definition: Definition): SolverResult[Unit] = {
    //  Dimensions must be consistent
    val dim = definition.rowPatterns.length
    if ( definition.columnPatterns.length == dim &&
         definition.startState.cells.length == dim &&
         definition.startState.cells.forall(_.length == dim) ) ().right
    else BadInput("Puzzle definition has mismatched dimensions").left
  }

  private def generateInitialConstraints(puzzle: Definition): SolverResult[StateConstraint] = {
    def patternToRowConstraints(pattern: List[Int], index: Int) =
      RowConstraint(index,pattern.zipWithIndex.toVector map(((len: Int, index: Int) => SegmentConstraint(0,puzzle.startState.dimension-len,len, index)).tupled))
    def patternToColumnConstraints(pattern: List[Int], index: Int) =
      ColumnConstraint(index,pattern.zipWithIndex.toVector map(((len: Int, index: Int) => SegmentConstraint(0,puzzle.startState.dimension-len,len, index)).tupled))
    StateConstraint(
      puzzle.startState,
      puzzle.rowPatterns.zipWithIndex map((patternToRowConstraints _).tupled),
      puzzle.columnPatterns.zipWithIndex map((patternToColumnConstraints _).tupled)).right
  }

  private def reifyConstraints(start: StateConstraint): SolverResult[StateConstraint] = {
    def recursiveReify(constraints: StateConstraint): SolverResult[StateConstraint] = {
      //println("...Reify from:")
      //print(constraints.board.renderBoard)
      val reified = for {
        reification <- constraints.reifyConstraints()
      } yield reification

      reified flatMap(reification => {
        if (reification.determined.isEmpty) {
          StateConstraint(
            constraints.board,
            reification.newConstraint.rowConstraints,
            reification.newConstraint.columnConstraints).right
        }
        else {
          /*println()
          println()
          println(s"Found ${reification.determined.size} new call values")
          println()*/
          recursiveReify(
            StateConstraint(
              reification.determined.foldLeft(constraints.board)((board: BoardState, set: CellDetermination) => board.update(set)),
              reification.newConstraint.rowConstraints,
              reification.newConstraint.columnConstraints)
            )
        }
      })
    }

    recursiveReify(start)
  }

  final case class ContextualizedSegmentConstraint(line: LineConstraint, seg: SegmentConstraint)

  private def chooseBranchSegment(from: StateConstraint): SolverResult[Option[ContextualizedSegmentConstraint]] = {
    def mostConstrainedChoice(acc: Option[ContextualizedSegmentConstraint], line: LineConstraint) = {
      def mostConstrainedSegmentChoice(acc: Option[ContextualizedSegmentConstraint], seg: SegmentConstraint) = {
        if (seg.freedom == 0) acc
        else acc match {
          case None => Some(ContextualizedSegmentConstraint(line, seg))
          case Some(ContextualizedSegmentConstraint(line2, seg2)) =>
            if ( seg.freedom < seg2.freedom ) {
              Some(ContextualizedSegmentConstraint(line, seg))
            }
            else acc
        }
      }
      line.constraints.foldLeft(acc)(mostConstrainedSegmentChoice)
    }
    val rowMin = from.rowConstraints.foldLeft(None:Option[ContextualizedSegmentConstraint])(mostConstrainedChoice)
    from.columnConstraints.foldLeft(rowMin)(mostConstrainedChoice).right
  }

  def replaceLine(inList: List[LineConstraint], line: LineConstraint, newLine: LineConstraint) = {
    def replace(el: LineConstraint) = {
      if (el == line) newLine
      else el
    }
    inList.map(replace)
  }

  def replaceSeg(inList: Vector[SegmentConstraint], seg: SegmentConstraint, newSeg: SegmentConstraint) = {
    def replace(el: SegmentConstraint) = {
      if (el == seg) newSeg
      else el
    }
    inList.map(replace)
  }

  private def bestBranch(from: StateConstraint, seg: Option[ContextualizedSegmentConstraint]): SolverResult[StateConstraint] = {
    seg match {
      case None => from.right //  No open choices remain - solved
      case Some(ContextualizedSegmentConstraint(line,seg)) => {
        def tryChoice(foundSolution: SolverResult[StateConstraint], start: Int): SolverResult[StateConstraint] = foundSolution match {
          case \/-(s) => foundSolution
          case _ => {
            Stats.noteBranch

            if (line.fitsAt(from.board, start, seg)) {
              def segmentOccupied(acc: BoardState, index: Int) = acc.update(line.rowOf(index),line.columnOf(index),Occupied)
              val newBoardWithPreUnoccupied = {
                if (start > 0) {
                  val startFrom = {
                    if ( seg.index == 0 ) 0
                    else {
                      val previousSeg = line.constraints(seg.index-1)
                      if (previousSeg.freedom == 0) previousSeg.maxStart+previousSeg.len
                      else start-1
                    }
                  }
                  (startFrom to start-1).foldLeft(from.board)((acc: BoardState,loc:Int) => acc.update(line.rowOf(loc),line.columnOf(loc),Unoccupied))
                }
                else from.board
              }
              val newBoardWithOccupied = (start until start+seg.len).foldLeft(newBoardWithPreUnoccupied)(segmentOccupied)
              val newBoardWithPostUnoccupied = {
                if (start < from.board.dimension-seg.len) {
                  val endWith = {
                    if (seg.index == line.constraints.length-1) from.board.dimension-1
                    else {
                      val nextSeg = line.constraints(seg.index+1)
                      if (nextSeg.freedom == 0) nextSeg.minStart-1
                      else start+seg.len
                    }
                  }
                  (start+seg.len to endWith).foldLeft(newBoardWithOccupied)((acc: BoardState,loc:Int) => acc.update(line.rowOf(loc),line.columnOf(loc),Unoccupied))
                }
                else newBoardWithOccupied
              }
              val newSeg = SegmentConstraint(start, start, seg.len, seg.index)
              val newRowConstraints = line match {
                case RowConstraint(row,segList) => replaceLine(from.rowConstraints, line, RowConstraint(row,replaceSeg(segList, seg, newSeg)))
                case _ => from.rowConstraints
              }
              val newColumnConstraints = line match {
                case ColumnConstraint(column,segList) => replaceLine(from.columnConstraints, line, ColumnConstraint(column,replaceSeg(segList, seg, newSeg)))
                case _ => from.columnConstraints
              }
              branch(StateConstraint(newBoardWithPostUnoccupied, newRowConstraints, newColumnConstraints))
            } else NoValidChoiceError.left
          }
        }
        //  Try each choice in turn
        val noSolution: SolverResult[StateConstraint] = NoValidChoiceError.left
        (seg.minStart to seg.maxStart).foldLeft(noSolution)(tryChoice)
      }
    }
  }
  private def branch(from: StateConstraint): SolverResult[StateConstraint] = {
    for {
      constrained <- reifyConstraints(from)
      seg <- chooseBranchSegment(constrained)
      result <- bestBranch(constrained, seg)
    } yield result
  }

  private def findSolution(puzzle: StateConstraint): SolverResult[BoardState] = {
    for {
      constrained <- branch(puzzle)
    } yield {
      constrained.board
    }
  }
}