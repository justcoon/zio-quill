package io.getquill.idiom

import io.getquill.ast._
import io.getquill.util.Interleave
import io.getquill.idiom.StatementInterpolator._

import scala.annotation.tailrec

object ReifyStatement {

  def apply(
    liftingPlaceholder:    Int => String,
    emptySetContainsToken: Token => Token,
    statement:             Statement,
    forProbing:            Boolean
  ): (String, List[External]) = {
    val expanded =
      forProbing match {
        case true  => statement
        case false => expandLiftings(statement, emptySetContainsToken)
      }
    token2string(expanded, liftingPlaceholder)
  }

  private def token2string(token: Token, liftingPlaceholder: Int => String): (String, List[External]) = {
    @tailrec
    def apply(
      workList:      List[Token],
      sqlResult:     Seq[String],
      liftingResult: Seq[External],
      uuidLifting:   scala.collection.Map[String, Int],
      liftingSize:   Int
    ): (String, List[External]) = workList match {
      case Nil => sqlResult.reverse.mkString("") -> liftingResult.reverse.toList
      case head :: tail =>
        head match {
          case StringToken(s2)            => apply(tail, s2 +: sqlResult, liftingResult, uuidLifting, liftingSize)
          case SetContainsToken(a, op, b) => apply(stmt"$a $op ($b)" +: tail, sqlResult, liftingResult, uuidLifting, liftingSize)
          case ScalarLiftToken(lift) =>
            // FIXME lifting position by uuid
            //            val (liftIndex, newLiftingSize) = uuidLifting.get(lift.uuid).map((_, liftingSize)).getOrElse((liftingSize, liftingSize + 1))
            //            val newUuidLifting = if (uuidLifting.contains(lift.uuid)) uuidLifting else uuidLifting + (lift.uuid -> liftIndex)
            val newUuidLifting = uuidLifting
            val (liftIndex, newLiftingSize) = (liftingSize, liftingSize + 1)
            apply(tail, liftingPlaceholder(liftIndex) +: sqlResult, lift +: liftingResult, newUuidLifting, newLiftingSize)
          case ScalarTagToken(tag) => apply(tail, liftingPlaceholder(liftingSize) +: sqlResult, tag +: liftingResult, uuidLifting, liftingSize + 1)
          case Statement(tokens)   => apply(tokens.foldRight(tail)(_ +: _), sqlResult, liftingResult, uuidLifting, liftingSize)
          case _: QuotationTagToken =>
            throw new UnsupportedOperationException("Quotation Tags must be resolved before a reification.")
        }
    }

    apply(List(token), Seq(), Seq(), scala.collection.Map(), 0)
  }

  private def expandLiftings(statement: Statement, emptySetContainsToken: Token => Token) = {
    Statement {
      statement.tokens.foldLeft(List.empty[Token]) {
        case (tokens, SetContainsToken(a, op, ScalarLiftToken(lift: ScalarQueryLift))) =>
          lift.value.asInstanceOf[Iterable[Any]].toList match {
            case Nil => tokens :+ emptySetContainsToken(a)
            case values =>
              val liftings = values.map(v => ScalarLiftToken(ScalarValueLift(lift.name, v, lift.encoder, lift.uuid, lift.quat)))
              val separators = List.fill(liftings.size - 1)(StringToken(", "))
              (tokens :+ stmt"$a $op (") ++ Interleave(liftings, separators) :+ StringToken(")")
          }
        case (tokens, token) =>
          tokens :+ token
      }
    }
  }
}
