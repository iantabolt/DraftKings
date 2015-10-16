import java.io.{ FileWriter, File }
import java.net.URL

import scala.annotation.tailrec

object DraftKings {
  case class Player(dkRank: Int, fpRank: Int, salary: Int, pos: String, name: String) {
    override def toString = {
      s"$name ... dk=$dkRank ... salary=$salary ... pos=$pos$fpRank"
    }
  }

  val SalaryCap = 50000
  val Positions = Map(
    "QB" -> 1,
    "RB" -> 3,
    "WR" -> 3,
    "TE" -> 1,
    "DST" -> 1)

  def getRanks(pos: String): Iterator[Vector[String]] = {
    val f = new File(pos)
    if (!f.exists()) {
      val fw = new FileWriter(f)
      val src = io.Source.fromURL(
        new URL(s"http://www.fantasypros.com/nfl/rankings/$pos.php?export=xls"))
      src.foreach { c =>
        fw.write(c)
      }
      fw.close()
    }
    val src = io.Source.fromFile(f)
    val vs = src.getLines().drop(6).map { line =>
      line.split("\t").toVector
    }
    if (pos == "dst") {
      vs.map { v =>
        v.updated(1, v(1).split(" ").last)
      }
    } else {
      vs
    }
  }

  def main(args: Array[String]) = {
    val salaries = {
      val salaryVs = io.Source.fromFile("DKSalaries.csv").getLines().drop(1).map { line =>
        line.split(",").map { entry =>
          if (entry.head == '"') entry.drop(1).dropRight(1)
          else entry
        }.toVector.map(_.trim)
      }.toList

      salaryVs.groupBy(_.head).mapValues { vs =>
        vs.zipWithIndex
      }.flatMap(_._2)
    }
    // salaries
    // (Vector("Position","Name","Salary","GameInfo","AvgPointsPerGame","teamAbbrev"),"posRank")

    val ranks = List("qb","te","wr","te","dst","rb").flatMap { pos =>
      getRanks(pos)
    }.map { v =>
      v(1) -> v
    }.toMap

    val salariesWithRanks = salaries.collect {
      case (v, posRank) if ranks.isDefinedAt(v(1)) => (ranks(v(1)), v, posRank)
    }

    val players = salariesWithRanks.map { case (rankV, salaryV, r) =>
      val v = (rankV ++ salaryV).map(_.trim)
      Player(r, rankV.head.toInt, v(10).toInt, v(8), v(1))
    }.toList

    val good = players.sortBy(_.dkRank).tails.collect {
      case p :: ps if ps.forall { p1 =>
        if (p.pos != p1.pos) true
        else p1.fpRank > p.fpRank || p1.salary > p.salary
      } => p
    }.toList

    val pool = good
      .groupBy(_.pos)
      .map { case (pos, ps) => pos -> ps.sortBy(_.fpRank).take(Positions(pos) * 5) }

    pool.foreach { case (pos, ps) =>
      println(pos + "\n--------------\n" + ps.mkString("\n") + "\n\n")
    }

    val optimalPicks: List[List[Player]] = {
      val allPossiblePicks = Iterator.fill(100000)(Positions.toList.permutations).flatten.map { positions =>
        makePicks(pool, positions, SalaryCap)
      }.collect { case Some(picks) =>
        picks
      }
      if (allPossiblePicks.isEmpty) Nil
      else {
        val (bestRankSum, bestSalary, maybePicks) =
          ((Double.MaxValue, Int.MaxValue, None: Option[List[Player]]) /: allPossiblePicks) { case ((bestRankSum, bestSalary, res), picks) =>
          val salary = picks.map(_.salary).sum
          val rankSum = picks.map(p => math.log(p.fpRank)).sum
          if (rankSum < bestRankSum) {
            (rankSum, math.max(bestSalary, salary), Some(picks))
          } else {
            (bestRankSum, math.max(bestSalary, salary), res)
          }
        }
        //        val app = allPossiblePicks.toList.map(_.toSet).distinct.map(_.toList)
//        val bestSalary = app.map(_.map(_.salary).sum).max
//        app.foreach(printPicks)
        println("Optimal remaining salary: " + (SalaryCap - bestSalary))
//        val bestRankSum = app.map(_.map(p => math.log(p.fpRank)).sum).min
        println("Optimal rank sum: " + bestRankSum)
//
//        app.filter { ps =>
//          //ps.map(_.salary).sum == bestSalary
//          ps.map(p => math.log(p.fpRank)).sum == bestRankSum
//        }

        maybePicks.toList
      }
    }
    optimalPicks match {
      case Nil => println("Could not find any valid picks")
      case pickss =>
        pickss.foreach(printPicks)
    }
  }

  def printPicks(picks: List[Player]): Unit = {
    println("PICKS\n=====")
    println("Remaining salary: " + (SalaryCap - picks.map(_.salary).sum))
    println("Log rank sum: " + picks.map(p => math.log(p.fpRank)).sum)
    List("QB", "RB", "WR", "TE", "DST").foreach { pos =>
      val ps = picks.filter(_.pos == pos)
      ps.sortBy(_.fpRank).foreach(println)
    }
    println()
  }

  @tailrec
  def makePicks(pool: Map[String, List[Player]], positions: List[(String, Int)], salaryRemaining: Int, picks: List[Player] = Nil): Option[List[Player]] = positions match {
    case Nil =>
      Some(picks)
    case (pos, n) :: positions1 =>
      val playersRemaining = positions.map(_._2).sum
      val avgSalaryPerPlayer: Double = salaryRemaining.toDouble / playersRemaining
      util.Random.shuffle(pool(pos)).find(p => p.salary <= avgSalaryPerPlayer) match {
        case None =>
          None
        case Some(pick) =>
          val newPool = pool.updated(pos, pool(pos).filterNot(_ == pick))
          val newPositions = if (n == 1) positions.tail else positions.tail :+ (pos, n-1)
          makePicks(newPool, newPositions, salaryRemaining - pick.salary, pick :: picks)
      }
  }
}
