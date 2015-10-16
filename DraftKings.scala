import java.net.URL

object DraftKings {
  case class Player(dkRank: Int, fpRank: Int, salary: Int, pos: String, name: String) {
    override def toString = {
      s"$name\tdk=$dkRank\tfp=$fpRank\tsalary=$salary\tpos=$pos"
    }
  }

  def main(args: Array[String]) = {
    val salaries = io.Source.fromFile("DKSalaries.csv").getLines().drop(1).map { line =>
      line.split(",").map { entry =>
        if (entry.head == '"') entry.drop(1).dropRight(1)
        else entry
      }.toVector
    }.toList

    val ranks = List("qb","te","wr","te","dst","rb").flatMap { pos =>
      io.Source.fromURL(
        new URL(s"http://www.fantasypros.com/nfl/rankings/$pos.php?export=xls"))
        .getLines().drop(6).map { line =>
          line.split("\t").toVector
        }
      }.map { v =>
        v(1) -> v
      }.toMap

    val players = salaries.collect {
      case v if ranks.isDefinedAt(v(1)) =>
        (ranks(v(1)) ++ v).map(_.trim)
    }.zipWithIndex.map { case (v, r) =>
      Player(r.toInt, v(0)toInt, v(10).toInt, v(8), v(1))
    }

    val good = players.sortBy(_.dkRank).tails.collect {
      case p :: ps if ps.forall { p1 =>
        if (p.pos != p1.pos) true
        else p1.fpRank > p.fpRank || p1.salary > p.salary
      } => p
    }.toList

    good.filter(_.fpRank < 60)
        .sortBy(p => p.fpRank - p.dkRank)
        .groupBy(_.pos)
        .flatMap(_._2.take(10).sortBy(_.fpRank))
        .foreach(println)
  }
}
