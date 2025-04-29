/**
  *  https://www.codingame.com/ide/puzzle/winamax-battle
  */

import math._
import scala.util._
import scala.io.StdIn._

/** a card */
class Card(name: String) {
    val value = name.substring(0, name.size - 1) match {
        case "J" => 11
        case "Q" => 12
        case "K" => 13
        case "A" => 14
        case _ => name.substring(0, name.size - 1).toInt
    }

    override def toString: String = s"${value}"
}

/** the amount of cards to be earned in a war */
class Loot(cards1: List[Card] = List(), cards2: List[Card] = List()) {
    def add(new1: List[Card], new2: List[Card]): Loot = {
        new Loot(cards1 ++ new1, cards2 ++ new2)
    }

    def add(new1: Card, new2: Card): Loot = {
        new Loot(cards1 :+ new1, cards2 :+ new2)
    }

    def cards: List[Card] = cards1 ++ cards2
}

/** the player with their cards */
class Player(_id: Int, _cards: List[Card]) {
    val id = _id
    val cards = _cards
    def draw(): (Player, Card) = (new Player(id, cards.tail), cards.head)
    def draw3(): (Player, List[Card]) = (
        new Player(id, cards.tail.tail.tail),
        List(cards.head, cards.tail.head, cards.tail.tail.head)
    )
    def gain(loot: Loot): Player = new Player(id, cards ++ loot.cards)
    val lost: Boolean = cards.length == 0
}

/* who won (if any) */
class GameResult(winner: Option[Player], rounds: Int) {
    def this(w: Player, r: Int) = this(Some(w), r)

    override val toString: String = winner match {
        case Some(p) => s"${p.id} ${rounds}"
        case None => "PAT"
    }
}

class Game(cards1: List[Card], cards2: List[Card]) {
    /** calculates the outcome of the game */
    def loop(
        player1: Player = new Player(1, cards1),
        player2: Player = new Player(2, cards2),
        round: Int = 0,
        loot: Loot = new Loot()
    ): GameResult = {
        if (player1.lost) new GameResult(player2, round)
        else if (player2.lost) new GameResult(player1, round)
        else {
            val (p1, c1) = player1.draw()
            val (p2, c2) = player2.draw()
            val nextLoot = loot.add(c1, c2)

            if (c1.value == c2.value) {
                if (p1.cards.length < 4 || p2.cards.length < 4) new GameResult(None, round + 1)
                else {
                    val (pw1, cw1) = p1.draw3()
                    val (pw2, cw2) = p2.draw3()

                    loop(pw1, pw2, round, nextLoot.add(cw1, cw2))
                }
            } else if (c1.value > c2.value) loop(
                    p1.gain(nextLoot),
                    p2,
                    round + 1
                )
            else loop(
                    p1,
                    p2.gain(nextLoot),
                    round + 1
                )
        }
    }
}

object Solution extends App {
    val n = readLine.toInt // the number of cards for player 1
    val cards1 = (for(i <- 0 until n)
        yield new Card(readLine)
    ).toList
    val m = readLine.toInt // the number of cards for player 2
    val cards2 = (for(i <- 0 until m)
        yield new Card(readLine)
    ).toList
    
    println(new Game(cards1, cards2).loop())
}
