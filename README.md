### Poker Masa Description

Cash Masa has 9 or 5 seats and a possible game status. Seats have a stack and status.
    One can buy-in with a seat for an empty side
    When there is one seat it's in WaitOthers status, when there is two seats both status is WaitNextHand, other buy-ins seat status is WaitNextHand.
    When there is no game, seats can be sitoutNext to remove from their side
    When there is a game, seats can sitoutNext to set their status to SitoutNextHand to be later removed from their side when the game is finished.
When there are at least two seats a game can be dealt, and be played.
    When a game is dealt, seats can apply moves to a game.
    When a game is finished, players stacks are updated from the finished game, players with a SitoutNextHand status are removed from their sides. and other players status is set to WaitNextHand, game status is removed

Masa(
    seats Side.Map[Seat]
      stack Float
      Seat.Status
          WaitOthers WaitNextHand SitoutNextHand Involved
    status: Option[Status]
  ) {

    def players: List[Seat]

    def player(side: Side): Option[Player]

    def isEmpty(side: Side)
    def isFull(side: Side)

    def nbPlayers = players.length

    
    def atLeastTwo = nbPlayers >= 2
    def headsUp = nbPlayers == 2
    def onlyOne = nbPlayers == 1
    def noOne = nbPlayers == 0

    def isGame = status.isDefined
    def noGame = !isGame
    def dealt = status.exists(_ < OneWin)
    def finished = status.exists(_ >= OneWin)

    def dealable = noGame && atLeastTwo

    def sitoutNext(value: Boolean)
    

}

Side
    index

Side.Map[A]: Map[Side, A]

### Poker Game Description

Players have roles Involved, Fold, New-Allin, Old-Allin that take turns to act. Fold and All-in players can't act further. There is a button that decides BB SB and first to act. Next to button is SB and next is BB. First to act on Pre-flop is next to BB, on other rounds it's next to button.
There are Betting rounds Pre-flop flop turn river. 
A betting round ends once all players have acted and all bets are equalized, all bets go in to the running pot or side pots.
When a betting round ends except river, and there are at least two involved players, next betting round begins.
When river betting round ends, involved and all-in players go to showdown.
When a betting round ends while one involved player remain and there are no all-in players, remaining player takes the pot. If there are all-in players they go to showdown.
On a betting round, when no involved players remain all-in players go to showdown.
On a betting round, when the action returns to a player, if facing a full-raise is allowed to raise, otherwise can only call or fold.
At the end of a betting round, players that went all-in create side pots. A side pot consists of amount of wager and players involved.

    Dealer(
        Betting Round
            Pre-flop Flop Turn River

        StackIndex button
        StackIndex turnToAct
        Int lastFullRaise

        List[Pot] sidePots
        Pot runningPot
            Int wager
            List[StackIndex] involved

        List[Stack] stacks
            Role
                Involved Folded Old-Allin New-Allin
            Int stack
            Int recentWager
            Option[Action] lastAction
    ) {

        lazy val StackIndex SB = (button + 1) % stacks.length
        lazy val StackIndex BB = (SB + 1) % stacks.length


        def allIns: Int
        def folds: Int
        def involveds: Int

        def noneInvolved = involveds === 0
        def oneInvolved = involveds === 1
        def allInsExists = allIns > 0

        def allActed: Boolean
        def wagersEqualized: Booelan

        def PotDistribution distributeOne()
        def List[PotDistribution] distributeAll(List[HandValueMagic] handValues)

        def Dealer endRound()
        def Dealer nextRound()
        def Dealer nextTurn()

        def Option[Dealer] check()
        def Option[Dealer] raise()
        def Option[Dealer] fold()
        def Option[Dealer] call()
        def Option[Dealer] allin()
    }

Situation(
    HandDealer handDealer,
    Dealer dealer) {

      lazy val actor = Actor(this)

      lazy val moves: List[Move] = actor.validMoves

      def raiseMove(Raise raise): Option[Move] = actor.validRaise(raise)

      lazy val possibleActs: List[PlayerAct] = moves map _.playerAct

      lazy val BettingRound round = dealer.round

      def nextTurn: Boolean = !nextRound && !end
      def nextRound: Boolean = roundEnd && round !== River && !end

      def roundEnd: Boolean = dealer.allActed && dealer.wagersEqualized

      def showdown: Boolean = if (roundEnd) {
          (round === River && !oneWin) ||
          (dealer.oneInvolved && dealer.allInsExists)
      } else {
          dealer.noneInvolved
      }

      def oneWin: Boolean = dealer.oneInvolved && !dealer.allInsExists
    
      def end: Boolean = showdown || oneWin

      def move(act PlayerAct): Valid[Move]
    }

    Actor(Situation situation) {

    }

    Move(
        playerAct PlayerAct,
        dealerAct DealerAct,
        playerDiff PlayerDiff,
        situationBefore: Situation,
        after: Dealer) {

        def before = situationBefore.dealer

        def situationAfter = Situation(finalizeAfter)

        def finalizeAfter: Dealer = after

    }


    DealerAct
        NextTurn(toAct StackIndex)
        NextRound(toAct StackIndex, middle MiddleCards, runningPot Pot, sidePots List[Pot])
        OneWin(winners Winners)
        Showdown(middle MiddleCards, hands List[Option[Hand]], winners Winners)

    PlayerAct
        Raise(Int to) ~ RR TR HR PR
            ThirdPotRaise
            HalfPotRaise
            PotRaise
        Call ~ CA
        Check ~ CH
        Fold ~ FO
        All-in ~ AA AC AH AF
            AllInCall
            AllInHalfRaise
            AllInFullRaise

    PlayerDiff
        newStack Int
        newWager Int
        newRole Role

    MiddleCards
        Flop Option[List[Card]]
        Turn Option[Card]
        River Option[Card]

    Winners
        pots List[PotDistribution]
        stacks List[Int]

    PotDistribution
      wager Int
      involved List[StackIndex]
      100 0 1 2

    Pot
       wager Int
       involved List[StackIndex]
       100 0 1 2 3

Dealer Visual - Fen

    bettingRound button turnToAct lastFullRaise!runningPot~sidePot
    role stack recentWager lastAction|. 

     (P|F|T|R) 0 0 100!100 0 1 2 3~50 0 1 2
     (I|F|O|N) 100 10 CA
     I 100 10 RR200
     I 100 10 .

Player Act - Uci

     Raise(Int to) ~ RR TR HR PR
         ThirdPotRaise
         HalfPotRaise
         PotRaise
     Call ~ CA
     Check ~ CH
     Fold ~ FO
     All-in ~ AA AC AH AF
         AllIn(Int to)
         AllInCall
         AllInHalfRaise
         AllInFullRaise
