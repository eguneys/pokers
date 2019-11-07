###


Stacks take turns to act. Fold and All-in drops from acting further. There is a button that decides BB SB and first to act. Next to button is SB and next is BB. First to act on Pre-flop is next to BB, on other rounds it's next to button.
There are Betting rounds Pre-flop flop turn river. Each betting round ends once all players have acted and all bets are equalized, all bets go in to the middle pot, and the next betting round begins. After river ends middle pot is distributed.
On a betting round, when the action returns to a player, if facing a full-raise is allowed to raise, otherwise can only call or fold.

At the end of a betting round players that went all-in create side pots. A side pot consists of amount of wager and players involved.

    Dealer(
        Int blinds
        Betting Round
            Pre-flop Flop Turn River

        StackIndex button
        StackIndex turnToAct
        StackIndex allowRaiseUntil
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

        lazy val StackIndex nextToAct()
    }

Pot
    wager involved
    100 0 1 2 3

Dealer

     blinds bettingRound button turnToAct allowRaiseUntil lastFullRaise!runningPot~sidePot
     role stack recentWager lastAction|. 

     100 (P|F|T|R) 0 0 0 100!100 0 1 2 3~50 0 1 2
     (I|F|O|N) 100 10 C
     I 100 10 R200
     I 100 10 .
