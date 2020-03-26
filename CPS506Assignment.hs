--methods to consider: determineSuit
--determine Flush, etc.
--determine winner
--Royal Flush>Straigh Flush>Four of a Kind>Full House>Flush>Straight>Three of a kind>Two pair>
--Pair>High Card

--Royal Flush:A, K, Q, J, 10, all the same suit. 
--Straight Flush:Five cards in a sequence, all in the same suit. 
--Four of a kind:All four cards of the same rank. 
--Full house: Three of a kind with a pair. 
--Flush:Any five cards of the same suit, but not in a sequence. 
--Straight:Five cards in a sequence, but not of the same suit. 
--Three of a Kind:Three cards of the same rank.
--Two pairs:Two different pairs. 
--Pair:Two cards of the same rank.
--High Card:When you haven't made any of the hands above, the highest card plays. 
module Poker where
    deal::[Integer]->[Char] --the method that executes other methods
    deal list=['a','b','c']


    retrieveCardValue::Integer->Integer --returns a card value in a range of 1-13
    retrieveCardValue value
        |value<=13=value
        |otherwise=retrieveCardValue (value-13)

    --retrieveSuitsList::[Char]->[(Integer,Char)]->[Char] --retrieves a list of suites
    retrieveSuitsList acc list --retrieves a list of suites
        |list==[]=acc
        |otherwise=do
            let newAcc=acc++[(snd (head list))]
            retrieveSuitsList newAcc (tail(list))


    determineSuitHelper::Integer->(Integer,Char) --determines the suit of 1 card and returns a tuple
    determineSuitHelper value
        |value<=13=(value,'C')
        |value<=26=(value,'D')
        |value<=39=(value,'H')
        |value<=52=(value,'S')

    determineSuit::[Integer]->[(Integer,Char)] --determines the suit of the whole hand
    determineSuit hand=map (determineSuitHelper) hand

    --checkRoyalFlush::[(Integer,Char)]->Bool
    --checkRoyalFlush hand=do
        --let reference_suit=snd (head(hand)) --retrieves a first suit
        --let suitsList=retrieveSuitsList([],hand)
        --if (foldr (==) reference_suit suitsList) --this part doesn't work yet!
            --then "Works!"


--remainder of card_number/13 gives you a suit