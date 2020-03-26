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
--Straight:Five cards in a sequence, but not of the same suit. (isInSequence and a Flush)
--Three of a Kind:Three cards of the same rank.
--Two pairs:Two different pairs. 
--Pair:Two cards of the same rank.
--High Card:When you haven't made any of the hands above, the highest card plays. 

module Poker where
    -- deal::[Integer]->[Char] --the method that executes other methods
    deal list = distribute list [] []
    
    -- right now they have nowhere else to go so i just put the hands together
    distribute [x, y] hand1 hand2 = do
        let head1 = x
        let head2 = y
        let fhand = hand1 ++ [head1]
        let shand = hand2 ++ [head2]
        fhand ++ shand
    distribute list hand1 hand2 = do
        let head1 = head list
        let head2 = head (tail list)
        let fhand = hand1 ++ [head1]
        let shand = hand2 ++ [head2]
        let listTail = tail(tail list)
        distribute listTail fhand shand

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

    determineSuitValueHelper::Integer->Integer --determines the suit of 1 card and returns a value
    determineSuitValueHelper value
        |value<=13=1
        |value<=26=2
        |value<=39=3
        |value<=52=4

    determineSuit::[Integer]->[(Integer,Char)] --determines the suit of the whole hand
    determineSuit hand=map (determineSuitHelper) hand

    isInSequence::[Integer]->Bool
    isInSequence hand=do--determines if the cards in the hand are in sequence
        let reducedHand=map (\x -> (x-1) `mod` 13) hand
        let headRHand=(head reducedHand)
        let newList=map (\x -> x + headRHand) [0,1,2,3,4]
        if newList==reducedHand
            then True else False


    checkRoyalFlush::[Integer]->[Char]
    checkRoyalFlush hand=do
        let reference_suit=determineSuitValueHelper (head hand) --retrieves a first suit
        if all(\card -> determineSuitValueHelper(card)==reference_suit) hand
            then "the same suits" else "different suits" --TODO finish


    checkFlush::[Integer]->Bool
    checkFlush hand=do
        let reference_suit=determineSuitValueHelper (head hand) --retrieves a first suit
        if all(\card -> determineSuitValueHelper(card)==reference_suit) hand 
            then True else False


    getHighCard hand = do
        let reducedHand = map (\x -> x `mod` 13) hand
        if (isInSequence reducedHand) && (head reducedHand) == 1
            then last reducedHand
            else if (head reducedHand) == 0
                then 13
                else if (head reducedHand) == 1
                    then 14
                    else last reducedHand
    
    getOnePair hand = do
        let reducedHand = map (\x -> x `mod` 13) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let pair = filter (\x -> snd x == 2) zipped
        fst (head pair)

    getTwoPair hand = do
        let reducedHand = map (\x -> x `mod` 13) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let pairs = filter (\x -> snd x == 2) zipped
        if fst (head pairs) == 1
            then
                fst (head pairs)
            else
                fst (last pairs)

    getFrequency _ [] = 0
    getFrequency x list = (length.filter(== x)) list
