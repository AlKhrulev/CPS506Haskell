--methods to consider: determineSuit
--determine Flush, etc.
--determine winner
--Royal Flush>Straigh Flush>Four of a Kind>Full House>Flush>Straight>Three of a kind>Two pair>
--Pair>High Card

--Royal Flush:A, K, Q, J, 10, all the same suit. DONE
--Straight Flush:Five cards in a sequence, all in the same suit. DONE
--Four of a kind:All four cards of the same rank. DONE
--Full house: Three of a kind with a pair. DONE
--Flush:Any five cards of the same suit, but not in a sequence. DONE
--Straight:Five cards in a sequence, but not of the same suit. (isInSequence and a Flush) DONE
--Three of a Kind:Three cards of the same rank. DONE
--Two pairs:Two different pairs.  DONE
--Pair:Two cards of the same rank. DONE
--High Card:When you haven't made any of the hands above, the highest card plays. DONE
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
        let specialCase = [0,9,10,11,12]
        if newList==reducedHand || specialCase==reducedHand
            then True else False


    checkRoyalFlush::[Integer]->Bool
    checkRoyalFlush hand=do
        let reference_suit=determineSuitValueHelper (head hand) --retrieves a first suit
        let reducedHand=map (retrieveCardValue) hand
        if all(\card -> determineSuitValueHelper(card)==reference_suit) hand && reducedHand==[1,10,11,12,13]
            then True else False


    checkFlush::[Integer]->Bool
    checkFlush hand=do
        let reference_suit=determineSuitValueHelper (head hand) --retrieves a first suit
        if all(\card -> determineSuitValueHelper(card)==reference_suit) hand 
            then True else False

    getHighCard::[Integer]->Integer
    getHighCard hand = do
        let reducedHand = map (\x -> x `mod` 13) hand
        if (head reducedHand) == 0
            then 13
            else if (head reducedHand) == 1
                then 14
                else last hand

    checkStraightFlush::[Integer]->Bool
    checkStraightFlush hand=if isInSequence(hand) && checkFlush(hand) then True else False

    checkStraight::[Integer]->Bool
    checkStraight hand=if isInSequence(hand) then True else False

    --returns a # of times element is present in a list
    getFrequency _ [] = 0
    getFrequency x list = (length.filter(== x)) list

    checkFourofAKind::[Integer]->Bool
    checkFourofAKind hand=do
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        if any (4==) frequencyHand then True else False


    checkThreeofAKind::[Integer]->Bool
    checkThreeofAKind hand=do --if there are 3 equal cards and not a Fullhouse
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        if (any (3==) frequencyHand) && not (checkFullhouse hand) then True else False


    checkFullhouse::[Integer]->Bool
    checkFullhouse hand=do
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        if (any (3==) frequencyHand) && (any (2==) frequencyHand) then True else False


    getOnePair hand = do
        let reducedHand = map (\x -> x `mod` 13) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let pair = filter (\x -> snd x == 2) zipped
        if length pair == 2
            then True
            else False
        -- pair
        -- fst (head pair)

    getTwoPair hand = do
        let reducedHand = map (\x -> x `mod` 13) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let pairs = filter (\x -> snd x == 2) zipped
        if length pairs == 4
            then True
            else False

        