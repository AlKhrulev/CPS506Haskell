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
    import Data.List

    -- MAIN COMPONENTS TO GAME --

    -- deal::[Integer]->[Char] --the method that executes other methods
    deal list =do
        let hands = distribute list [] []
        let sortedHand1 = sortHand (fst hands)
        let sortedHand2 = sortHand (snd hands)
        let winner=determineWinner sortedHand1 sortedHand2
        convert winner
    
    -- right now they have nowhere else to go so i just put the hands together
    distribute [x, y] hand1 hand2 = do
        let head1 = x
        let head2 = y
        let fhand = hand1 ++ [head1]
        let shand = hand2 ++ [head2]
        (fhand, shand)
    distribute list hand1 hand2 = do
        let head1 = head list
        let head2 = head (tail list)
        let fhand = hand1 ++ [head1]
        let shand = hand2 ++ [head2]
        let listTail = tail(tail list)
        distribute listTail fhand shand

    sortHand hand = do
        let splitFunc = (\x -> x `rem` 13 == 0)
        let sortedHand = sortBy (\x y -> compare ((x-1) `mod` 13) ((y-1) `mod` 13)) hand
        let kings = takeWhile (\x -> x `rem` 13 == 0)
        let nonKings = dropWhile (\x -> x `rem` 13 == 0)
        sortedHand
        

    convert hand = do
        -- list of tuples, each tuple has a number mapped to a suit
        -- the division of the card value (42, 30, etc) by 13 will result in its suit
        let suits = [(0, "C"), (1, "D"), (2, "H"), (3, "S")]
        -- list of characters in order of the way they correspond to their value mod 13
        -- 13 mod 13 = 0, so king is at start of list, etc
        let values = ["13", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"]
        -- concatenates the value and suit into one string and maps it to a converted hand
        let converted = map (\x -> ( values !! (x `mod` 13) ) ++ (snd ( suits !! ((x-1) `div` 13) )) ) hand
        (sort converted) --sort in terms of ascii and return the result

    --determineRank::[Integer]->Integer
    determineRank hand --determines the rank of the hand
        |checkRoyalFlush(hand)=10
        |checkStraightFlush(hand)=9
        |checkFourofAKind(hand)=8
        |checkFullhouse(hand)=7
        |checkFlush(hand)=6
        |checkStraight(hand)=5
        |checkThreeofAKind(hand)=4
        |getTwoPair(hand)=3
        |getOnePair(hand)=2
        |otherwise=1 --basically the case of the high card


    determineWinner hand1 hand2 --determines the winner
        |determineRank(hand1)>determineRank(hand2)=hand1
        |determineRank(hand1)<determineRank(hand2)=hand2
        |determineRank(hand1)==8=(tieBreakerFourofaKind hand1 hand2) --added from here
        |determineRank(hand1)==7=(tieBreakerFullhouse hand1 hand2)
        |determineRank(hand1)==4=(tieBreakerThreeofaKind hand1 hand2)
        |determineRank(hand1)==3=(tieBreakerTwoPairs hand1 hand2)
        |determineRank(hand1)==2=(tieBreakerOnePair hand1 hand2) --to here
        |getHighCard(hand1)>getHighCard(hand2)=hand1
        |getHighCard(hand2)>getHighCard(hand1)=hand2
        |determineSuitValueHelper(getHighCardValue(hand1))>determineSuitValueHelper(getHighCardValue(hand2))=hand1
        |otherwise=hand2
        --determineSuitValueHelper(getHighCard(hand2))>determineSuitValueHelper(getHighCard(hand1))=hand2
    
    tieBreakerTwoPairs hand1 hand2=do
        let pairs1=getTwoPairValue hand1
        let pairs2=getTwoPairValue hand2
        if fst(pairs1)>fst(pairs2)
            then hand1 
            else if fst(pairs1)<fst(pairs2)
                then hand2
                else if snd(pairs1)>snd(pairs2) 
                    then hand1
                    else if snd(pairs1)<snd(pairs2)
                        then hand2 
                        else if determineSuitValueHelper(fst pairs1)>determineSuitValueHelper(fst pairs2)
                            then hand1
                            else if determineSuitValueHelper(fst pairs2)>determineSuitValueHelper(fst pairs1)
                                then hand2
                                else if determineSuitValueHelper(snd pairs1)>determineSuitValueHelper(snd pairs2)
                                then hand1
                                    else if determineSuitValueHelper(snd pairs2)>determineSuitValueHelper(snd pairs1)
                                    then hand2
                                    else tieBreakerPairsHelper hand1 hand2

    tieBreakerPairsHelper hand1 hand2
        |getHighCard(hand1)>getHighCard(hand2)=hand1
        |getHighCard(hand2)>getHighCard(hand1)=hand2
        |determineSuitValueHelper(getHighCardValue(hand1))>determineSuitValueHelper(getHighCardValue(hand2))=hand1
        |otherwise=hand2


    tieBreakerOnePair hand1 hand2=do
        let pair1=getOnePairValue hand1
        let pair2=getOnePairValue hand2
        if pair1>pair2
            then hand1
            else if pair1<pair2
                then hand2
                else if determineSuitValueHelper(pair1)>determineSuitValueHelper(pair2)
                    then hand1
                    else if determineSuitValueHelper(pair1)<determineSuitValueHelper(pair2)
                        then hand2
                        else tieBreakerPairsHelper hand1 hand2

    tieBreakerThreeofaKind hand1 hand2=do
        let highcard1=getThreeKindValue hand1
        let highcard2=getThreeKindValue hand2
        if highcard1>highcard2
            then hand1
            else if highcard1<highcard2
                then hand2
                else if determineSuitValueHelper(highcard1)>determineSuitValueHelper(highcard2)
                    then hand1
                    else if determineSuitValueHelper(highcard1)<determineSuitValueHelper(highcard2)
                        then hand2
                        else tieBreakerPairsHelper hand1 hand2

    tieBreakerFourofaKind hand1 hand2=do
        let highcard1=getFourKindValue hand1
        let highcard2=getFourKindValue hand2
        if highcard1>highcard2
            then hand1
            else if highcard1<highcard2
                then hand2
                else if determineSuitValueHelper(highcard1)>determineSuitValueHelper(highcard2)
                    then hand1
                    else if determineSuitValueHelper(highcard1)<determineSuitValueHelper(highcard2)
                        then hand2
                        else tieBreakerPairsHelper hand1 hand2

    tieBreakerFullhouse hand1 hand2=do
        let highcard1=getThreeKindValue hand1
        let highcard2=getThreeKindValue hand2
        if highcard1>highcard2
            then hand1
            else if highcard1<highcard2
                then hand2
                else if determineSuitValueHelper(highcard1)>determineSuitValueHelper(highcard2)
                    then hand1
                    else if determineSuitValueHelper(highcard1)<determineSuitValueHelper(highcard2)
                        then hand2
                        else tieBreakerOnePair hand1 hand2

    -- HELPER FUNCTIONS --

    --retrieveCardValue::Integer->Integer --returns a card value in a range of 1-13
    retrieveCardValue value
        |value<=13=value
        |otherwise=retrieveCardValue (value-13)

    --retrieveSuitsList::[Char]->[(Integer,Char)]->[Char] --retrieves a list of suites
    retrieveSuitsList acc list --retrieves a list of suites
        |list==[]=acc
        |otherwise=do
            let newAcc=acc++[(snd (head list))]
            retrieveSuitsList newAcc (tail(list))


    --determineSuitHelper::Integer->(Integer,Char) --determines the suit of 1 card and returns a tuple
    determineSuitHelper value
        |value<=13=(value,'C')
        |value<=26=(value,'D')
        |value<=39=(value,'H')
        |value<=52=(value,'S')

    --determineSuitValueHelper::Integer->Integer --determines the suit of 1 card and returns a value
    determineSuitValueHelper value
        |value<=13=1
        |value<=26=2
        |value<=39=3
        |value<=52=4

    --determineSuit::[Integer]->[(Integer,Char)] --determines the suit of the whole hand
    determineSuit hand=map (determineSuitHelper) hand

    --isInSequence::[Integer]->Bool
    isInSequence hand=do--determines if the cards in the hand are in sequence
        let reducedHand=map (\x -> (x-1) `mod` 13) hand
        let headRHand=(head reducedHand)
        let newList=map (\x -> x + headRHand) [0,1,2,3,4]
        let specialCase = [0,9,10,11,12]
        if newList==reducedHand || specialCase==reducedHand
            then True else False

    --returns a # of times element is present in a list
    getFrequency x list = (length.filter(== x)) list

    -- get pair value
    getOnePairValue hand = do
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let pair = filter (\x -> snd x == 2) zipped
        fst (head pair)

    -- get two pair values
    getTwoPairValue hand = do
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let pairs = filter (\x -> snd x == 2) zipped
        (fst (head pairs), fst (last pairs))

    --get high card three kind
    getThreeKindValue hand = do
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let trips = filter (\x -> snd x == 3) zipped
        fst (head trips)
        
    --get high card four kind
    getFourKindValue hand = do
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let quads = filter (\x -> snd x == 4) zipped
        fst (head quads)


    -- METHODS TO CHECK RANKING OF HAND --

    --getHighCard::[Integer]->Integer
    getHighCard hand = do
        let reducedHand = map (\x -> (x-1) `mod` 13) hand
        if (head reducedHand) /= 0
            then (last reducedHand) + 1
            else if (isInSequence hand) && (last reducedHand) /= 12
                then (last reducedHand) + 1
                else 14
        -- if (head reducedHand) == 0
        --     then 13
        --     else last reducedHand
        
    getHighCardFixed hand = do
        let reducedHand = sort (map (retrieveCardValue) hand)
        if reducedHand==[1,2,3,4,5] then 5 else if reducedHand==[1,10,11,12,13] then 13 
            else if head(reducedHand)==1 then 14 else last(reducedHand)

    getHighCardValue hand = do
        let reducedHand = map (\x -> (x-1) `mod` 13) hand
        if (head reducedHand) /= 0
            then last hand
            else if (isInSequence hand) && (last reducedHand) /= 12
                then last hand
                else head hand

    --getOnePair::[Integer]->Bool
    getOnePair hand = do
        let reducedHand = map (\x -> x `mod` 13) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let pair = filter (\x -> snd x == 2) zipped
        length pair == 2

    --getTwoPair::[Integer]->Bool
    getTwoPair hand = do
        let reducedHand = map (\x -> x `mod` 13) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        let zipped = zip hand frequencyHand
        let pairs = filter (\x -> snd x == 2) zipped
        length pairs == 4
    
    --checkThreeofAKind::[Integer]->Bool
    checkThreeofAKind hand=do --if there are 3 equal cards and not a Fullhouse
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        if (any (3==) frequencyHand) && not (checkFullhouse hand) then True else False
        
    --checkStraight::[Integer]->Bool
    checkStraight hand=if isInSequence(hand) && not(checkFlush(hand)) then True else False

    --checkFlush::[Integer]->Bool
    checkFlush hand=do
        let reference_suit=determineSuitValueHelper (head hand) --retrieves a first suit
        if all(\card -> determineSuitValueHelper(card)==reference_suit) hand 
            then True else False

    --checkFullhouse::[Integer]->Bool
    checkFullhouse hand=do
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        if (any (3==) frequencyHand) && (any (2==) frequencyHand) then True else False
        
    --checkFourofAKind::[Integer]->Bool
    checkFourofAKind hand=do
        let reducedHand = map (retrieveCardValue) hand
        let frequencyHand = map (\x -> getFrequency x reducedHand) reducedHand
        if any (4==) frequencyHand then True else False

    --checkStraightFlush::[Integer]->Bool
    checkStraightFlush hand=if isInSequence(hand) && checkFlush(hand) then True else False

    --checkRoyalFlush::[Integer]->Bool
    checkRoyalFlush hand=do
        let reference_suit=determineSuitValueHelper (head hand) --retrieves a first suit
        let reducedHand=map (retrieveCardValue) hand
        if all(\card -> determineSuitValueHelper(card)==reference_suit) hand && reducedHand==[1,10,11,12,13]
            then True else False