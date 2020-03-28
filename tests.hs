module Tests where
    import Poker
    testcase x
        |x==0 = [14, 48, 46, 44, 16] -- high card
        |x==1 = [52, 26, 32, 15, 3] -- one pair
        |x==2 = [12, 51, 43, 30, 1] -- two pair
        |x==3 = [8, 47, 34, 50, 31] -- three of a kind
        |x==4 = [2, 29, 17, 5, 45] -- straight
        |x==5 = [13, 10, 9, 7, 6] -- flush
        |x==6 = [10, 49, 23, 28, 41] -- full house
        |x==7 = [11, 24, 37, 50, 40] -- four of a kind
        |x==8 = [18, 19, 20, 21, 22] -- straight flush
        |x==9 = [36, 37, 38, 39, 27] -- royal flush
        |otherwise = [1,2,3,4,5] -- general case
    
    test x y = do
        let hand1 = Poker.sortHand (testcase x)
        let hand2 = Poker.sortHand (testcase y)
        let winner = Poker.determineWinner hand1 hand2
        Poker.convert winner