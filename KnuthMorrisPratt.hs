import Data.List (isPrefixOf, isInfixOf)
import Test.QuickCheck

data Alpha = A | B | C deriving (Eq, Show, Ord)

instance Arbitrary Alpha where
  arbitrary = elements [A, B, C]

data Tree a = Nil | Node { top :: a, next :: Tree a, rest :: Tree a }

mp pattern text = any done (scanl step init text)
  where make :: [Alpha] -> Tree [Alpha] -> Tree [Alpha]
        make [] r = Node [] Nil r
        make t r  = Node t n r
          where n = make (tail t) (step r (head t))
        init :: Tree [Alpha]
        init = make pattern Nil
        step :: Tree [Alpha] -> Alpha -> Tree [Alpha]
        step Nil x = init
        step acc@(Node t n r) x
          | check acc x = n
          | otherwise   = step r x

kmp pattern text = any done (scanl step init text)
  where make :: [Alpha] -> Tree [Alpha] -> Tree [Alpha]
        make [] r = Node [] Nil r
        make t r  = Node t n r'
          where n  = make (tail t) (step r (head t))
                r' = if check r (head t) then rest r else r
        init :: Tree [Alpha]
        init = make pattern Nil
        step :: Tree [Alpha] -> Alpha -> Tree [Alpha]
        step Nil x = init
        step acc@(Node t n r) x
          | check acc x = n
          | otherwise   = step r x

check Nil x = False
check acc x = isPrefixOf [x] (top acc)

done Nil = False
done acc = (top acc) == []

main = do { quickCheck (\x y -> not (isInfixOf (x :: [Alpha]) y) ==> not (mp x y))
          ; quickCheck (\x y -> not (isInfixOf (x :: [Alpha]) y) ==> not (kmp x y))
          ; quickCheck (\x y -> isInfixOf (x :: [Alpha]) y ==> mp x y)
          ; quickCheck (\x y -> isInfixOf (x :: [Alpha]) y ==> kmp x y)
          }
