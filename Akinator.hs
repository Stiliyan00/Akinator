{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Akinator where
import Distribution.Compat.Directory (makeAbsolute)
import System.Win32.NLS (lOCALE_SMONDECIMALSEP)
import Control.Arrow (ArrowChoice(left))
import System.Directory (doesPathExist)
import Text.Read (readMaybe)
---ВАЖНО! Не пишеше в условието,че първоначалната база от данни трябва да е произволна
--затова аз съм си направил предварително подготвена при която съм приел, че дърворо ми няма връх с излизащо само
--едно ребро, т.е. за всеки въпрос има 2 отговора и повреме и след играта това не се нарушава 

--ВАЖНО! Играта се стартита с функцията startGame

data BinTree a
    = EmptyTree
    | Node a (BinTree a) (BinTree a)
    deriving(Show, Eq, Read)

isLeaf :: BinTree a -> Bool
isLeaf EmptyTree = False
isLeaf (Node a EmptyTree EmptyTree) = True
isLeaf _ = False

getRoot :: BinTree String -> String
getRoot EmptyTree = ""
getRoot (Node root left right) = root

makeEdge1 :: String -> BinTree String
makeEdge1 a = Node a EmptyTree EmptyTree

makeEdge :: String -> String -> String -> BinTree String
makeEdge root left right = Node root (makeEdge1 left) (makeEdge1 right)

fileName :: String
fileName = "tree.txt"

startGame :: IO String
startGame = do  d <- doesPathExist fileName
                if d == True then do
                        myTree <- readFile fileName
                        play1 myTree
                else return "NO SUCH FILE"

play1 :: String -> IO String
play1 x = let
    tree = readMaybe x :: Maybe (BinTree String)
    in playHelper tree x

playHelper :: Maybe (BinTree String) -> String -> IO String
playHelper tree x
    |tree == Nothing  = return "INVALID INPUT! THIS IS NOT A TREE!"
    |otherwise = play tree x

play :: Maybe (BinTree String) -> String -> IO String
play Nothing _ = return ""
play (Just EmptyTree) _ = return ""
play (Just (Node root left right)) stringTree = do  putStrLn root
                                                    ans <- getLine
                                                    case ans of
                                                        "yes" -> questionCase right stringTree
                                                        "no"  -> questionCase left stringTree
                                                        _     -> do putStrLn("INVALID VALUE! PLEASE ENTER yes OR no !")
                                                                    play (Just (Node root left right)) stringTree

questionCase :: BinTree String -> String -> IO String
questionCase EmptyTree stringTree = play (Just EmptyTree) stringTree
questionCase (Node root left right) stringTree
    |(isLeaf (Node root left right)) = guessCase (Node root left right) stringTree
    |otherwise = play (Just (Node root left right)) stringTree

guessCase :: BinTree String -> String -> IO String
guessCase EmptyTree _ = return "Error" -- impossible case
guessCase (Node root left right) stringTree = do    putStrLn ("Is your animal " ++ root ++ "?")
                                                    ans <- getLine
                                                    case ans of
                                                        "yes" -> loseCase stringTree
                                                        "no"  -> winCase stringTree root -- <- ---------------
                                                        _     -> do putStrLn("INVALID VALUE! PLEASE ENTER yes OR no !")
                                                                    guessCase (Node root left right) stringTree

loseCase :: String -> IO String
loseCase stringTree = do    putStrLn "You lost the game! Would you like to play again?"
                            ans <- getLine
                            case ans of
                                "yes" -> startGame
                                "no"  -> return "Ok. Have a nice day"
                                _     -> do putStrLn("INVALID VALUE! PLEASE ENTER yes OR no !")
                                            loseCase stringTree

winCase :: String -> String -> IO String
winCase stringTree root = do    putStrLn("I surrender, what is the answer?")
                                ans <- getLine
                                putStrLn ("How can I tell it apart")
                                newQuestion <- getLine
                                addInformation stringTree newQuestion root ans
                                putStrLn "Ok i have added the info :D   Would you like to play again?"
                                secondAns <- getLine
                                case secondAns of
                                    "yes" -> startGame
                                    "no"  -> return "Ok. Have a nice day."
                                    _     -> do putStrLn("INVALID VALUE! PLEASE ENTER yes OR no !")
                                                winCase stringTree root


addInformation :: String  -> String -> String -> String -> IO()
addInformation stringTree newQuestion root ans = do putStrLn("For which one the answer is yes?")
                                                    tAns <- getLine
                                                    if(tAns == root || tAns == ans) then do
                                                        if (tAns == root) then do
                                                            addInfo stringTree root (makeEdge newQuestion ans root)
                                                        else do addInfo stringTree root (makeEdge newQuestion root ans)
                                                    else do putStrLn("INVALID VALUE! PLEASE ENTER " ++ root ++ " OR " ++ ans ++ "!")
                                                            addInformation stringTree newQuestion root ans

addInfo :: String -> String -> BinTree String -> IO()
addInfo strTree prevAns newEdge = let
    tree = read strTree :: BinTree String
    in addInfoHelper tree prevAns newEdge

addInfoHelper :: BinTree String -> String -> BinTree String -> IO()
addInfoHelper tree prevAns newEdge = do writeFile fileName (show (findAndReplace tree prevAns newEdge))

--findAndReplace - функция за намиране на връх (листо) в дървото и замяната му с нов връх от вида
--   въпрос
--  /      \
--prevAns newAns

--дърво + (Node "нов въпрос" ( Node "Нов отговор" EmptyTree EmptyTree) (Node "стар въпрос" EmptyTree EmptyTree) )
--или
--дърво + (Node "нов въпрос" ( Node "стар въпрос" EmptyTree EmptyTree) (Node "Нов отговор" EmptyTree EmptyTree) )
findAndReplace :: BinTree String -> String -> BinTree String -> BinTree String
findAndReplace EmptyTree _ _ = EmptyTree
findAndReplace (Node root left right) ans newEdge
    |root == ans = newEdge
    |otherwise =  Node root (findAndReplace left ans newEdge) (findAndReplace right ans newEdge)

