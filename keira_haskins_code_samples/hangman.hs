{-# LANGUAGE LambdaCase #-}
import Control.Monad
import Data.Char
import Data.List
import Data.Bool
import System.IO
import System.Random
import Text.Printf
import Text.Read

-----------------------------------------------------------------
--
-- Author   :: Keira Haskins
-- Date     :: 08-25-2017
-- Class    :: CS 456 -> Advanced Declarative Programming
-- Synopsis :: Play a game of hangman, based on the C code
--             Taken from code by Graphix, who took from:
--             http://www.hangman.symbolwebdesign.nl
--
-----------------------------------------------------------------

showLogo = do
  putStrLn "--------------------------------------------"
  putStrLn "| #  #   #   #   #  #### #   #   #   #   # |"
  putStrLn "| #  #  # #  ##  # #     ## ##  # #  ##  # |"
  putStrLn "| #### ##### # # # #  ## # # # ##### # # # |"
  putStrLn "| #  # #   # #  ## #   # #   # #   # #  ## |"
  putStrLn "| #  # #   # #   #  ###  #   # #   # #   # |"
  putStrLn "--------------------------------------------\n"

-- Print the gallows given the number of missed characters.
prn_galg x =
  case x of
    0 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn ""
        putStrLn ""
        putStrLn ""
        putStrLn ""
        putStrLn ""
        putStrLn ""
        putStrLn "____________\n"
    1 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn ""
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "__|_________\n"
    2 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn "  _______"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "__|_________\n"
    3 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn "  _______"
        putStrLn "  |/"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "__|_________\n"
    4 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn "  _______"
        putStrLn "  |/   | "
        putStrLn "  |    O "
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "  |"
        putStrLn "__|_________\n"
    5 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn "  _______"
        putStrLn "  |/   | "
        putStrLn "  |    O "
        putStrLn "  |    | "
        putStrLn "  |    | "
        putStrLn "  |"
        putStrLn "__|_________\n"
    6 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn "  _______"
        putStrLn "  |/   | "
        putStrLn "  |    O "
        putStrLn "  |   \\|"
        putStrLn "  |    | "
        putStrLn "  |"
        putStrLn "__|_________\n"
    7 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn "  _______"
        putStrLn "  |/   | "
        putStrLn "  |    O "
        putStrLn "  |   \\|/"
        putStrLn "  |    | "
        putStrLn "  |"
        putStrLn "__|_________\n"
    8 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn "  _______"
        putStrLn "  |/   | "
        putStrLn "  |    O "
        putStrLn "  |   \\|/"
        putStrLn "  |    | "
        putStrLn "  |   /  "
        putStrLn "__|_________\n"
    9 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn "  _______"
        putStrLn "  |/   | "
        putStrLn "  |    O "
        putStrLn "  |   \\|/"
        putStrLn "  |    | "
        putStrLn "  |   / \\"
        putStrLn "__|_________\n"
    10 ->
      do
        let s = "Amount of wrong letters: " ++ s ++ "\n" where s = show x
        putStrLn s
        putStrLn "  _______"
        putStrLn "  |/   | "
        putStrLn "  |    X "
        putStrLn "  |   \\|/"
        putStrLn "  |    | "
        putStrLn "  |   / \\"
        putStrLn "__|_________\n"

-- replace "aardvark" 'a' "meh" -> "mehmehrdvmehrk"
replace :: String -> Char -> String -> String
replace xs c s = reverse $ foldr fun [] xs
  where fun x acc = if x == c then acc ++ (reverse s)
                              else acc ++ [x]

-- lower "AAABBBC" -> "aaabbbc"
lower :: [Char] -> [Char]
lower xs = [toLower x | x <- xs, isAlpha x]

-- select even [1..26] "abcdefghijklmnopqrstuvwxyz" => "bdfhjlnprtvxz"
select :: (t1 -> Bool) -> [t1] -> [t] -> [t]
select prd xs ys = [y | (x,y) <- zip xs ys, prd x]

-- findRepChar "fireball" "........" 'l' -> "......ll"
findRepChar :: Eq b => [b] -> [b] -> b -> [b]
findRepChar word dotword chr =
  map snd (map (\(x,y) -> if x == chr then (x,chr) else (x,y)) (zip word dotword))

-- Recursive function used to actually run/play the game.
-- Takes the word to be guessed, the current/dot word, errors,
-- n steps and the previous characters if the user specifies
-- more than a single character.
hangman :: [Char] -> [Char] -> Int -> Int -> [Char] -> IO()
hangman guessWord currentWord errors n [] = do
  let n' = n
  if (guessWord /= currentWord) && (errors < 10) then do
    printf "%d.     %s" n "Enter the letter(s) you want to guess: "
    guess <- getLine
    hangman guessWord currentWord errors n guess
  else
    hangman guessWord currentWord errors n "a"
hangman guessWord currentWord errors n last = do
  if (guessWord /= currentWord) && (errors < 10) then do
    let guess = lower last
    if and (map isAlpha guess) then do
      let currentWord' = findRepChar guessWord currentWord (head guess)
      if (head guess) `elem` guessWord then do
        putStrLn "\nThat letter was correct.\n"
        printf "%s%s\n\n" "The word including the letters you guessed: " currentWord'
        prn_galg errors
        hangman guessWord currentWord' errors (n + 1) (tail last)
      else do
        putStrLn "\nThat letter was incorrect.\n"
        printf "%s%s\n\n" "The word including the letters you guessed: " currentWord
        prn_galg (errors + 1)
        hangman guessWord currentWord' (errors + 1) (n + 1) (tail last)
    else do
      putStrLn "Only alphanumeric symbols are allowed (a-z, A-Z), try again:"
      hangman guessWord currentWord errors n ""
  else do
    putStrLn "---------------"
    putStrLn "--- Results ---"
    putStrLn "---------------\n"
    if guessWord == currentWord then
      putStrLn "Congratulations you guessed the right word!\n"
    else
      printf "You guessed the wrong word. The word was %s. Better luck next time!\n" guessWord

main :: IO()
main = do
  showLogo
  contents   <- readFile "words.txt"
  let content = replace contents '|' " "
  let txtls   = words content
  x          <- randomRIO (0,length txtls)
  let
    errors    = 0
    n         = 1 :: Int
    guessWord = head (select (== x) [0..(length txtls) - 1] txtls)
    currentWord = concat [z ++ "." | let z = "", r <- [1..(length guessWord)]]
  printf "%s\n\n%s\n%s\n%s\n%s\n\n%s%s\n\n"
         "Welcome to the game Hangman!"
         "The objective in this game is to guess the word."
         "You can enter both uppercase and lowercase letters."
         "If you think you know the word, you can type it in."
         "You will lose if you have guessed 10 letters wrong."
         "This is the word you need to guess: "
         currentWord

  hangman guessWord currentWord errors n ""
