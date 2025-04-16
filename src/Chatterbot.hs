-- Ómar Bessi Ómarsson
-- Veronika Juzkova

module Chatterbot where
import Utilities ( map2, mmap, orElse, pick )
import System.Random (randomIO)
import Data.Char
import Data.Maybe

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a = Wildcard | Item a
  deriving (Eq, Show)

-- A pattern is a list of pattern elements
newtype Pattern a = Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string
type Phrase = [String]

newtype Rule = Rule (Pattern String, [Template String])
  deriving (Eq, Show)

type BotBrain = [Rule]

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()


--------------------------------------------------------

-- This takes a brain, and returns a function
-- Which will take a phrase as an input and calculate the result
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b =
  fmap rulesApply (mapM makePair b)

-- A rule maps a pattern to many answers, so we choose one
-- at random, and that's our bot
makePair :: Rule -> IO (Pattern String, Template String)
makePair (Rule (pattern, list)) = do
  rand_guy <- randomIO :: IO Float
  return (pattern, pick rand_guy list)
    

rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
 -- 1. Match the phrase with a pattern.
 -- 2. Reflect the match.
 -- 3. Substitute the match in the target pattern.
rulesApply list sentence = 
 case maybe_cool_thing of
  Nothing -> ["ARGH!"]
  _ -> just_cool_thing
  where 
    Just just_cool_thing = maybe_cool_thing
    maybe_cool_thing = transformationsApply reflect list sentence

reflect :: Phrase -> Phrase
reflect = map (reflectWord reflections)

reflectWord :: [(String, String)] -> String -> String
reflectWord [] word = word
reflectWord ((first_person, second_person):xs) word
  |word == first_person = second_person
  |otherwise = reflectWord xs word

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),

    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
ruleCompile (wannabe_pattern, wannabe_templates) = Rule (starPattern lowered_wannabe_p, map starPattern wannabe_templates)
  where 
    lowered_wannabe_p = map toLower wannabe_pattern

--------------------------------------


-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: Eq a => a -> [a] -> Pattern a
mkPattern _ [] = Pattern []
mkPattern checker (x:xs)
  | x == checker = let (Pattern rest) = mkPattern checker xs
    in Pattern (Wildcard : rest)
  | otherwise    = let (Pattern rest) = mkPattern checker xs
    in Pattern (Item x : rest)


-- >>> mkPattern '*' "Hi *!"


stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words

starPattern :: String -> Pattern String
starPattern = stringToPattern "*"

reductions :: [(Pattern String, Pattern String)]
reductions = (map . map2) (starPattern, starPattern)
  [ ( "please *", "*" ),
    ( "could you *", "*" ),
    ( "can you *", "*"),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
reductionsApply pairs original  =
  case transformationsApply id pairs original of
    Nothing -> original
    _ -> reductionsApply pairs (func original)
    where 
      func = unwrapMaybe . transformationsApply id pairs

unwrapMaybe :: Maybe a -> a
unwrapMaybe maybe = r
  where Just r = maybe

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a template with the list given as the third argument
substitute :: Template a -> [a] -> [a]
substitute (Pattern elems) replacement = go elems
  where
    go [] = []
    go (Item x : xs) = x : go xs
    go (Wildcard : xs) = replacement ++ go xs


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern ps) xs of
    Nothing -> Nothing
    Just _  -> Just [x]

longerWildcardMatch (Pattern (Wildcard:ps)) (x:xs) =
  case match (Pattern (alice:Wildcard:ps)) (x:xs) of
    Nothing -> Nothing
    Just ys -> Just (x:ys)
    where alice = Item x


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => Pattern a -> [a] -> Maybe [a]

{- PART 1, either is empty-}
match (Pattern []) [] = Just [] --Both
match (Pattern []) _ = Nothing --Empty Pattern
match (Pattern (Wildcard:_)) [] = Nothing --Empty input w wildcard
match (Pattern (_:_)) [] = Nothing --Empty input wo wildcard

{- Part 2; Non-empty, no wildcard -}
match (Pattern (Item x:xs)) (t:ts)
  |x /= t = Nothing
  |x == t = match (Pattern xs) ts 

{- Part 3; Wildcards.-}
match (Pattern (Wildcard:ps)) xs =
  Utilities.orElse
    (singleWildcardMatch (Pattern (Wildcard:ps)) xs)
    (longerWildcardMatch (Pattern (Wildcard:ps)) xs)

-- >>> match (mkPattern '*' "I hate *") "I hate bad waffles"
-- Just "bad waffles"

-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------

-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: Eq a => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat)

-- Applying a single pattern
transformationApply :: Eq a => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
-- Empty pattern
transformationApply given_func string (p, Pattern []) =
  case match p string of
    Just _  -> Just []
    Nothing -> Nothing

-- Item
transformationApply given_func string (p,Pattern ((Item x):xs)) 
  | isNothing s0 = Nothing
  | otherwise = Just (x:s1)
  where 
    s0 = transformationApply given_func (given_func string) (p, Pattern xs) 
    (Just s1) = transformationApply given_func (given_func string) (p, Pattern xs) 

-- Wildcard    
transformationApply given_func string (p,Pattern (Wildcard:xs)) 
  | isNothing (match p string) = Nothing
  | otherwise = Just(given_func replacement++s)
  where
    (Just s) = transformationApply given_func (given_func string) (p, Pattern xs)
    Just replacement = match p string


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
transformationsApply string_func [] sentence = Nothing

transformationsApply string_func ((p,t):xs) sentence =
  case transformationApply string_func sentence (p, t) of
    Just result -> Just result
    Nothing     -> transformationsApply string_func xs sentence