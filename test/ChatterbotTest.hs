-- Tests for the Chatterbot module
import Chatterbot
import Test.HUnit

-- In this file, the operator "~?=" tests if the two values are equal.

-- Test reflections work
reflectTest =
  test [
    reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
        ~?= ["you", "will", "never", "see", "your", "reflection", "in", "my", "eyes"]
  ]

transformations = [(stringToPattern "*" "I hate *",
                    stringToPattern "*" "Why do you hate * ?")]

-- Test with several options
testMember v options = test $ assertBool message $ v `elem` options
  where message = show (v) ++ " not part of: " ++ show options

-- Testing rulesApply with several cases
rulesApplyTest =
  test [
    rulesApply transformations (words "I hate my mother")
      ~?= (words "Why do you hate your mother ?"),
    rulesApply transformations (words "ARGH!") `testMember` [(words "ARGH!"), (words "")],
    rulesApply [(stringToPattern "*" "I need *", stringToPattern "*" "Why do you need * ?")]
               (words "I need waffles")
               ~?= (words "Why do you need waffles ?")
  ]

-- Test reduce
reduceTest =
  test [
    (reduce.words) "can you please tell me what Haskell is" ~?= words "what is Haskell",
    (reduce.words) "i am very very tired" ~?= words "i am tired"
  ]

substituteTest =
  test [
    substitute (mkPattern 'x' "3*cos(x) + 4 - x") "5.37" ~?= "3*cos(5.37) + 4 - 5.37"
  ]

rulesCompileTest =
  test [
    rulesCompile [("I need *", ["Are you sure you need * ?"])]
    ~?= [Rule (stringToPattern "*" "i need *", [stringToPattern "*" "Are you sure you need * ?"])]
  ]

matchTest =
  let match1 wc pat = match (mkPattern wc pat)
  in test [
    match1 'x' "2*x+3" "2*7+3" ~?= Just "7",
    match1 '*' "frodo" "gandalf" ~?= Nothing,
    match1 2 [1,3..5] [1,3..5] ~?= Just [],
    match1 '*' "* and *" "you and me" ~?= Just "you",
    match1 'x' "2*x+3+x" "2*7+3" ~?= Nothing,
    match1 '*' "*do" "bdo" ~?= Just "b",
    match1 '*' "*do" "dobedo" ~?= Just "dobe",
    match1 '*' "*do" "bedobe" ~?= Nothing,
    match1 '*' "" "" ~?= Just [],
    match1 '*' "abba" "" ~?= Nothing,
    match1 '*' "" "abba" ~?= Nothing,
    match1 '*' "a" "a" ~?= Just [],
    match1 '*' "*" "a" ~?= Just "a",
    match1 '*' "*" "abba" ~?= Just "abba",
    match1 '*' "*X*" "aXb" ~?= Just "a",
    match1 '*' "*X*" "aaXbb" ~?= Just "aa"
  ]

-- Test that if we susbstitute first, and match
-- We get what we used for substitution
matchTest2 =
  let substituteAndMatch pat = (match pat) . (substitute pat)
      pat = mkPattern '*' "Hello *, how are you?"
      s = "Robert"
  in substituteAndMatch pat s ~?= Just s

frenchPresentation = ((mkPattern '*' "My name is *"),
                      (mkPattern '*' "Je m'appelle *"))

transformationApplyTest =
  test [
    transformationApply id "My name is Zacharias" frenchPresentation
      ~?= Just "Je m'appelle Zacharias",
    transformationApply id "My shoe size is 45" frenchPresentation
      ~?= Nothing
  ]

swedishPresentation = ((mkPattern '*' "My name is *"),
                       (mkPattern '*' "Mitt namn är *"))

presentations = [frenchPresentation, swedishPresentation]

matchAndTransformTest =
  test [
    matchAndTransform id (mkPattern '*' "My name is *") "My name is Zacharias"
      ~?= Just "Zacharias",
    matchAndTransform reverse (mkPattern '*' "My name is *") "My name is Zacharias"
      ~?= Just "sairahcaZ"
  ]

transformationsApplyTest =
  test [
    transformationsApply id presentations "My name is Zacharias"
      ~?= Just "Je m'appelle Zacharias",
    transformationsApply id (reverse presentations) "My name is Zacharias"
      ~?= Just "Mitt namn är Zacharias",
    transformationsApply id (reverse presentations) "My shoe size is 45"
      ~?= Nothing
  ]

unitTests = runTestTTAndExit $
  test [
    "substitute" ~: Main.substituteTest,
    "match" ~: Main.matchTest,
    "match" ~: Main.matchTest2,
    "matchAndTransform" ~: matchAndTransformTest,
    "transformationApply" ~: transformationApplyTest,
    "transformationsApply" ~: transformationsApplyTest,
    "reflect" ~: reflectTest,
    "rulesApply" ~: rulesApplyTest,
    "reduceTest" ~: reduceTest,
    "rulesCompile" ~: rulesCompileTest
  ]


main = unitTests
