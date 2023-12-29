import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import GLua.Position (LineColPos (..), Region (..))
import qualified GLuaFixer.Interface as GLua
import GLuaFixer.LintMessage (
  Issue (..),
  LintMessage (..),
  Severity (..),
 )
import GLuaFixer.LintSettings (LintSettings, defaultLintSettings)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Unit tests"
    [ testQuotedStringWarningPosition
    , testEmptyIfWarningPosition
    ]

-- | Regression test for https://github.com/FPtje/GLuaFixer/issues/169
testQuotedStringWarningPosition :: TestTree
testQuotedStringWarningPosition =
  testCase "The syntax inconsistency warning is thrown and in the right region" $
    let
      input = "bar = a or b\nfoo = \"\" and \"\" and \"dddd\" || \"[]\""
      expectedRegion1 = Region (LineColPos 0 8 8) (LineColPos 0 10 10)
      warning1 = SyntaxInconsistency "||" "or"
      msg1 = LintMessage LintWarning expectedRegion1 warning1 testFilePath

      expectedRegion2 = Region (LineColPos 1 27 40) (LineColPos 1 29 42)
      warning2 = SyntaxInconsistency "||" "or"
      msg2 = LintMessage LintWarning expectedRegion2 warning2 testFilePath
    in
      [msg1, msg2] @=? lintString input

-- | Regression test for https://github.com/FPtje/GLuaFixer/issues/170
testEmptyIfWarningPosition :: TestTree
testEmptyIfWarningPosition = do
  testCase "The empty-if-statement is thrown and in the right region for if-statements" $
    let
      inputEmptyIf = "if true then end"
      inputWithElse = "if true then\nelse print(1) end"
      expectedRegionEmptyIf = Region (LineColPos 0 0 0) (LineColPos 0 16 16)
      expectedRegionWithElse = Region (LineColPos 0 0 0) (LineColPos 1 0 13)
      warning = EmptyIf
      msgEmptyIf = LintMessage LintWarning expectedRegionEmptyIf warning testFilePath
      msgWithElse = LintMessage LintWarning expectedRegionWithElse warning testFilePath
    in
      do
        lintString inputEmptyIf @=? [msgEmptyIf]
        lintString inputWithElse @=? [msgWithElse]

-- | Helper to lint a string
lintString :: String -> [LintMessage]
lintString input =
  let
    settings = defaultLintSettings
  in
    case GLua.lex settings testFilePath input of
      Left errs -> errs
      Right mTokens -> case GLua.parse settings testFilePath mTokens of
        Left errs -> errs
        Right ast -> GLua.lexiconLint testFilePath settings mTokens <> GLua.astLint testFilePath settings ast

testFilePath :: String
testFilePath = "test input"
