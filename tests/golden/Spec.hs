import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified GLuaFixer.Interface as GLua
import GLuaFixer.LintSettings (LintSettings, defaultLintSettings)
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFile, goldenVsString)

main :: IO ()
main = defaultMain =<< goldenTests

data TestPair = TestPair {inputFile :: FilePath, outputFile :: FilePath}

goldenTests :: IO TestTree
goldenTests = do
  inputFiles <- findByExtension [".lua"] "tests/golden/data/input"
  expectedOutputFiles <- findByExtension [".lua"] "tests/golden/data/output"
  let
    testPairs = zipWith TestPair inputFiles expectedOutputFiles

  pure $
    testGroup
      "Golden pretty print tests"
      [ goldenVsString
        (takeBaseName input)
        output
        (prettyPrintFile input)
      | TestPair input output <- testPairs
      ]

prettyPrintFile :: FilePath -> IO LBS.ByteString
prettyPrintFile inputFile = do
  contents <- readFile inputFile
  let
    Right mtokens = GLua.lex lintSettings inputFile contents
    Right ast = GLua.parse lintSettings inputFile mtokens
    prettyprinted = GLua.prettyprint lintSettings ast

  pure $ LBS8.pack prettyprinted

lintSettings :: LintSettings
lintSettings = defaultLintSettings
