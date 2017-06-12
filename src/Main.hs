module Main where

import Foundation
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Traversable
import qualified Data.Attoparsec.ByteString as Attoparsec
import qualified Data.ByteString
import qualified Data.Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List
import qualified GHC.Base
import qualified System.Environment
import System.Directory
import System.FilePath


-- | Files and file names are parsed into a list of this type.
data Content
  = PromptVar ByteString -- ^ prompt the user for a substitution
  | EnvVar ByteString -- ^ a shell environment variable
  | String ByteString -- ^ normal text


-- | Parse one chunk of 'Content'.
parseChunk :: Attoparsec.Parser Content
parseChunk = Attoparsec.choice
  [ do s <- Attoparsec.takeWhile1 (/=questionMark) -- '?' question mark
       pure (String s)
  , do Attoparsec.word8 questionMark
       mode <- Attoparsec.takeWhile validChar
       Attoparsec.word8 questionMark
       name <- Attoparsec.takeWhile validChar
       Attoparsec.word8 questionMark
       case mode of
         "env" -> pure $ EnvVar name
         "" -> pure $ PromptVar name
         _ -> fail $ "Couldn't recognize mode: " <> bsToChars mode
  ]
  where
    questionMark = 63
    space = 32
    validChar c = c /= questionMark
               && c /= space


bsToChars :: ByteString -> [Char]
bsToChars = fmap (Data.Char.chr . fromIntegral) . Data.ByteString.unpack


charsToBs :: [Char] -> ByteString
charsToBs = Data.ByteString.pack . fmap (fromIntegral . Data.Char.ord)


parseBS :: ByteString -> Either GHC.Base.String [Content]
parseBS = Attoparsec.parseOnly (Attoparsec.many' parseChunk <* Attoparsec.endOfInput)


-- | Given a list of variables, that the user has already given a meaning to,
-- convert a list of 'Content's into a bytestring, by substituting the strings
-- given by the user and the content of environment variables for the 'Content' variables
-- and concatenating the resulting strings (without spaces, etc.).
convert :: [Content] -> StateT (HashMap ByteString ByteString) IO ByteString
convert con = do
  asked <- get
  bss <- forM con $ \c -> case c of
    String bs -> pure bs
    EnvVar var -> do
      menv <- lift $ System.Environment.lookupEnv (bsToChars var)
      case menv of
        Nothing -> lift $ throwIO $ CouldntFindEnvVar var
        Just str -> pure $ charsToBs str
    PromptVar var ->
      case HashMap.lookup var asked of
        Just res -> pure res
        Nothing -> do
          lift $ Data.ByteString.putStr $ "Please give a value for '" <> var <> "': "
          res <- lift Data.ByteString.getLine
          modify' (HashMap.insert var res)
          pure res
  pure (mconcat bss)


-- | Return all files in a directory.
-- > allFiles "/home/xyz"
-- [".zsh_rc", "Documents/CV.docx", ..]
allFiles :: FilePath -> IO [FilePath]
allFiles = flip go ""
  where
    go dir prefix = do
      content <- listDirectory dir
      dirs <- filterM (doesDirectoryExist . (</>) dir) content
      subfiles <- mconcat <$> traverse (\d -> go (dir </> d) (prefix </> d)) dirs
      let files = fmap (prefix </>) $ diff content dirs
      pure $ files <> subfiles

    -- Unlike \\, this works in O(n) time relative to the first list.
    diff (x:xs) (y:ys) | x == y = diff xs ys
    diff (x:xs) (y:ys) | x /= y = x : diff xs (y:ys)
    diff xs [] = xs
    diff [] ys = undefined


data TemplateExceptions
  = CouldntParseFilename ByteString GHC.Base.String -- ^ name and attoparsec info
  | CouldntFindEnvVar ByteString
  deriving (Eq, Show)

instance Exception TemplateExceptions where
  displayException (CouldntParseFilename name info) =
    "Couldn't parse filename '" <> bsToChars name <> "': \n" <> info
  displayException (CouldntFindEnvVar var) =
    "Couldn't find the environment variable '" <> bsToChars var <> "'."

main :: IO ()
main = do
  [name] <- getArgs
  templateDir <- (</> toList name) <$> getAppUserDataDirectory "templates"
  templateFiles <- allFiles templateDir
  (destFiles, asked) <- runStateT (mapM (convert <=< parse . charsToBs) templateFiles) HashMap.empty
  flip evalStateT asked $ forM_ (Data.List.zip (fmap (templateDir </>) templateFiles) destFiles) $ \(tf, df) -> do
    bs <- lift $ Data.ByteString.readFile tf
    cs <- parse bs
    completed <- convert cs
    lift $ createDirectoryIfMissing True (dropFileName $ bsToChars df)
    lift $ Data.ByteString.writeFile (bsToChars df) completed
  where
    parse bs = case parseBS bs of
      Right a -> pure a
      Left s -> lift $ throwIO $ CouldntParseFilename bs s
