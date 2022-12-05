{-# LANGUAGE OverloadedStrings #-}
-- for argument parsing
import Options.Applicative as A
import Data.Monoid ((<>))

-- for YAML
import Data.Yaml
import Control.Applicative -- <$>, <*>
import Data.Maybe (fromJust, isJust, fromMaybe)
import qualified Data.ByteString.Char8 as BS

data MyFlag = MyFlag
	{code :: String,
	name :: String,
	full_name :: String,
	relative_height :: Int,
	relative_width :: Int}
	deriving (Show)

data MyArgs = MyArgs
	{ codeselect :: String
	, flip :: Bool
	, addon :: Maybe String }

instance FromJSON MyFlag where
    parseJSON (Object v) = MyFlag <$>
                           v .: "code" <*>
                           v .: "name" <*>
                           v .: "full_name" <*>
                           v .: "relative_height" <*>
                           v .: "relative_width"
    parseJSON _ = error "Can't parse flag from YAML."

interpretArgs :: MyArgs -> IO ()
interpretArgs (MyArgs codeselect flip addon) =
    do
      let conffile = fromMaybe "templates/base.yml" addon
      ymlData <- BS.readFile conffile
      flags <- Data.Yaml.decodeThrow ymlData
      let flagselect = head (filter (\a -> code a == codeselect) flags)
      let nameselect = name flagselect
      putStrLn ("You have selected the " ++ nameselect ++ ".")


sample :: A.Parser MyArgs
sample = MyArgs
     <$> argument str
          ( metavar "CODE"
         <> help "Flag code to use, present in provided or shipped ´*.yml´ files, generally of the form ´<ISO 3166-1 alpha-2>_<year>´." )
     <*> switch
          ( long "flip"
         <> short 'f'
         <> help "Whether to flip horizontally" )
     <*> ( optional $ strOption
          ( metavar "ADDON"
         <> long "addon"
         <> short 'a'
         <> help "Add `.yml` configurations for more flag codes." ))

main :: IO ()
main = A.execParser opts >>= interpretArgs
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Create a flag."
     <> header "hflagcreate — a Haskell flag creator." )
