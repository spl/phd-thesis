{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Control.Arrow ((&&&), (***), first, second)
import Data.Monoid (mconcat, (<>))
import Data.Text (Text)
import Data.String (IsString(..))

import Format

import qualified Data.Sequence as S

--------------------------------------------------------------------------------

commonMetaids :: Page
commonMetaids = injectMacro "Metaid" $ mconcat
  [ latinLower
  , latinUpper
  , primesWith macroPrimes [1..3] latinLower
  , subWith (range 0 4) latinLower
  , greekLower
  , primesWith macroPrimes [1..3] greekLower
  , subWith (range 0 4) greekLower
  ]

commonVarids :: Page
commonVarids = prefixLhs "V." $ injectMacro "Varid" $ mconcat
  [ latinLower
  , primesWith straightQuotePrimes [1..2] latinLower
  ]

commonConids :: Page
commonConids = prefixLhs "C." $ injectMacro "Conid" $ mconcat
  [ latinUpper
  , primesWith straightQuotePrimes [1..2] latinUpper
  ]

--------------------------------------------------------------------------------

otherMetaidsBase :: Page
otherMetaidsBase = S.fromList $ map mkPlain
  [ ("ty",    tauR)                  -- Type
  , ("tz",    upsilonR)              -- Type
  , ("tyf",   mathringR tauR)        -- Type functor
  , ("tzf",   mathringR upsilonR)    -- Type functor
  , ("sc",    varsigmaR)             -- Type scheme
  , ("scf",   mathringR varsigmaR)   -- Type scheme
  , ("sb",    thetaR)                -- Substitution
  , ("sbf",   mathringR thetaR)      -- Substitution for type functors
  , ("env",   gammaR_)               -- Type environment
  , ("envf",  mathringR gammaR_)     -- Type functor environment
  ]

otherMetaids :: Page
otherMetaids = injectMacro "Metaid" $ mconcat
  [ otherMetaidsBase
  , primesWith tickPrimes [1..3] otherMetaidsBase
  , subWith (range 0 4) otherMetaidsBase
  , subWith latinLower otherMetaidsBase
  ]

--------------------------------------------------------------------------------

otherConids :: Page
otherConids = injectMacro "Conid" $ S.fromList $ map mkPlain
  [ ("T.num",   "N")            -- Numbers
  , ("T.str",   "S")            -- Strings
  ]

--------------------------------------------------------------------------------

synids :: Page
synids = injectMacro "Synid" $ S.fromList $ map mkPlain
  [ ("S.exp",   "Exp")          -- Expressions/terms
  , ("S.typ",   "Typ")          -- Types
  , ("S.env",   "Env")          -- Environment
  ]

--------------------------------------------------------------------------------

keywords :: Page
keywords = injectMacro "Keyword" $ fromList fromString $
  [ "let", "in", "where"
  , "if", "then", "else", "as"
  , "case", "of"
  , "class", "instance"
  , "type", "data", "newtype", "family"
  , "infix", "infixl", "infixr"
  , "do"
  , "fix"
  ]

--------------------------------------------------------------------------------

eqlabelR :: Text -> TeX
eqlabelR = TMacro "eqlabel" . TRaw

mkRule :: Maybe Text -> Text -> Format
mkRule pre t = mkPlain $ ("R." <>) *** eqlabelR $ case pre of
  Nothing  -> (t, t)
  Just pre -> (pre <> t, pre <> "-" <> t)

toRules :: [Text] -> Page
toRules = S.fromList . map (mkRule Nothing)

mkRules :: [Text] -> [Text] -> Page
mkRules x y = S.fromList $ cproductL (mkRule . Just) x y

rules :: Page
rules = mconcat
  [ toRules ["tra","rew","r","rs","num","str","var","app","lam","fix","let"]
  , mkRules ["t","c","p"] ["var","app","lam","fix","let","rew"]
  , mkRules ["tt"] ["abs","rep","comp"]
  , mkRules ["m","s"] ["var","app","mvar"]
  , mkRules ["p"] ["var","app"]
  , mkRules ["red"] ["lam","let","fix"]
  ]

--------------------------------------------------------------------------------

pages :: Page
pages = mconcat
  [ commonMetaids
  , commonVarids
  , commonConids
  , otherMetaids
  , otherConids
  , synids
  , keywords
  , rules
  ]

main :: IO ()
main = printPage pages

