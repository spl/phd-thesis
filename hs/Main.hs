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
  , ("dx",    deltaR)                -- Type functor variable
  , ("tvM",   "M")                   -- Type variable set M
  , ("tvN",   "N")                   -- Type variable set N
  , ("tva",   alphaR)                -- Type variable set alpha
  , ("Meta.tvara",  alphaR)          -- Type variable metavariable a
  , ("Meta.typa",   tauR)            -- Type metavariable a
  , ("Meta.typb",   upsilonR)        -- Type metavariable b
  , ("Meta.tsuba",  thetaR)          -- Type substitution
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

mathcals :: Page
mathcals = injectMacro "mathcal" $ S.fromList $ map mkPlain
  [ ("T.A",     "A")            -- Abstract type
  , ("T.R",     "R")            -- Representation type
  ]

mathscrs :: Page
mathscrs = injectMacro "mathscr" $ S.fromList $ map mkPlain
  [ ("A.G",     "G")            -- Generalization
  , ("A.U",     "U")            -- Unification
  , ("A.W",     "W")            -- Algorithm W
  ]

--------------------------------------------------------------------------------

synids :: Page
synids = injectMacro "Synid" $ S.fromList $ map mkPlain
  [ ("Syn.tvar",  "TVar")         -- Type variables
  , ("Syn.typ",   "Typ")          -- Types
  , ("Syn.exp",   "Exp")          -- Expressions/terms
  , ("Syn.env",   "Env")          -- Type environments
  , ("Syn.tyf",   "TyF")          -- Type functors
  , ("Syn.envf",  "EnF")          -- Type functor environments
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
mkRule mPre t = mkPlain $ ("R." <>) *** eqlabelR $ case mPre of
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
  , mathcals
  , mathscrs
  , synids
  , keywords
  , rules
  ]

main :: IO ()
main = printPage pages

