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

commonVarids :: Page
commonVarids = injectId Varid $ mconcat
  [ latinLower
  , primesWith [1..3] latinLower
  , subWith (range 0 4) latinLower
  , greekLower
  , primesWith [1..3] greekLower
  , subWith (range 0 4) greekLower
  ]

commonConids :: Page
commonConids = injectId Conid $ mconcat
  [ latinUpper
  , primesWith [1..3] latinUpper
  , subWith (range 0 4) latinUpper
  , greekUpper
  , primesWith [1..3] greekUpper
  , subWith (range 0 4) greekUpper
  ]

--------------------------------------------------------------------------------

specialBase :: Page
specialBase = S.fromList $ map Plain
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

specialVarids :: Page
specialVarids = injectId Varid $ mconcat
  [ specialBase
  , primesWith [1..3] specialBase
  , subWith (range 0 4) specialBase
  , subWith latinLower specialBase
  ]

--------------------------------------------------------------------------------

eqlabelR :: Text -> TeX
eqlabelR = TMacro "eqlabel" . TRaw

mkRule :: Maybe Text -> Text -> Format
mkRule pre t = Plain $ id *** eqlabelR $ case pre of
  Nothing  -> (t, t)
  Just pre -> (pre <> t, pre <> "-" <> t)

toRules :: [Text] -> Page
toRules = S.fromList . map (mkRule Nothing)

mkRules :: [Text] -> [Text] -> Page
mkRules x y = S.fromList $ cproductL (mkRule . Just) x y

rules :: Page
rules = mconcat
  [ toRules ["Tra","Rew","R","RS","Var","App","Lam","Fix","Let"]
  , mkRules ["T","C","P"] ["Var","App","Lam","Fix","Let","Rew"]
  , mkRules ["TT"] ["Abs","Rep","Comp"]
  , mkRules ["M","S"] ["Var","App","MVar"]
  , mkRules ["P"] ["Var","App"]
  , mkRules ["Red"] ["Lam","Let","Fix"]
  ]

--------------------------------------------------------------------------------

mathkwR :: Text -> TeX
mathkwR = TMacro "mathkw" . TRaw

mkMathKw :: Text -> Format
mkMathKw = mkPlainWith mathkwR

mkMathKws :: [Text] -> Page
mkMathKws = S.fromList . map mkMathKw

mathKws :: Page
mathKws = mkMathKws
  ["let","in","where"
  ,"if","then","else","as"
  ,"case","of"
  ,"class","instance"
  ,"type","data","newtype","family"
  ,"infix","infixl","infixr"
  ,"do"
  ,"fix"
  ]

--------------------------------------------------------------------------------

pages :: Page
pages = mconcat
  [ commonVarids
  , commonConids
  , specialVarids
  , rules
  , mathKws
  ]

main :: IO ()
main = printPage pages

