{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

--------------------------------------------------------------------------------

module Format where

--------------------------------------------------------------------------------

import Data.Monoid (Monoid(..), (<>))
import Data.Foldable (foldMap)

import Data.Sequence (Seq, (<|), ViewL(..))
import qualified Data.Sequence as S

import Data.Text (Text)
import qualified Data.Text as TS
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Lazy.Builder as B

import Data.String (IsString(..))

import Control.Arrow ((***))

--------------------------------------------------------------------------------

type Page = Seq Format

data Form a
  = Plain a           -- Unadorned
  | Sub a a           -- Subscript
  | Super a a         -- Superscript
  | Indexed a a a     -- Both subscript and superscript
  | Prime a Int       -- Primes
  | SubPrime a a Int  -- Subscript and primes
  deriving (Eq, Functor, Ord, Show)

type Lhs    = Form Text
type Rhs    = Form TeX
type Format = Form (Text, TeX)

data TeX
  = TEmpty            -- Empty
  | TRaw Text         -- Text
  | TConst Text       -- 0-argument macro
  | TMacro Text TeX   -- 1-argument command
  | TBraces TeX       -- Braces
  | TSeq TeX TeX      -- Sequencing
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

instance Monoid TeX where
  mempty  = TEmpty
  mappend = TSeq

--------------------------------------------------------------------------------

instance IsString Lhs where
  fromString = Plain . fromString

instance IsString TeX where
  fromString = TRaw . fromString

instance IsString Rhs where
  fromString = Plain . fromString

instance IsString Format where
  fromString s = Plain (fromString s, fromString s)

--------------------------------------------------------------------------------

-- Format construction

-- Raw showable on lhs and rhs
fromShow :: Show a => a -> Format
fromShow = fromString . show

-- Raw character on lhs and rhs
fromChar :: Char -> Format
fromChar = fromString . return

-- A 'plain' format with text on the lhs and TeX on the rhs
mkPlain :: Text -> TeX -> Format
mkPlain = curry Plain

-- A format with the arg on the lhs and a constant macro of the arg on the rhs
mkConst :: Text -> Format
mkConst t = mkPlain t $ TConst t

subWithF :: Monoid a => a -> Form a -> Form a
subWithF yb (Plain x)         = Sub x yb
subWithF yb (Sub x xb)        = Sub x (xb <> yb)
subWithF yb (Super x xp)      = Indexed x yb xp
subWithF yb (Indexed x xb xp) = Indexed x (xb <> yb) xp
subWithF yb (Prime x m)       = SubPrime x yb m
subWithF yb (SubPrime x xb m) = SubPrime x (xb <> yb) m

superWithF :: Monoid a => a -> Form a -> Form a
superWithF yp (Plain x)         = Super x yp
superWithF yp (Sub x xb)        = Indexed x xb yp
superWithF yp (Super x xp)      = Super x (xp <> yp)
superWithF yp (Indexed x xb xp) = Indexed x xb (xp <> yp)
superWithF yp (Prime x m)       = error "Format.superWithF: Cannot superscript a prime."
superWithF yp (SubPrime x xb m) = error "Format.superWithF: Cannot superscript a subscript and prime."

subWith :: Page -> Page -> Page
subWith = cproductS' (\(Plain xb) yp -> subWithF xb yp)

superWith :: Page -> Page -> Page
superWith = cproductS' (\(Plain xb) yp -> superWithF xb yp)

primeWithF :: Int -> Form a -> Form a
primeWithF n (Plain x)         = Prime x n
primeWithF n (Sub x xb)        = SubPrime x xb n
primeWithF n (Super x xp)      = error "Format.primeWithF: Cannot prime a superscript."
primeWithF n (Indexed x xb xp) = error "Format.primeWithF: Cannot prime a subscript and superscript."
primeWithF n (Prime x _)       = Prime x n
primeWithF n (SubPrime x xb _) = SubPrime x xb n

primeWith :: Int -> Page -> Page
primeWith n = fmap (primeWithF n)

primesWith :: [Int] -> Page -> Page
primesWith ns p = mconcat (fmap (\i -> fmap (primeWithF i) p) ns)

-- Pair two formats into one by combining the lhs/rhs of each
pairF :: (Text -> Text -> Text) -> (TeX -> TeX -> TeX) -> Format -> Format -> Format
pairF fl fr (Plain (l1,r1)) (Plain (l2,r2)) = Plain (fl l1 l2, fr r1 r2)
pairF fl fr _               _               = error "Format.pairF: cannot pair non-Plain formats"

--------------------------------------------------------------------------------

fromList :: (a -> Format) -> [a] -> Page
fromList f = foldMap (S.singleton . f)

fromSeq :: (a -> Format) -> Seq a -> Page
fromSeq = fmap

combine :: Lhs -> Rhs -> Maybe Format
combine (Plain x)         (Plain y)         = Just $ Plain (x,y)
combine (Sub x xb)        (Sub y yb)        = Just $ Sub (x,y) (xb,yb)
combine (Super x xp)      (Super y yp)      = Just $ Super (x,y) (xp,yp)
combine (Indexed x xb xp) (Indexed y yb yp) = Just $ Indexed (x,y) (xb,yb) (xp,yp)
combine (Prime x m)       (Prime y n)
  | m == n                                  = Just $ Prime (x,y) m
combine (SubPrime x xb m) (SubPrime y yb n)
  | m == n                                  = Just $ SubPrime (x,y) (xb,yb) m
combine _                 _                 = Nothing

--------------------------------------------------------------------------------

-- Format deconstruction

split :: Format -> (Lhs, Rhs)
split (Plain (x,y))                   = (Plain x,Plain y)
split (Sub (x,y) (xb,yb))             = (Sub x xb,Sub y yb)
split (Super (x,y) (xp,yp))           = (Super x xp,Super y yp)
split (Indexed (x,y) (xb,yb) (xp,yp)) = (Indexed x xb xp,Indexed y yb yp)
split (Prime (x,y) n)                 = (Prime x n,Prime y n)
split (SubPrime (x,y) (xb,yb) n)      = (SubPrime x xb n,SubPrime y yb n)

--------------------------------------------------------------------------------

-- Format pretty-printing

pprLhs :: Lhs -> Builder
pprLhs (Plain x)         = B.fromText x
pprLhs (Sub x xb)        = B.fromText x <> B.singleton '_' <> B.fromText xb
pprLhs (Super x xp)      = B.fromText x <> B.singleton '\'' <> B.fromText xp
pprLhs (Indexed x xb xp) = B.fromText x <> B.singleton '_' <> B.fromText xb <> B.singleton '\'' <> B.fromText xp
pprLhs (Prime x n)       = B.fromText x <> B.fromText (TS.replicate n "'")
pprLhs (SubPrime x xb n) = B.fromText x <> B.singleton '_' <> B.fromText xb <> B.fromText (TS.replicate n "'")

pprTeX :: TeX -> Builder
pprTeX TEmpty        = B.fromString ""
pprTeX (TRaw t)      = B.fromText t
pprTeX (TConst nm)   = B.singleton '\\' <> B.fromText nm <> B.fromString "{}"
pprTeX (TMacro nm t) = B.singleton '\\' <> B.fromText nm <> B.singleton '{' <> pprTeX t <> B.singleton '}'
pprTeX (TBraces t)   = B.singleton '{' <> pprTeX t <> B.singleton '}'
pprTeX (TSeq t1 t2)  = pprTeX t1 <> pprTeX t2

pprRhs :: Rhs -> Builder
pprRhs (Plain x)         = pprTeX x
pprRhs (Sub x xb)        = B.singleton '{' <> pprTeX x <> B.fromString "}_{" <> pprTeX xb <> B.singleton '}'
pprRhs (Super x xp)      = B.singleton '{' <> pprTeX x <> B.fromString "}^{" <> pprTeX xp <> B.singleton '}'
pprRhs (Indexed x xb xp) = B.singleton '{' <> pprTeX x <> B.fromString "}_{" <> pprTeX xb <> B.fromString "}^{" <> pprTeX xb <> B.singleton '}'
pprRhs (Prime x n)       = pprTeX x <> pprPrimes n
pprRhs (SubPrime x xb n) = B.singleton '{' <> pprTeX x <> pprPrimes n <> B.fromString "}_{" <> pprTeX xb <> B.singleton '}'

pprPrimes :: Int -> Builder
pprPrimes n = pprTeX . TConst $ case n of
  1 -> "prime"
  2 -> "dprime"
  3 -> "trprime"
  4 -> "qprime"
  _ -> error $ "Format.pprPrimes: " <> show n <> " primes not supported."

pprFormat :: Format -> Builder
pprFormat f = case split f of
  (lhs, rhs) -> B.fromString "%format " <> pprLhs lhs <> B.fromString " = \"" <> pprRhs rhs <> B.singleton '"'

pprPage :: Page -> Builder
pprPage = foldMap ((<> B.singleton '\n') . pprFormat)

prettyPage :: Page -> TL.Text
prettyPage = B.toLazyText . pprPage

printPage :: Page -> IO ()
printPage = TLIO.putStr . prettyPage

--------------------------------------------------------------------------------

-- Inject Varid or Conid

data IdMacro = Varid | Conid

idToText :: IdMacro -> Text
idToText Varid = "Varid"
idToText Conid = "Conid"

wrapTeX :: IdMacro -> TeX -> TeX
wrapTeX im = TMacro (idToText im)

injectIdF :: IdMacro -> Format -> Format
injectIdF im = fmap (id *** wrapTeX im)

injectId :: IdMacro -> Page -> Page
injectId im = fmap (injectIdF im)

--------------------------------------------------------------------------------

-- Format collections

-- Latin alphabet
latinLower, latinUpper, latin :: Page
latinLower = fromList fromChar ['a'..'z']
latinUpper = fromList fromChar ['A'..'Z']
latin = latinLower <> latinUpper

-- Greek alphabet
greekLower, greekUpper, greek :: Page
greekLower = fromList mkConst ["alpha","beta","gamma","delta","epsilon","varepsilon","zeta","eta","theta","vartheta","iota","kappa","varkappa","lambda","mu","nu","xi","pi","varpi","rho","varrho","sigma","varsigma","tau","upsilon","phi","varphi","chi","psi","omega"]
greekUpper = fromList mkConst ["Gamma","Delta","Lambda","Phi","Pi","Psi","Sigma","Theta","Upsilon","Xi","Omega"]
greek = greekLower <> greekUpper

-- Combined Latin and Greek alphabets
alpha :: Page
alpha = latin <> greek

-- Numbers in a range
range :: Int -> Int -> Page
range m n = fromList fromShow [m..n]

-- Common Haskell types and type classes
haskellTypes, haskellClasses :: Page
haskellTypes   = fromList fromString ["Char","Int","Integer","Float","Double","List","Bool","Maybe","Either","Ordering"]
haskellClasses = fromList fromString ["Show","Read","Eq","Enum","Bounded","Num","Monad","Functor","Applicative"]

--------------------------------------------------------------------------------

-- Convenient macros for the rhs

-- Greek lowercase
alphaR = TConst "alpha"
betaR = TConst "beta"
gammaR = TConst "gamma"
deltaR = TConst "delta"
epsilonR = TConst "epsilon"
varepsilonR = TConst "varepsilon"
zetaR = TConst "zeta"
etaR = TConst "eta"
thetaR = TConst "theta"
varthetaR = TConst "vartheta"
iotaR = TConst "iota"
kappaR = TConst "kappa"
varkappaR = TConst "varkappa"
lambdaR = TConst "lambda"
muR = TConst "mu"
nuR = TConst "nu"
xiR = TConst "xi"
piR = TConst "pi"
varpiR = TConst "varpi"
rhoR = TConst "rho"
varrhoR = TConst "varrho"
sigmaR = TConst "sigma"
varsigmaR = TConst "varsigma"
tauR = TConst "tau"
upsilonR = TConst "upsilon"
phiR = TConst "phi"
varphiR = TConst "varphi"
chiR = TConst "chi"
psiR = TConst "psi"
omegaR = TConst "omega"

-- Greek uppercase
gammaR_ = TConst "Gamma"
deltaR_ = TConst "Delta"
lambdaR_ = TConst "Lambda"
phiR_ = TConst "Phi"
piR_ = TConst "Pi"
psiR_ = TConst "Psi"
sigmaR_ = TConst "Sigma"
thetaR_ = TConst "Theta"
upsilonR_ = TConst "Upsilon"
xiR_ = TConst "Xi"
omegaR_ = TConst "Omega"

-- Math
ensuremathR = TMacro "ensuremath"
mathscR = ensuremathR . TMacro "textsc"
mathbfR = TMacro "mathbf"
mathcalR = TMacro "mathcal"
mathpzcR = TMacro "mathpzc"
mathringR = TMacro "mathring"
dotR = TMacro "dot"
hatR = TMacro "hat"
tildeR = TMacro "tilde"
checkR = TMacro "check"
scriptstyleR t = TBraces (TConst "scriptstyle" <> t)
scriptscriptstyleR t = TBraces (TConst "scriptscriptstyle" <> t)

--------------------------------------------------------------------------------

-- Seq diagonalization

-- Adapted from Doaitse Swierstra's list diagonalization
diagonalS :: Seq (Seq a) -> Seq a
diagonalS = diag EmptyL EmptyL . S.viewl
  where
    diag :: ViewL (Seq a) -> ViewL (Seq a) -> ViewL (Seq a) -> Seq a
    diag EmptyL    EmptyL EmptyL     =  S.empty
    diag EmptyL    ll     EmptyL     =       diag ll            EmptyL         EmptyL
    diag EmptyL    ll     (r :< rr)  =       diag (r :< v2s ll) EmptyL         (S.viewl rr)
    diag (w :< ww) ll     rr         =
      case S.viewl w of
        EmptyL                       ->      diag (S.viewl ww)  ll             rr
        v :< vv                      -> v <| diag (S.viewl ww)  (vv :< v2s ll) rr
    v2s :: ViewL a -> Seq a
    v2s EmptyL    = S.empty
    v2s (x :< xs) = x <| xs

-- Cartesian product and 3-product
cproductS :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
cproductS f xs ys = diagonalS $
  do {y <- ys ; return $ do {x <- xs ; return $ f x y}}

cproductS3 :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
cproductS3 f xs ys zs = diagonalS $
  do {y <- ys ; z <- zs ; return $ do {x <- xs ; return $ f x y z}}

-- Sorted Cartesian product and 3-product
cproductS' :: Ord c => (a -> b -> c) -> Seq a -> Seq b -> Seq c
cproductS' f xs ys = S.sort $ cproductS f xs ys

cproductS3' :: Ord d => (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
cproductS3' f xs ys zs = S.sort $ cproductS3 f xs ys zs

--------------------------------------------------------------------------------

-- List diagonalization (from Doaitse Swierstra)

diagonalL :: [[a]] -> [a]
diagonalL = diag [] []
  where
    diag [] []  []         = []
    diag [] ll  (r:rr)     =    diag (r:ll) []     rr
    diag [] ll  []         =    diag ll     []     []
    diag ((v:vv):ww) ll rr = v: diag ww     (vv:ll) rr
    diag ([]    :ww) ll rr =    diag ww     ll      rr

-- Cartesian product
cproductL :: (a -> b -> c) -> [a] -> [b] -> [c]
cproductL f xs ys = diagonalL [[f x y | x <- xs] | y <- ys]

-- Cartesian product
cproductL3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
cproductL3 f xs ys zs = diagonalL [[f x y z | x <- xs] | y <- ys,  z <- zs]

