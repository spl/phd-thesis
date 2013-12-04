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

import Control.Arrow (first, second, (***))

--------------------------------------------------------------------------------

type Page = Seq Format

data Form a
  = Plain a                       -- Unadorned
  | Sub a a                       -- Subscript
  | Super a a                     -- Superscript
  | Indexed a a a                 -- Both subscript and superscript
  | Prime (Int -> TeX) Int a      -- Prime with macro and count
  | SubPrime (Int -> TeX) Int a a -- Subscript and prime with macro and count
  deriving Functor

type Lhs    = Form Text
type Rhs    = Form (Text, TeX)

--------------------------------------------------------------------------------

data Format = Format Text Text (Form (Text, Text, TeX))

instance Eq Format where
  f1 == f2 = case (split f1,split f2) of
    ((pre1,post1,lhs1,_),(pre2,post2,lhs2,_)) ->
      (pre1,pprLhs lhs1,post1) == (pre2,pprLhs lhs2,post2)

instance Ord Format where
  f1 <= f2 = case (split f1,split f2) of
    ((pre1,post1,lhs1,_),(pre2,post2,lhs2,_)) ->
      (pre1,pprLhs lhs1,post1) <= (pre2,pprLhs lhs2,post2)

mapFormat :: ((Text, Text, TeX) -> (Text, Text, TeX)) -> Format -> Format
mapFormat f (Format pre post x) = Format pre post (fmap f x)

--------------------------------------------------------------------------------

data TeX
  = TEmpty            -- Empty
  | TRaw Text         -- Text
  | TConst Text       -- 0-argument macro
  | TMacro Text TeX   -- 1-argument command
  | TBraces TeX       -- Braces
  | TSeq TeX TeX      -- Sequencing
  deriving (Eq, Ord, Show)

instance Monoid TeX where
  mempty  = TEmpty
  mappend = TSeq

--------------------------------------------------------------------------------

instance IsString Lhs where
  fromString = Plain . fromString

instance IsString TeX where
  fromString = TRaw . fromString

instance IsString Format where
  fromString s = Format "" "" (Plain (fromString s, "", fromString s))

--------------------------------------------------------------------------------

-- Format construction

-- Raw showable on lhs and rhs
fromShow :: Show a => a -> Format
fromShow = fromString . show

-- Raw character on lhs and rhs
fromChar :: Char -> Format
fromChar = fromString . return

-- A 'plain' format with text on the lhs and TeX on the rhs
mkPlain :: (Text, TeX) -> Format
mkPlain (l,r) = Format "" "" $ Plain (l,"",r)

-- A 'plain' format with text on the lhs and modified text on the rhs
mkPlainWith :: (Text -> TeX) -> Text -> Format
mkPlainWith f t = mkPlain (t,f t)

-- A format with the arg on the lhs and a constant macro of the arg on the rhs
mkPlainConst :: Text -> Format
mkPlainConst = mkPlainWith TConst

subWithF :: Monoid a => a -> Form a -> Form a
subWithF yb (Plain x)           = Sub x yb
subWithF yb (Sub x xb)          = Sub x (xb <> yb)
subWithF yb (Super x xp)        = Indexed x yb xp
subWithF yb (Indexed x xb xp)   = Indexed x (xb <> yb) xp
subWithF yb (Prime f n x)       = SubPrime f n x yb
subWithF yb (SubPrime f n x xb) = SubPrime f n x (xb <> yb)

superWithF :: Monoid a => a -> Form a -> Form a
superWithF yp (Plain x)         = Super x yp
superWithF yp (Sub x xb)        = Indexed x xb yp
superWithF yp (Super x xp)      = Super x (xp <> yp)
superWithF yp (Indexed x xb xp) = Indexed x xb (xp <> yp)
superWithF yp (Prime{})         = error "Format.superWithF: Cannot superscript a prime."
superWithF yp (SubPrime{})      = error "Format.superWithF: Cannot superscript a subscript and prime."

subWith :: Page -> Page -> Page
subWith = cproductS' $
  \(Format pre1 post1 (Plain xb)) (Format pre2 post2 yp) ->
    Format (pre1 <> pre1) (post1 <> post1) $ subWithF xb yp

superWith :: Page -> Page -> Page
superWith = cproductS' $
  \(Format pre1 post1 (Plain xb)) (Format pre2 post2 yp) ->
    Format (pre1 <> pre1) (post1 <> post1) $ superWithF xb yp

primeWithF :: (Int -> TeX) -> Int -> Form a -> Form a
primeWithF f n (Plain x)           = Prime f n x
primeWithF f n (Sub x xb)          = SubPrime f n x xb
primeWithF _ _ (Super{})           = error "Format.primeWithF: Cannot prime a superscript."
primeWithF _ _ (Indexed{})         = error "Format.primeWithF: Cannot prime a subscript and superscript."
primeWithF _ _ (Prime{})           = error "Format.primeWithF: Cannot prime a prime."
primeWithF _ _ (SubPrime{})        = error "Format.primeWithF: Cannot prime a subscript and prime."

primeWithFormat :: (Int -> TeX) -> Int -> Format -> Format
primeWithFormat f n (Format pre post x) = Format pre post (primeWithF f n x)

primeWith :: (Int -> TeX) -> Int -> Page -> Page
primeWith f n = fmap (primeWithFormat f n)

primesWith :: (Int -> TeX) -> [Int] -> Page -> Page
primesWith f ns p = mconcat (fmap (\n -> fmap (primeWithFormat f n) p) ns)

tickPrimes :: Int -> TeX
tickPrimes n = TRaw $ TS.replicate n "'"

macroPrimes :: Int -> TeX
macroPrimes n = TRaw "^" <> TConst (case n of
  1 -> "prime"
  2 -> "dprime"
  3 -> "trprime"
  4 -> "qprime"
  _ -> error $ "Format.macroPrimes: " <> show n <> " primes not supported.")

straightQuotePrimes :: Int -> TeX
straightQuotePrimes n = mconcat $ replicate n $ TConst "mathstraightquote"

--------------------------------------------------------------------------------

fromList :: (a -> Format) -> [a] -> Page
fromList f = foldMap (S.singleton . f)

fromSeq :: (a -> Format) -> Seq a -> Page
fromSeq = fmap

--------------------------------------------------------------------------------

prefixLhs :: Text -> Page -> Page
prefixLhs pre = fmap f
  where
    f (Format _ post x) = Format pre post x

injectMacro :: Text -> Page -> Page
injectMacro m = fmap (mapFormat (\(l,_,r) -> (l,m,r)))

--------------------------------------------------------------------------------

-- Format deconstruction

split :: Format -> (Text, Text, Lhs, Rhs)
split (Format pre post form) = case split' form of
  (lhs,rhs) -> (pre,post,lhs,rhs)
  where
    split' :: Form (Text, Text, TeX) -> (Lhs, Rhs)
    split' (Plain (x,m,y))                           = (Plain x           , Plain (m,y))
    split' (Sub (x,m,y) (xb,mb,yb))                  = (Sub x xb          , Sub (m,y) (mb,yb))
    split' (Super (x,m,y) (xp,mp,yp))                = (Super x xp        , Super (m,y) (mp,yp))
    split' (Indexed (x,m,y) (xb,mb,yb) (xp,mp,yp))   = (Indexed x xb xp   , Indexed (m,y) (mb,yb) (mp,yp))
    split' (Prime f n (x,m,y))                       = (Prime f n x       , Prime f n (m,y))
    split' (SubPrime f n (x,m,y) (xb,mb,yb))         = (SubPrime f n x xb , SubPrime f n (m,y) (mb,yb))

--------------------------------------------------------------------------------

-- Format pretty-printing

pprLhs :: Lhs -> Builder
pprLhs (Plain x)           = B.fromText x
pprLhs (Sub x xb)          = B.fromText x <> B.singleton '_' <> B.fromText xb
pprLhs (Super x xp)        = B.fromText x <> B.singleton '\'' <> B.fromText xp
pprLhs (Indexed x xb xp)   = B.fromText x <> B.singleton '_' <> B.fromText xb <> B.singleton '\'' <> B.fromText xp
pprLhs (Prime _ n x)       = B.fromText x <> pprLhsPrimes n
pprLhs (SubPrime _ n x xb) = B.fromText x <> B.singleton '_' <> B.fromText xb <> pprLhsPrimes n

pprTeX :: TeX -> Builder
pprTeX TEmpty        = B.fromString ""
pprTeX (TRaw t)      = B.fromText t
pprTeX (TConst nm)   = B.singleton '\\' <> B.fromText nm <> B.fromString "{}"
pprTeX (TMacro nm t) = B.singleton '\\' <> B.fromText nm <> B.singleton '{' <> pprTeX t <> B.singleton '}'
pprTeX (TBraces t)   = B.singleton '{' <> pprTeX t <> B.singleton '}'
pprTeX (TSeq t1 t2)  = pprTeX t1 <> pprTeX t2

pprMacro :: (Text, TeX) -> Builder
pprMacro (m,x) | TS.null m = pprTeX x
               | otherwise = pprTeX $ TMacro m x

pprRhs :: Rhs -> Builder
pprRhs (Plain x)               = pprMacro x
pprRhs (Sub x xb)              = B.singleton '{' <> pprMacro x <> B.fromString "}_{" <> pprMacro xb <> B.singleton '}'
pprRhs (Super x xp)            = B.singleton '{' <> pprMacro x <> B.fromString "}^{" <> pprMacro xp <> B.singleton '}'
pprRhs (Indexed x xb xp)       = B.singleton '{' <> pprMacro x <> B.fromString "}_{" <> pprMacro xb <> B.fromString "}^{" <> pprMacro xb <> B.singleton '}'
pprRhs (Prime f n (m,x))       = pprMacro (m,x <> f n)
pprRhs (SubPrime f n (m,x) xb) = pprMacro (m,TBraces (x <> f n) <> TRaw "_") <> pprMacro xb

pprLhsPrimes :: Int -> Builder
pprLhsPrimes n = B.fromText $ TS.replicate n "'"

pprFormat :: Format -> Builder
pprFormat f = case split f of
  (pre, post, lhs, rhs) ->
    B.fromString "%format " <>
    B.fromText pre <> pprLhs lhs <> B.fromText post <>
    B.fromString " = \"" <>
    pprRhs rhs <> B.singleton '"'

pprPage :: Page -> Builder
pprPage = foldMap ((<> B.singleton '\n') . pprFormat)

prettyPage :: Page -> TL.Text
prettyPage = B.toLazyText . pprPage

printPage :: Page -> IO ()
printPage = TLIO.putStr . prettyPage

--------------------------------------------------------------------------------

-- Format collections

-- Latin alphabet
latinLower, latinUpper, latin :: Page
latinLower = fromList fromChar ['a'..'z']
latinUpper = fromList fromChar ['A'..'Z']
latin = latinLower <> latinUpper

-- Greek alphabet
greekLower, greekUpper, greek :: Page
greekLower = fromList mkPlainConst ["alpha","beta","gamma","delta","epsilon","varepsilon","zeta","eta","theta","vartheta","iota","kappa","varkappa","lambda","mu","nu","xi","pi","varpi","rho","varrho","sigma","varsigma","tau","upsilon","phi","varphi","chi","psi","omega"]
greekUpper = fromList mkPlainConst ["Gamma","Delta","Lambda","Phi","Pi","Psi","Sigma","Theta","Upsilon","Xi","Omega"]
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

