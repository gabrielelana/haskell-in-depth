{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib
    ( someFunc
    ) where

import Data.Type.Dec
import Data.Type.Equality
import Data.Type.Nat
import Data.Type.Nat.LE
import Data.Singletons
import Data.Singletons.TH
import Control.Monad.Trans
import Data.Void (absurd)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- #############################################################################
-- Floors, doors, equality and decidability
-- #############################################################################

-- A floor is described by two numbers: the maximum number
-- of floors and the current floor, and he relationship between them

-- GoodFloor is a constraint, consisting of 3 constraint

type GoodFloor mf cf = (
  -- we can always get a singleton of `maximum floor` natural number
  SNatI mf,
  -- we can always get a singleton of `current floor` natural number
  SNatI cf,
  -- current floor is less than or equal the maximum floor
  LE cf mf)

-- Every floor must stick to this constraint, we can specify them to the constructor

-- ASK: what's the magic for which a tuple of constraints is a constraint?

data Floor (mf :: Nat) (cf :: Nat) where
  MkFloor :: GoodFloor mf cf => Floor mf cf

-- We don't need the values of the floors we can always get them through the singletons

instance Show (Floor mf cf) where
  show MkFloor = "Floor " <> show (snatToNat (snat :: SNat cf))
                 <> " of " <> show (snatToNat (snat :: SNat mf))


-- Constraint to check that we are not at the maximum floor

type BelowTop mf cf = LE ('S cf) mf

-- NOTE: I tried and this is the proof needed by the compiler, you can require
-- them or you can derive them
-- type AboveBottom mf cf = (SNatI cf, LE cf mf)

up' :: BelowTop mf cf => Floor mf cf -> Floor mf ('S cf)
up' MkFloor = MkFloor

-- withSNat :: SNat n -> (SNatI n => r) -> r
-- withLEProof :: LEProof n m -> (LE n m => r) -> r

-- down' :: AboveBottom mf cf => Floor mf (S cf) -> Floor mf cf
down' :: forall mf cf. Floor mf ('S cf) -> Floor mf cf
down' MkFloor =
  withSNat n $
    withLEProof p
      MkFloor
  where
    n :: SNat cf
    n = case snat :: SNat ('S cf) of
          SS -> snat
    p :: LEProof cf mf
    p = leStepL leProof

-- How can we prove that two floors are the same?

-- from Data.Type.Equality we get the type operator :~: with ~Refl~ data constructor which
-- will bring the ~ type equality constraint

-- eqNat :: forall n m. (SNatI n, SNatI m) => Maybe (n :~: m)

sameFloor :: forall m from to. Floor m to -> Floor m from -> Maybe (to :~: from)
sameFloor MkFloor MkFloor = eqNat

-- To build a floor we need to proof that the current floor is less than equal maximum floor

-- decideLE :: forall n m. (SNatI n, SNatI m) => Dec (LEProof n m)
-- data Dec a = Yes a | No (Neg a)
-- data Neg a = a -> Void

mkFloor :: forall m c. (SNatI m, SNatI c) => Maybe (Floor m c)
mkFloor = case decideLE :: Dec (LEProof c m) of
            Yes p -> withLEProof p $ Just MkFloor
            No _ -> Nothing

-- #############################################################################
-- Type elevator and safe primitive operations over it
-- #############################################################################

-- Generate singletons for doors

data Door = Opened | Closed
  deriving (Show, Eq)

genSingletons [''Door]

-- NOTE: it doesn't compile
-- $(singletons [d|
--  data Door = Opened | Closed
--   deriving (Show, Eq)
--   |])

-- NOTE: it compiles but better the one above
-- $(singletons [d|
--  data Door = Opened | Closed
--   |])
-- deriving instance Show Door
-- deriving instance Eq Door

-- Make an elevator, `singletons` generated an instance of SingI for Door

data Elevator (m :: Nat) (c :: Nat) (door :: Door) where
  MkElevator :: SingI door => Floor m c -> Elevator m c door

-- Extract the current floor from an elevator

currentFloor :: Elevator m c d -> Floor m c
currentFloor (MkElevator f) = f

-- Extract the current door from an elevator
-- NOTE: type -> singleton -> only value of that singleton
-- NOTE: d -> SDoor d (with `sing`) -> Door (with `fromSing`)

currentDoor :: forall m c d. Elevator m c d -> Door
currentDoor (MkElevator _) = fromSing (sing :: Sing d)

-- We can apply them to show an elevator

instance Show (Elevator m c d) where
  show el = "Elevator {current = " <> show (currentFloor el) <> ", door = " <> show (currentDoor el) <> "}"


-- Low level functions
upLL :: IO ()
upLL = putStrLn "Going up"

downLL :: IO ()
downLL = putStrLn "Going down"

openLL :: IO ()
openLL = putStrLn "Door is opening"

closeLL :: IO ()
closeLL = putStrLn "Door is closing"

-- Let's do some real work
up :: (BelowTop mx c, MonadIO m) => Elevator mx c 'Closed -> m (Elevator mx ('S c) 'Closed)
up (MkElevator f) = do
  liftIO upLL
  pure (MkElevator $ up' f)

down :: MonadIO m => Elevator mx ('S c) 'Closed -> m (Elevator mx c 'Closed)
down (MkElevator f) = do
  liftIO downLL
  pure (MkElevator $ down' f)

-- NOTE: by having the same c on Floor and Elevator we are imposing that the
-- current floor is the same
open :: MonadIO m => Floor mx c -> Elevator mx c 'Closed -> m (Elevator mx c 'Opened)
open _ (MkElevator f) = do
  liftIO openLL
  pure (MkElevator f)

close :: MonadIO m => Floor mx c -> Elevator mx c 'Opened -> m (Elevator mx c 'Closed)
close _ (MkElevator f) = do
  liftIO closeLL
  pure (MkElevator f)

-- What if we have a door with an unknown type-level state?
-- door unknown is specified by a generic type variable `d`

-- We can always summon the singleton value using the type class `Sing`

ensureClosed :: forall mx c d m. MonadIO m => Elevator mx c d -> m (Elevator mx c 'Closed)
ensureClosed e@(MkElevator fl) =
  case sing :: (Sing d) of
    SClosed -> pure e
    SOpened -> close fl e

ensureOpenedAt :: forall mx c d m. MonadIO m => Floor mx c -> Elevator mx c d -> m (Elevator mx c 'Opened)
ensureOpenedAt _ e@(MkElevator f) =
  case sing :: (Sing d) of
    SClosed -> open f e
    SOpened -> pure e

-- #############################################################################
-- Deciding elevator moves
-- #############################################################################

-- Define a type for moves with all the constraints hidden within the constructor

-- ASK: shouldn't this be the constraint?
-- GoingUp :: LE to mf => Move mx to from
-- GoingDown :: to ~ S f => Move mx to from

-- RESPONSE: no, the assumption is that we always move one floor at the time,
-- but there's no such constraint ((⇀‸↼))

-- Do something like this work?
-- type UpFloor n m = EqNat n ('S m)
-- type DownFloor n m = EqNat ('S n) m
-- GoingUp :: (UpFloor to from, BelowTop mx from) => Move mx to from
-- GoingDown :: (DownFloor to from, from ~ 'S f) => Move mx to from

data Move mf to from where
  StandStill :: Move m to from
  GoingUp :: BelowTop mx from => Move mx to from
  GoingDown :: from ~ 'S fl => Move mx to form

-- The goal is to define the followign function
-- decideMove :: forall mx to from. Floor mx to -> Floor mx from -> Move mx to from

-- We have 3 scenarios
-- 1. to == from -> we will not move
-- 2. to < from -> elevator goes down
-- 3. from < to -> elevator goes up

-- To decide that we need the following function provided by the `fin` package
-- discreteNat :: forall n m. (SNatI n, SNatI m) => Dec (n :~: m)
-- decideLE :: forall n m. (SNatI n, SNatI m) => Dec (LEProof n m)

decideMove :: forall mx to from.
  Floor mx to -> Floor mx from -> Move mx to from
decideMove MkFloor MkFloor =
  case discreteNat :: Dec (to :~: from) of
    -- to == from -> we will not move
    Yes Refl -> StandStill
    -- to /= from
    -- we have proof that `Neg (to :~: from)`
    No to_neq_from ->
      case decideLE :: Dec (LEProof to from) of
        -- to < from -> GoingDown
        Yes to_le_from ->
           withAboveGround to_le_from to_neq_from GoingDown
        -- to > from -> GoingUp
        No to_gt_from ->
           withLEProof (belowTop to_gt_from) GoingUp
  where
    -- we know `Neg (LEProof to from)`
    -- we implicitly know that `LE to mx` because of `Floor mx to`
    -- we need proof that `from` is LE than mx -> `LEProof ('S from) mx`
    -- PROOF:
    -- - leTrans :: LEProof n m -> LEProof m p -> LEProof n p
    -- - leSwap :: forall n m. (SNatI n, SNatI m) => Neg (LEProof n m) -> LEProof ('S m) n
    -- - `leSwap (Neg (LEProof to from))` -> `LEProof ('S from) to`
    -- - `leProof` -> `LEProof to mx`
    -- - `leTrans (LEProof ('S from) to) (LEProof to mx)` -> `LEProof ('S from) mx)`
    belowTop :: Neg (LEProof to from) -> LEProof ('S from) mx
    belowTop neg = leTrans (leSwap neg) leProof

    -- we know (LEProof to from)
    -- we know Neg (to :~: from)
    -- we need proof that (from ~ 'S floor) from some `floor`
    -- (forall fl. from ~ 'S fl => r) is the constraint that we need for `r` which is GoingDown
    -- PROOF:
    -- - pattern match on the constructor fo LEProof
    -- - if `(LESucc _)` then `(LEProof (S n) (S m))` then `from ~ (S m)` then OK
    -- - if `LEZero` we "formally" know nothing about m
    --   - we "intuitively" know that `from` is more tha Z if `to` reached Z first
    --   - we need to proove it
    --   - we use the singleton of `from`
    --   - if it's SZ then we have an absurd because we know
    --     - to ~ Z
    --     - Neg (to :~: from)
    --     - therefore Neg (from ~ Z)
    --     - we can use `absurd` and the Void from `neq Refl` to get `r`
    --   - if it's SS then we have proven what we need
    withAboveGround :: LEProof to from -> Neg (to :~: from) -> (forall fl. from ~ 'S fl => r) -> r
    withAboveGround (LESucc _) _ r = r
    withAboveGround LEZero neq r =
      case snat :: SNat from of
        SZ -> absurd $ neq Refl
        SS -> r

-- TODO: does't compile
-- moveTo ::
--   MonadIO m =>
--   Floor mx to ->
--   Elevator mx from 'Closed ->
--   m (Elevator mx to 'Closed)
-- moveTo fl el =
--   case decideMove fl (currentFloor el) of
--     StandStill -> pure el
--     GoingUp -> up el >>= moveTo fl
--     GoingDown -> down el >>= moveTo fl
