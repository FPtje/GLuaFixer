{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GLuaFixer.Effects.Interruptible where

import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Static (SideEffects (WithSideEffects), StaticRep, evalStaticRep, getStaticRep, unsafeEff_)
import qualified System.Signal as Signal

-- | Effect for allowing graceful interruptions. Interruptions are polled, so they can be ignored.
data Interruptible :: Effect

type instance DispatchOf Interruptible = Static WithSideEffects

newtype instance StaticRep Interruptible = Interruptible (IORef Bool)

-- | Run an interruptible in IO, installs signal handlers to take care of the interrupting
runInterruptible :: IOE :> es => Eff (Interruptible : es) a -> Eff es a
runInterruptible m = do
  aborted <- unsafeEff_ $ newIORef False
  unsafeEff_ $ do
    Signal.installHandler Signal.sigTERM $ \_ -> atomicWriteIORef aborted True
    Signal.installHandler Signal.sigINT $ \_ -> atomicWriteIORef aborted True

  evalStaticRep (Interruptible aborted) m

-- | Interrupt the computation
interrupt :: Interruptible :> es => Eff es ()
interrupt = do
  Interruptible aborted <- getStaticRep
  unsafeEff_ $ atomicWriteIORef aborted True

-- | Returns whether an interrupt has been sent
hasBeenInterrupted :: Interruptible :> es => Eff es Bool
hasBeenInterrupted = do
  Interruptible aborted <- getStaticRep
  unsafeEff_ $ readIORef aborted

-- | For loop that stops iterating when interrupted. It does not interfere within a computation.
interruptibleFor :: Interruptible :> es => [a] -> (a -> Eff es b) -> Eff es [b]
interruptibleFor list f = go list
 where
  go = \case
    [] -> pure []
    (x : xs) -> do
      weDone <- hasBeenInterrupted
      if weDone
        then pure []
        else do
          let !res = f x
          (:) <$> res <*> go xs
