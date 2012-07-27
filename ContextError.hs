{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, UndecidableInstances #-}

module ContextError (
    CE(..),
    ContextError(..),
    errMap
    )where

import Control.Monad.Error

class (MonadError e m) => ContextError c e m | m -> e, m -> c where
    withContext :: c -> m a -> m a

newtype CE m a = CE {unCE :: m a} deriving (Monad)

instance (MonadPlus mp, MonadError (mp c, e) m) => MonadError e (CE m) where
    throwError e = CE $ throwError (mzero, e)
    catchError (CE m) f = CE $ catchError m $ \(_, e) -> (unCE $ f e)

instance (MonadPlus mp, MonadError (mp c, e) m) => ContextError c e (CE m) where
    withContext ctx (CE m) = CE $ catchError m $ \(c, e) -> throwError (c `mplus` return ctx, e)

instance (Error b, MonadPlus mp) => Error (mp a, b) where
    noMsg = (mzero, noMsg)

errMap :: (MonadError e2 me2, Error e1) => (e1 -> e2) -> Either e1 a -> me2 a
errMap f e = either (throwError . f) return e

test :: MonadError String me => Int -> me Int
test 0 = throwError "0"
test 1 = return 1

test2 :: ContextError String String me => Int -> me Int
test2 x = withContext "mofo" $ test x

test3 :: MonadError (Maybe String, String) me => Int -> me Int
test3 = unCE . test2

test4 :: Int -> String
test4 x = either show show $ (test3 x :: Either (Maybe String, String) Int)
