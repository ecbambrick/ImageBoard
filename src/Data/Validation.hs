module Data.Validation where

------------------------------------------------------------------------- Types

-- | The result from an action that can either fail or succeed. Similar to
-- | Either but the Applicative instance keeps track of all errors rather than
-- | just the first.
data Result e a = Failure e | Success a deriving (Show, Eq)

instance Functor (Result e) where
    fmap f (Success x) = Success (f x)
    fmap _ (Failure x) = Failure x

instance Monoid e => Applicative (Result e) where
    Failure x <*> Failure y = Failure (x `mappend` y)
    Failure x <*> Success _ = Failure x
    Success _ <*> Failure y = Failure y
    Success f <*> Success y = Success (f y)
    pure x                  = Success x

instance Monoid e => Monad (Result e) where
    return x        = Success x
    Failure x >>= _ = Failure x
    Success x >>= f = f x

-------------------------------------------------------------------- Validation

-- | Assert that the given predicate is true; otherwise, the given error is
-- | returned as a failure.
assert :: (Monoid e) => Bool -> e -> Result e ()
assert True  _ = Success ()
assert False e = Failure e

-- | Assert that the given predicate is false; otherwise, the given error is
-- | returned as a failure.
reject :: (Monoid e) => Bool -> e -> Result e ()
reject True  e = Failure e
reject False _ = Success ()

-- | Runs the monadic function on the success value if available.
whenSuccess :: (Monad m, Monoid e) => Result e a -> (a -> m b) -> m ()
whenSuccess result @ (Success x) f = f x >> pure ()
whenSuccess result _               =        pure ()

-- | Runs the monadic function on the failure value if available.
whenFailure :: (Monad m, Monoid e) => Result e a -> (e -> m b) -> m ()
whenFailure result @ (Failure e) f = f e >> pure ()
whenFailure result _               =        pure ()
