module Official.Solutions where

import Prelude
import Data.List (List(..), concatMap, (:))
import Data.Maybe (Maybe(..))

class
  Apply m <= Bind m where
  bind :: forall a b. m a -> (a -> m b) -> m b

class (Applicative m, Bind m) <= Monad m

instance bindList :: Bind List where
  bind xs f = concatMap f xs

instance bindMaybe :: Bind Maybe where
  bind Nothing _ = Nothing
  bind (Just a) f = f a
