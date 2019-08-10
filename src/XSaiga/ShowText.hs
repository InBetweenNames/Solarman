module XSaiga.ShowText where

import Data.Text
{-import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Text.Lazy.Builder.RealFloat (realFloat)

class ShowText a where
    tshow :: a -> Text

instance ShowText Int where
    tshow = toStrict . toLazyText . decimal

instance ShowText Float where
    tshow = toStrict . toLazyText . realFloat

instance ShowText Text where
    tshow = id


-} --TODO: when TH works on ARM, use text-show

tshow :: (Show a) => a -> Text
tshow = pack . show
