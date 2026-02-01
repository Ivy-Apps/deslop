module Effects.AI where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic (interpret, send)

newtype AIError = GenericError Text

data AI :: Effect where
    Prompt :: Text -> AI m (Either AIError Text)

type instance DispatchOf AI = Dynamic

prompt :: (AI :> es) => Text -> Eff es (Either AIError Text)
prompt = send . Prompt

runAI :: (IOE :> es) => Eff (AI : es) a -> Eff es a
runAI = interpret $ \_ -> \case
    Prompt p -> pure . Left $ GenericError ("Not implemented: " <> p)
