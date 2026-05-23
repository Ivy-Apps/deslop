module Effects.Random (
    Random (..),
    rGenRandomInt,
    runRandom,
) where

import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Random (randomRIO)

data Random :: Effect where
    GenRandomInt :: (Int, Int) -> Random m Int

type instance DispatchOf Random = 'Dynamic

rGenRandomInt :: (Random :> es) => (Int, Int) -> Eff es Int
rGenRandomInt = send . GenRandomInt

runRandom :: (IOE :> es) => Eff (Random : es) a -> Eff es a
runRandom = interpret $ \_ -> \case
    GenRandomInt bounds -> liftIO $ randomRIO bounds
