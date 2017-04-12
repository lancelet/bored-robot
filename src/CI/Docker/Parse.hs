module CI.Docker.Parse where

import Data.Maybe
import Language.Dockerfile

getFroms :: Dockerfile -> [BaseImage]
getFroms = mapMaybe getFrom

getFrom :: InstructionPos -> Maybe BaseImage
getFrom (InstructionPos (From b) _ _) = Just b
getFrom _ = Nothing
