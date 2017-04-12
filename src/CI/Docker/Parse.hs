module CI.Docker.Parse
    ( getFroms
    , getFrom
    , Language.Dockerfile.parseString
    , Language.Dockerfile.parseFile
    , Language.Dockerfile.prettyPrint
    , Language.Dockerfile.prettyPrintInstructionPos
    , Language.Dockerfile.Dockerfile
    , Language.Dockerfile.InstructionPos(..)
    , Language.Dockerfile.Instruction(..)
    , Language.Dockerfile.BaseImage(..)
    , Language.Dockerfile.Tag
    ) where

import Data.Maybe
import Language.Dockerfile

getFroms :: Dockerfile -> [BaseImage]
getFroms = mapMaybe getFrom

getFrom :: InstructionPos -> Maybe BaseImage
getFrom (InstructionPos (From b) _ _) = Just b
getFrom _ = Nothing
