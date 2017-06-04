module Minmax where

import Data.String
import Control.Lens
import Data.Tree

data MinMaxTree = MinMaxTree { value::Int, nextSteps::[MinMaxTree] } | MinMax { value::Int }

