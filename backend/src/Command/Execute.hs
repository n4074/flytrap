
module Command.Execute where

import System.Process.Typed (shell)


execute c i o = shell c
