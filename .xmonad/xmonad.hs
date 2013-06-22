import XMonad
import XMonad.Hooks.DynamicLog

main = do
    xmonad =<< dzen defaultConfig
        { terminal    = myTerminal
        , modMask     = myModMask
        , borderWidth = myBorderWidth
        }

-- yes, these are functions; just very simple ones
-- that accept no input and return static values
myTerminal    = "urxvt"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 1
