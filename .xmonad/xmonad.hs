import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Loggers
import System.Process
import Data.Time
import Data.List (isPrefixOf)
import XMonad.Actions.CopyWindow (copy)
import System.Locale
import System.IO
import Data.Monoid
import System.Exit
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, composeOne, (-?>))
import XMonad.Actions.DynamicWorkspaces ( addWorkspacePrompt
                                        , removeWorkspace
                                        , renameWorkspace
                                        , withWorkspace
                                        , withNthWorkspace
                                        , selectWorkspace)
-- import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Actions.Navigation2D ( Navigation2D
                                   , lineNavigation
                                   , centerNavigation
                                   , fullScreenRect
                                   , singleWindowRect
                                   , switchLayer
                                   , windowGo
                                   , windowSwap
                                   , windowToScreen
                                   , screenGo
                                   , screenSwap
                                   , Direction2D
                                   )

-- functions for tagging windows and selecting them by tags
-- import XMonad.Actions.TagWindows

-- Better ways to spawn apps; safeSpawn and unsafeSpawn
import XMonad.Util.Run

-- Hints on windows
import XMonad.Layout.LayoutHints

import XMonad.Prompt
import XMonad.Prompt.Shell

-- Nice workspace bindings
import XMonad.Actions.CycleWS ( toggleWS )

-- control focus
import qualified XMonad.StackSet as W

-- key/mouse bindings
import qualified Data.Map        as M

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
myBorderWidth   = 1
myWorkspaces    = ["1","2","3","4","5","6","7","8","9","10","11","12","13","14",
                   "15","16","17","18","19","20","21","22"]
myModMask       = mod4Mask
myNormalBorderColor  = "#282a2e"
myFocusedBorderColor = "#a54242"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask,   xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask,   xK_r     ), renameWorkspace defaultXPConfig)
    , ((modm .|. shiftMask,   xK_c     ), kill)
    , ((modm,                 xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask,   xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,                 xK_n     ), refresh)
    , ((modm,                 xK_Tab   ), windows W.focusDown)
    , ((modm,                 xK_h     ), windowGo   L False)
    , ((modm,                 xK_l     ), windowGo   R False)
    , ((modm,                 xK_k     ), windowGo   U False)
    , ((modm,                 xK_j     ), windowGo   D False)
    , ((modm .|. shiftMask,   xK_h     ), windowSwap L False)
    , ((modm .|. shiftMask,   xK_l     ), windowSwap R False)
    , ((modm .|. shiftMask,   xK_k     ), windowSwap U False)
    , ((modm .|. shiftMask,   xK_j     ), windowSwap D False)
    , ((modm,                 xK_m     ), windows W.focusMaster)
    , ((modm .|. controlMask, xK_h     ), sendMessage Shrink)
    , ((modm .|. controlMask, xK_l     ), sendMessage Expand)
    , ((modm,                 xK_t     ), withFocused $ windows . W.sink)
    , ((modm,                 xK_comma ), sendMessage (IncMasterN 1))
    , ((modm,                 xK_period), sendMessage (IncMasterN (-1)))
    , ((modm,                 xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask,   xK_q     ), io exitSuccess)
    , ((modm,                 xK_q     ), spawn "ghc ~/.xmonad/xmonad.hs; userctl restart xmonad")
    , ((controlMask,          xK_Print ), spawn "sleep 0.2; scrot -s")
    , ((0,                    xK_Print ), spawn "scrot")
    , ((modm,                 xK_o     ), toggleWS)
    , ((modm,                 xK_p     ), shellPrompt myXPConfig)

      -- Prompt for a workspace to switch to
    , ((modm,                 xK_v     ), selectWorkspace myXPConfig)

      -- Prompt for a workspace and copy all client from the current one there
    , ((modm .|. controlMask, xK_m     ), withWorkspace myXPConfig (windows . copy))

      -- Remove current workspace (must be empty)
    , ((modm .|. shiftMask                  , xK_BackSpace  ), removeWorkspace)

    , ((modm .|. shiftMask, xK_a), addWorkspacePrompt myXPConfig)

    ]
    ++

--    -- tagging
--    [
--      ((modm, xK_g), tagPrompt promptConfig (\s -> withFocused (addTag s)))
--    , ((modm .|. shiftMask, xK_g), tagDelPrompt promptConfig)
--    ]
--    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    where workspaceKeys = [xK_1 .. xK_9] ++ [xK_0] ++ [xK_F1 .. xK_F12]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myLayout = avoidStruts $
    smartBorders tiled
    ||| Mirror tiled
    ||| smartBorders (layoutHints (Tall 1 (99/100) (2/5)))
        where
            -- default tiling algorithm partitions the screen into two panes
            tiled   = Tall nmaster delta ratio

            -- The default number of windows in the master pane
            nmaster = 1

            -- Default proportion of screen occupied by master pane
            ratio   = 3/5

            -- Percent of screen to increment by when resizing panes
            delta   = 3/100

myManageHook = manageDocks
    <+> composeAll
        [ className =? "MPlayer"        --> doFloat
        , className =? "Gimp"           --> doFloat
        -- Float Firefox dialog windows
        , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
        , resource  =? "desktop_window" --> doIgnore
        , resource  =? "kdesktop"       --> doIgnore
        -- Send things to appropriate workspaces
        , className =? "Firefox"        --> doShift "3"
        , className =? "Conky"        --> doShift "22"
        ]
    <+>
        composeOne [isFullscreen -?> doFullFloat]

myEventHook = mempty
myLogHook = mempty
myStartupHook = return ()

myXPConfig :: XPConfig
myXPConfig =
    XPC { font              = "-*-terminus-medium-r-*-*-*-120-75-75-*-*-iso8859-15"
        , bgColor           = "grey22"
        , fgColor           = "grey80"
        , fgHLight          = "black"
        , bgHLight          = "grey"
        , borderColor       = "white"
        , promptBorderWidth = 0
        , promptKeymap      = defaultXPKeymap
        , completionKey     = xK_Tab
        , changeModeKey     = xK_grave
        , position          = Top
        , height            = 18
        , historySize       = 256
        , historyFilter     = id
        , defaultText       = []
        , autoComplete      = Nothing
        , showCompletionOnTab = False
        , searchPredicate   = isPrefixOf
        , alwaysHighlight   = True
        }

myConfig = defaultConfig {
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

main = xmonad myConfig
