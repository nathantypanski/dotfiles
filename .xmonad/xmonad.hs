import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- import XMonad.Util.Dzen
import XMonad.Util.Loggers
import System.Process
import Data.Time
import System.Locale
import System.IO
import Data.Monoid
import System.Exit
import XMonad.Layout.NoBorders

-- functions for tagging windows and selecting them by tags
import XMonad.Actions.TagWindows

-- Better ways to spawn apps; safeSpawn and unsafeSpawn
import XMonad.Util.Run

-- Hints on windows
import XMonad.Layout.LayoutHints

import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell

-- Nice workspace bindings
import XMonad.Actions.CycleWS

-- dzen
import qualified System.Dzen     as D

-- control focus
import qualified XMonad.StackSet as W

-- key/mouse bindings
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Quick and easy settings
--

myTerminal      = "termite"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
myBorderWidth   = 2
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myModMask       = mod4Mask
myNormalBorderColor  = "#282a2e"
myFocusedBorderColor = "#a54242"

dzenCommand = (RawCommand "dzen2"
    ["-ta","l"
    ,"-fg","#eeeeee"
    ,"-bg","#303030"
    ,"-w", "1600"
    ,"-e","button2=;"
    ])

------------------------------------------------------------------------
-- Main loop
--

main = do
    dzw <- D.createDzen dzenCommand
    xmonad $ defaults
        {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP myDzenPP
                             {
                                ppOutput = hPutStrLn dzw
                             },
        startupHook        = myStartupHook
        }


------------------------------------------------------------------------
-- dzen pretty printer
--

myDzenPP :: PP
myDzenPP = defaultPP { ppCurrent  = dzenColor "#005f00" "#afd700" . pad . wrap "" ""
                     , ppVisible  = dzenColor "#002b36" "#839496" . pad
                     , ppHidden   = dzenColor "#eeeeee" "#808080" . pad
                     , ppUrgent   = dzenColor "#eeeeee" "#d70000" . pad
                     , ppExtras   = [
                                      padL loadAvg
                                    , date "%r"
                                    , padL battery
                                    , logCmd "echo -n '^fg(#81a2be)^i(.dzen/icons/arch_10x10.xbm)^fg() '"
                                    ]
                     , ppHiddenNoWindows = const ""
                     , ppWsSep    = ""
                     , ppSep      = ""
                     , ppLayout   = dzenColor "#1c1c1c" "#d0d0d0" .
                                    (\ x -> pad $ case x of
                                              "Tall"        -> "||"
                                              "Mirror Tall" -> "="
                                              "Hinted Full" -> "[H]"
                                              "Hinted Tall" -> "|H|"
                                              _             -> x
                                    )
                     , ppTitle    = wrap "^ca(2,xdotool key super+shift+c)" "^ca()" . dzenColor "#005f00" "#afd700" . shorten 40 . pad
                     }

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm,               xK_p     ), spawn "~/bin/dmenu/dmenu_run.sh")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_y   ),   windows W.focusDown)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    , ((mod4Mask .|. shiftMask, xK_z), spawn "i3lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((0, xK_Print), spawn "scrot")
    , ((modm , xK_o ), toggleWS)
    ]
    ++

    -- tagging
    [
      ((modm, xK_g), tagPrompt promptConfig (\s -> withFocused (addTag s)))
    , ((modm .|. shiftMask, xK_g), tagDelPrompt promptConfig)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
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

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageDocks
    <+> composeAll
        [ className =? "MPlayer"        --> doFloat
        , className =? "Gimp"           --> doFloat
        -- Float Firefox dialog windows
        , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
        , resource  =? "desktop_window" --> doIgnore
        , resource  =? "kdesktop"       --> doIgnore
        -- Send things to appropriate workspaces
        , className =? "Firefox"        --> doShift "3:web"
        ]

------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()
------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--
myStartupHook = return ()

promptConfig = defaultXPConfig {
      font = "-*-terminus-medium-*-*-*-16-*-*-*-*-*-iso8859-*"
    , promptBorderWidth = 0
    , historySize = 1
    , alwaysHighlight = True
    , bgColor = "#1D1F21"
    , fgColor = "#C5C8C6"
    , fgHLight = "#DE935F"
    , bgHLight = "#373B41"
--  , defaultText = "λ "
    , position = Top
    , showCompletionOnTab = False
    , height = 20
}

------------------------------------------------------------------------
-- Default config

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
