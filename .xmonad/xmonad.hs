import XMonad
import XMonad.Layout.Decoration (Theme (..), DefaultShrinker(..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Groups.Helpers (moveToGroupUp, moveToGroupDown, swapUp
                                    ,swapDown, swapMaster, focusGroupUp
                                    ,focusGroupDown, focusUp, focusDown)
import XMonad.Layout.Tabbed (Shrinker(..), addTabs)
import XMonad.Layout.Simplest (Simplest(..))
import XMonad.Layout.Groups (group)
import XMonad.Layout.Groups.Examples (TiledTabsConfig(..)
                                     ,tallTabs
                                     ,rowOfColumns, shrinkMasterGroups
                                     ,expandMasterGroups
                                     ,increaseNMasterGroups
                                     ,decreaseNMasterGroups
                                     ,shrinkText, defaultTheme)
import XMonad.Layout.LayoutHints (layoutHints)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat
                                  ,composeOne, (-?>))
import XMonad.Hooks.ManageDocks (Direction2D(L, R, U, D)
                                ,ToggleStruts(..)
                                ,manageDocks, avoidStruts)
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeWorkspace
                                        ,renameWorkspace, withWorkspace
                                        ,withNthWorkspace, selectWorkspace)
import XMonad.Layout.Named (named)
import XMonad.Layout.Renamed (renamed, Rename(..))
import XMonad.Actions.Navigation2D (Navigation2D, Direction2D
                                   ,lineNavigation, centerNavigation
                                   ,fullScreenRect, singleWindowRect
                                   ,switchLayer, windowGo, windowSwap
                                   ,windowToScreen, screenGo, screenSwap)
import XMonad.Hooks.DynamicLog (PP, dynamicLogString, dynamicLogWithPP
                               ,pad, ppTitle, ppLayout, defaultPP
                               ,ppCurrent, ppVisible, ppHidden
                               ,ppHiddenNoWindows, ppUrgent, ppSep
                               ,ppOutput, ppWsSep, ppExtras, wrap, shorten
                               ,xmobarColor)
import XMonad.Actions.TagWindows (addTag, tagDelPrompt, tagPrompt)
import XMonad.Actions.CycleWS ( toggleWS )
import XMonad.Actions.GridSelect (defaultGSConfig,
                                  goToSelected)
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.Loggers (Logger, logCmd, loadAvg, date, battery)
import XMonad.Prompt (XPConfig (..), XPPosition(Top)
                     ,font, bgColor, defaultXPKeymap, fgColor, fgHLight
                     ,bgHLight, borderColor, promptBorderWidth, promptKeymap
                     ,completionKey, changeModeKey, position, height
                     ,historySize, historyFilter, defaultText, autoComplete
                     ,showCompletionOnTab, searchPredicate, alwaysHighlight)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Window (windowPromptGoto)
import XMonad.StackSet (shiftMaster, focusMaster, sink
                       ,greedyView, shift, view)
import Data.List (isPrefixOf)
import Data.Monoid (All, mempty)
import Data.Map (Map, fromList)
import System.Process (CmdSpec (RawCommand))
import System.Exit (exitSuccess)

colorBackground :: String
colorBackground = "#151515"

colorCurrentLine :: String
colorCurrentLine = "#FF8939"

colorSelection :: String
colorSelection = "#404040"

colorForeground :: String
colorForeground = "#D7D0C7"

colorComment :: String
colorComment = "#dddddd"

colorRed :: String
colorRed = "#E84F4F"

colorOrange :: String
colorOrange = "#F39D21"

colorYellow :: String
colorYellow = "#E1AA5D"

colorGreen :: String
colorGreen = "#B8D6AC"

colorAqua :: String
colorAqua = "#4E9FB1"

colorBlue :: String
colorBlue = "#7DC1CF"

colorPurple :: String
colorPurple = "#9B64FB"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth   = 1

myWorkspaces :: [String]
myWorkspaces = map show [1..10]

myModMask :: KeyMask
myModMask = mod4Mask

myNormalBorderColor :: String
myNormalBorderColor = colorComment

myFocusedBorderColor :: String
myFocusedBorderColor = colorForeground

terminus :: String
terminus = "-*-terminus-medium-r-*-*-12-120-*-*-*-*-iso8859-*"

restartCommand :: String
restartCommand =  "if type xmonad;"
                        ++ "then xmonad --recompile && "
                        ++ "xmonad --restart;"
                        ++ "else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
                        ++ " && killall xmobar"

shiftLayout :: X ()
shiftLayout =
    sendMessage NextLayout

myKeys :: XConfig Layout -> Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = fromList $
    [
      ((modm .|. shiftMask,   xK_r     ), renameWorkspace myXPConfig)
    , ((modm,                 xK_Return), spawn $ "urxvt")
    , ((modm .|. shiftMask,   xK_c     ), kill)
    , ((modm, xK_space                 ), shiftLayout)
    , ((modm .|. shiftMask,   xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,                 xK_n     ), refresh)
    , ((modm,                 xK_h     ), windowGo   L False)
    , ((modm,                 xK_j     ), focusGroupDown)
    , ((modm,                 xK_k     ), focusGroupUp)
    , ((modm,                 xK_l     ), windowGo   R False)
    , ((modm,                 xK_Tab   ), focusDown)
    , ((modm .|. shiftMask,   xK_Tab   ), focusUp)
    , ((modm .|. shiftMask,   xK_h     ), moveToGroupUp False)
    , ((modm .|. shiftMask,   xK_j     ), focusDown)
    , ((modm .|. shiftMask,   xK_k     ), focusUp)
    , ((modm .|. shiftMask,   xK_l     ), moveToGroupDown False)
    , ((modm .|. controlMask, xK_h     ), shrinkMasterGroups)
    , ((modm .|. controlMask, xK_j     ), increaseNMasterGroups)
    , ((modm .|. controlMask, xK_k     ), decreaseNMasterGroups)
    , ((modm .|. controlMask, xK_l     ), expandMasterGroups)
    , ((modm,                 xK_s     ), goToSelected defaultGSConfig)
    , ((modm,                 xK_f     ), withFocused $ windows . sink)
    , ((modm,                 xK_comma ), sendMessage (IncMasterN 1))
    , ((modm,                 xK_period), sendMessage (IncMasterN (-1)))
    , ((modm,                 xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask,   xK_q     ), io exitSuccess)
    , ((modm,                 xK_q     ), spawn restartCommand)
    , ((modm,                 xK_i     ), spawn "firefox")
    , ((modm,                 xK_t     ), spawn "~/bin/tmux-urxvt")
    , ((controlMask,          xK_Print ), spawn "sleep 0.2; scrot -s")
    , ((0,                    xK_Print ), spawn "scrot")
    , ((modm,                 xK_o     ), toggleWS)
    , ((modm,                 xK_p     ), shellPrompt myXPConfig)
    , ((modm,                 xK_slash ), windowPromptGoto myXPConfig {
                                              autoComplete = Just 500000
                                          })
    , ((modm,                 xK_m     ), tagPrompt
                                            myXPConfig $ withFocused . addTag)
    , ((modm .|. shiftMask,   xK_m     ), tagDelPrompt myXPConfig)

      -- Prompt for a workspace to switch to
    , ((modm,                 xK_v     ), selectWorkspace myXPConfig)

      -- Prompt for a workspace and copy all client from the current one there
    , ((modm .|. controlMask, xK_m     ), withWorkspace
                                            myXPConfig (windows . copy))

      -- Remove current workspace (must be empty)
    , ((modm .|. shiftMask             , xK_BackSpace  ), removeWorkspace)

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
        , (f, m) <- [(greedyView, 0), (shift, shiftMask)]]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys
        , (f, m) <- [(greedyView, 0), (shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(view, 0), (shift, shiftMask)]]

    where workspaceKeys = [xK_1 .. xK_9] ++ [xK_0] ++ [xK_F1 .. xK_F12]

myMouseBindings :: XConfig t -> Map (KeyMask, Button) (Window -> X ()) 
myMouseBindings (XConfig {XMonad.modMask = modm}) = fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows shiftMaster)
    ]

myTheme :: Theme
myTheme = defaultTheme { activeColor         = colorCurrentLine
                       , inactiveColor       = colorBackground
                       , urgentColor         = colorRed
                       , activeBorderColor   = colorComment
                       , inactiveBorderColor = colorBackground
                       , urgentBorderColor   = colorBackground
                       , activeTextColor     = colorForeground
                       , inactiveTextColor   = colorComment
                       , urgentTextColor     = "#FF0000"
                       , fontName            = terminus
                       , decoWidth           = 200
                       , decoHeight          = 16
                       , windowTitleAddons   = []
                       , windowTitleIcons    = []
                       }

myLayout = avoidStruts $
    tallTabs (TTC 1 0.5 (3/100) 1 0.5 (3/100) shrinkText myTheme)
    ||| smartBorders tiled
    where
        -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio

        -- The default number of windows in the master pane
        nmaster = 1

        -- Default proportion of screen occupied by master pane
        ratio   = 3/5

        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

myManageHook :: ManageHook
myManageHook = manageDocks
    <+> composeAll
        [ className =? "MPlayer"        --> doFloat
        , className =? "Gimp"           --> doFloat
        , stringProperty "WM_NAME" =? "Firefox Preferences" --> doFloat
        -- Float Firefox dialog windows
        , (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
        , resource  =? "desktop_window" --> doIgnore
        ]
    <+>
        composeOne [isFullscreen -?> doFullFloat]

myEventHook :: Event -> X All
myEventHook = mempty

myLogHook :: X ()
myLogHook = ewmhDesktopsLogHook

myStartupHook :: X ()
myStartupHook = return ()

myXPConfig :: XPConfig
myXPConfig =
    XPC { XMonad.Prompt.font = terminus
        , bgColor           = colorBackground
        , fgColor           = colorForeground
        , fgHLight          = colorForeground
        , bgHLight          = colorSelection
        , borderColor       = colorComment
        , promptBorderWidth = 0
        , promptKeymap      = defaultXPKeymap
        , completionKey     = xK_Tab
        , changeModeKey     = xK_grave
        , position          = Top
        , height            = 14
        , historySize       = 256
        , historyFilter     = id
        , defaultText       = []
        , autoComplete      = Nothing
        , showCompletionOnTab = False
        , searchPredicate   = isPrefixOf
        , alwaysHighlight   = True
        }

-- | Some nice xmobar defaults.
mybarPP :: PP
mybarPP = defaultPP { ppCurrent =
                         xmobarColor colorBackground colorGreen . wrap "[" "]"
                    , ppTitle   =
                         xmobarColor colorGreen  colorSelection . shorten 40
                    , ppVisible = wrap "(" ")"
                    , ppUrgent  = xmobarColor colorRed colorYellow
                    }

main :: IO ()
main = do
    h <- spawnPipe "/home/nathan/.xmonad/xmobar/dist/build/xmobar/xmobar" 
    xmonad defaultConfig {
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
            logHook            = ewmhDesktopsLogHook <+> 
                    dynamicLogWithPP mybarPP { ppOutput = hPutStrLn h },
            startupHook        = myStartupHook
        }
