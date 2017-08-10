-- xmonad config stolen from Vic Fryzel, edited by ya pal Toth
-- http://github.com/vicfryzel/xmonad-config

import           Data.Maybe
import           Graphics.X11.ExtraTypes
import           System.IO
import           System.Exit
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.FadeWindows
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.Gaps
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Util.Cursor
import           XMonad.Util.Run(spawnPipe)
import           XMonad.Util.EZConfig(additionalKeys)
import           XMonad.Util.WorkspaceCompare
import qualified XMonad.Hooks.EwmhDesktops as E
import qualified XMonad.StackSet           as W
import qualified Data.Map                  as M


------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "urxvt"

-- The command to lock the screen or show the screensaver.
myScreensaver         = "~/.lockscreen/lock.sh"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot    = "select-screenshot"

-- The command to take a fullscreen screenshot.
myScreenshot          = "scrot"

-- dmenu Stuff
dmenuFont             = "Gohu GohuFont:pixelsize=14" -- Must be in XFT, XLFD is unsupported
dmenuPrompt           = "$ >"
dmenuNForegroundColor = "#e6bf98"
dmenuNBackgroundColor = "#2b1414"
dmenuSForegroundColor = "#f2f1b9"
dmenuSBackgroundColor = "#52332b"

-- transparency of inactive windows
trans                 = 0.1


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--

myWorkspaces = ["1","2","3","4","5","6","7","8","9"]
-----------------------------------------------------------------------
-- Window rules
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

myManageHook = composeAll
    [ className =? "Gimp"             --> doFloat
    , className =? "VirtualBox"       --> doFullFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)
    ]


------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

myLayout =
    lessBorders OnlyFloat (avoidStruts $ spacing 15 (
    gaps [(U,15), (D,15), (R,15), (L,15)] $
    Tall 1 (3/100) (1/2)
    ||| Mirror (Tall 1 (3/100) (1/2))
    )
    ||| Full
    )
    ||| noBorders (fullscreenFull Full)


------------------------------------------------------------------------
-- Colors and borders
--
--

myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#2B1414"

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#4baeaa"

-- Color of workspaces with no windows
xmobarNoWindowColor = "#61778d"

-- Width of the window border in pixels.
myBorderWidth = 5


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

mySink = "alsa_output.pci-0000_00_1f.3.analog-stereo"

myKeys =
    [ ((myModMask, xK_p ), spawn $
          "dmenu_run -fn '" ++ dmenuFont
          ++ "' -p '"  ++ dmenuPrompt
          ++ "' -nf '" ++ dmenuNForegroundColor
          ++ "' -nb '" ++ dmenuNBackgroundColor
          ++ "' -sf '" ++ dmenuSForegroundColor
          ++ "' -sb '" ++ dmenuSBackgroundColor
          ++ "'")
    ]





compareToCurrent :: X (WindowSpace -> Ordering)
compareToCurrent =
    do comp <- getWsCompare
       ws <- gets windowset
       let cur = W.workspace (W.current ws)
       return (\w -> comp (W.tag cur) (W.tag w))

greaterNonEmptyWs =
    do comp <- compareToCurrent
       return (\w -> comp w == LT && (isJust $ W.stack w))

lessNonEmptyWs =
    do comp <- compareToCurrent
       return (\w -> comp w == GT && (isJust $ W.stack w))

moveToNextNonEmptyNoWrap = moveTo Next (WSIs greaterNonEmptyWs)
moveToPrevNonEmptyNoWrap = moveTo Prev (WSIs lessNonEmptyWs)

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
    [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook =
    spawn "compton --config ~/.compton"
    <+> setDefaultCursor xC_left_ptr

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaults
        { logHook = do
            fadeWindowsLogHook $ composeAll
                [                 opaque
                , isUnfocused --> transparency trans
                ]
            dynamicLogWithPP $ xmobarPP
                { ppOutput          = hPutStrLn xmproc
                , ppTitle           = const ""
                , ppLayout          = const ""
                , ppHiddenNoWindows = xmobarColor xmobarNoWindowColor ""
                , ppCurrent         = xmobarColor xmobarCurrentWorkspaceColor ""
                , ppSep             = "   "
                }
        , manageHook = manageDocks <+> myManageHook
        , startupHook = setWMName "LG3D" <+> myStartupHook
        } `additionalKeys` myKeys


------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def
    { -- simple stuff
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

    -- bindings
    , mouseBindings      = myMouseBindings

      -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , startupHook        = myStartupHook
    , handleEventHook    = E.fullscreenEventHook <+> docksEventHook
    }

