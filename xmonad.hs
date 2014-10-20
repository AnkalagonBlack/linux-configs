-- validate syntax: xmonad --recompile
{-# LANGUAGE NoMonomorphismRestriction #-}
------------------------------------------------------------------------
 
import XMonad hiding (Tall)
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.GridSelect
import XMonad.Hooks.DynamicHooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ComboP
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.Run
import Control.Monad (liftM2)
import Data.Monoid
import Graphics.X11
import Graphics.X11.Xinerama
import System.Exit
import System.IO
 
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map as M
 
-- XMonad:
main = do
  dzen <- spawnPipe myStatusBar
  conkytop <- spawnPipe myTopBar
  --conkympd <- spawnPipe myMPDBar
  --conkyhdd <- spawnPipe myHDDBar
  xmonad $ myUrgencyHook $ defaultConfig
 
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
 
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
 
    , layoutHook         = myLayout
    , manageHook         = myManageHook <+> manageDocks <+> dynamicMasterHook
    , handleEventHook    = myEventHook
    , logHook            = dynamicLogWithPP $ myDzenPP_ dzen
    , startupHook        = myStartupHook
    }
 
myTerminal = "urxvt"
myFocusFollowsMouse = True
myBorderWidth = 1
myModMask = mod4Mask
myWorkspaces = ["bro", "cns", "music", "4", "5", "6", "7", "8", "9"]
myNormalBorderColor = "#0f0f0f"
myFocusedBorderColor = "#0000AA"
 
myEventHook = mempty
myStartupHook = return ()
 
-- Color, font and iconpath definitions:
myFont = "-*-montecarlo-medium-r-normal-*-11-*-*-*-c-*-*-*"
myIconDir = "/home/and1/.dzen"
myDzenFGColor = "#555555"
myDzenBGColor = "#222222"
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = "#0f0f0f"
myPatternColor = "#1f1f1f"
mySeperatorColor = "#555555"
 
-- GSConfig options:
myGSConfig = defaultGSConfig
    { gs_cellheight = 50
    , gs_cellwidth = 250
    , gs_cellpadding = 10
    , gs_font = "" ++ myFont ++ ""
    }
 
-- XPConfig options:
myXPConfig = defaultXPConfig
    { font = "" ++ myFont ++ ""
    , bgColor = "" ++ myNormalBGColor ++ ""
    , fgColor = "" ++ myNormalFGColor ++ ""
    , fgHLight = "" ++ myNormalFGColor ++ ""
    , bgHLight = "" ++ myUrgentBGColor ++ ""
    , borderColor = "" ++ myFocusedBorderColor ++ ""
    , promptBorderWidth = 1
    , position = Bottom
    , height = 16
    , historySize = 100
    }
 
-- Theme options:
myTheme = defaultTheme
    { activeColor = "" ++ myFocusedBGColor ++ ""
    , inactiveColor = "" ++ myDzenBGColor ++ ""
    , urgentColor = "" ++ myUrgentBGColor ++ ""
    , activeBorderColor = "" ++ myFocusedBorderColor ++ ""
    , inactiveBorderColor = "" ++ myNormalBorderColor ++ ""
    , urgentBorderColor = "" ++ myNormalBorderColor ++ ""
    , activeTextColor = "" ++ myFocusedFGColor ++ ""
    , inactiveTextColor = "" ++ myDzenFGColor ++ ""
    , urgentTextColor = "" ++ myUrgentFGColor ++ ""
    , fontName = "" ++ myFont ++ ""
    }
 
-- Statusbar options:
myStatusBar = "dzen2 -x '0' -y '0' -h '16' -w '1300' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myTopBar = "conky -c .conkytop | dzen2 -x '1300' -y '0' -h '16' -w '620' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myMPDBar = "conky -c .conkympd | dzen2 -x '0' -y '1184' -h '16' -w '1600' -ta 'l' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
myHDDBar = "conky -c .conkyhdd | dzen2 -x '1600' -y '1184' -h '16' -w '320' -ta 'r' -fg '" ++ myDzenFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "'"
 
-- Urgency hint options:
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    { args = ["-x", "0", "-y", "1184", "-h", "16", "-w", "1920", "-ta", "r", "-expand", "l", "-fg", "" ++ myUrgentFGColor ++ "", "-bg", "" ++ myNormalBGColor ++ "", "-fn", "" ++ myFont ++ ""] }
 
-- Layouts:
myLayout = avoidStruts $ layoutHints $ onWorkspace "1:irc" (resizableTile ||| Mirror resizableTile) $ onWorkspace "6:GIMP" gimpLayout $ smartBorders (Full ||| resizableTile ||| Mirror resizableTile)
    where
    resizableTile = ResizableTall nmaster delta ratio []
    tabbedLayout = tabbedBottomAlways shrinkText myTheme
    gimpLayout = combineTwoP (TwoPane 0.04 0.82) (tabbedLayout) (Full) (Not (Role "gimp-toolbox"))
    nmaster = 1
    ratio = toRational (2/(1+sqrt(5)::Double))
    delta = 3/100
 
-- Key bindings:
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, xK_Print), spawn "scrot screen_%Y-%m-%d.png -d 1") -- take screenshot
    , ((mod1Mask .|. controlMask, xK_Home), spawn "mpc toggle") -- play/pause song
    , ((mod1Mask .|. controlMask, xK_End), spawn "mpc stop") -- stop playback
    , ((mod1Mask .|. controlMask, xK_Prior), spawn "mpc prev") -- previous song
    , ((mod1Mask .|. controlMask, xK_Next), spawn "mpc next") -- next song
    , ((modMask, xK_Tab), windows W.focusDown) -- move focus to the next window
    , ((modMask, xK_j), windows W.focusDown) -- move focus to the next window
    , ((modMask, xK_k), windows W.focusUp) -- move focus to the previous window
    , ((modMask, xK_b), sendMessage ToggleStruts) -- toggle the statusbar gap
    , ((modMask, xK_m), windows W.swapMaster) -- swap the focused window and the master window
    , ((modMask, xK_comma), sendMessage (IncMasterN 1)) -- increment the number of windows in the master area
    , ((modMask, xK_period), sendMessage (IncMasterN (-1))) -- deincrement the number of windows in the master area
    , ((modMask, xK_Return), spawn "urxvt") -- move focus to the master window
    , ((modMask, xK_space), sendMessage NextLayout) -- rotate through the available layout algorithms
    , ((modMask, xK_g), goToSelected myGSConfig) -- display grid select and go to selected window
    , ((modMask, xK_Left), prevWS) -- switch to previous workspace
    , ((modMask, xK_Right), nextWS) -- switch to next workspace
    , ((modMask, xK_p), spawn "dmenu_run")
    , ((modMask .|. shiftMask, xK_Tab), windows W.focusUp) -- move focus to the previous window
    , ((modMask .|. shiftMask, xK_g), gridselectWorkspace myGSConfig W.view) -- display grid select and go to selected workspace
    , ((modMask .|. shiftMask, xK_h), sendMessage Shrink) -- shrink the master area
    , ((modMask .|. shiftMask, xK_j), windows W.swapDown) -- swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k), windows W.swapUp)  -- swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_l), sendMessage Expand) -- expand the master area
    , ((modMask .|. shiftMask, xK_Return), focusUrgent) -- move focus to urgent window
    , ((modMask .|. controlMask, xK_q), io (exitWith ExitSuccess)) -- quit xmonad
    , ((modMask .|. controlMask, xK_r), spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart") -- restart xmonad
    , ((modMask .|. controlMask, xK_d), withFocused $ windows . W.sink) -- push window back into tiling
    , ((modMask .|. controlMask, xK_f), setLayout $ XMonad.layoutHook conf) -- reset the layouts on the current workspace to default
    , ((modMask .|. controlMask, xK_h), sendMessage MirrorExpand) -- expand the height/width
    , ((modMask .|. controlMask, xK_j), windows W.swapDown) -- swap the focused window with the next window
    , ((modMask .|. controlMask, xK_k), windows W.swapUp)  -- swap the focused window with the previous window
    , ((modMask .|. controlMask, xK_l), sendMessage MirrorShrink) -- shrink the height/width
    , ((modMask .|. shiftMask, xK_c), kill) --close focused window
    , ((modMask .|. controlMask, xK_Left), withFocused (keysMoveWindow (-30,0))) -- move floated window 10 pixels left
    , ((modMask .|. controlMask, xK_Right), withFocused (keysMoveWindow (30,0))) -- move floated window 10 pixels right
    , ((modMask .|. controlMask, xK_Up), withFocused (keysMoveWindow (0,-30))) -- move floated window 10 pixels up
    , ((modMask .|. controlMask, xK_Down), withFocused (keysMoveWindow (0,30))) -- move floated window 10 pixels down
    ]
    ++
    [ ((m .|. modMask, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9] -- mod-[F1..F9], switch to workspace n
    --, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- mod-shift-[F1..F9], move window to workspace n
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] -- mod-shift-[F1..F9], move window to workspace n
    ]
    ++
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_F10, xK_F11, xK_F12] [0..] -- mod-{F10,F11,F12}, switch to physical/Xinerama screens 1, 2, or 3
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] -- mod-shift-{F10,F11,F12}, move window to screen 1, 2, or 3
    ]
 
-- Mouse bindings:
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- set the window to floating mode and move by dragging
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster)) -- raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) -- set the window to floating mode and resize by dragging
    , ((modMask, button4), (\_ -> prevWS)) -- switch to previous workspace
    , ((modMask, button5), (\_ -> nextWS)) -- switch to next workspace
    ]
 
-- Window rules:
myManageHook = composeAll . concat $
    [ [isDialog --> doFloat]
    , [className =? c --> doFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "1:irc" | x <- my1Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "2:www" | x <- my2Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShift "3:music" | x <- my3Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "4:misc" | x <- my4Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "5:xbmc" | x <- my5Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "6:GIMP" | x <- my6Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "7:slideshow!" | x <- my7Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "8:foo()" | x <- my8Shifts]
    , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "9:vbox" | x <- my9Shifts]
    ]
    where
    doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Ekiga", "MPlayer", "Nitrogen", "Nvidia-settings", "Skype", "Sysinfo", "XCalc", "XFontSel", "Xmessage"]
    myTFloats = ["Downloads", "Iceweasel Preferences", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window", "kdesktop"]
    my1Shifts = ["Chromium"]
    my2Shifts = ["rxvt"]
    my3Shifts = []
    my4Shifts = ["NetBeans"]
    my5Shifts = ["MPlayer"]
    my6Shifts = []
    my7Shifts = []
    my8Shifts = []
    my9Shifts = []
 
-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
    { ppCurrent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHidden = wrap ("^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    --, ppHiddenNoWindows = wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()^i(" ++ myIconDir ++ "/corner.xbm)") "^fg()^bg()^p()" . dropIx $ wsId
    , ppUrgent = wrap (("^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()^i(" ++ myIconDir ++ "/corner.xbm)^fg(" ++ myUrgentFGColor ++ ")")) "^fg()^bg()^p()" . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tall-right.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-mirror-bottom.xbm)"
        --"Hinted combining Tabbed Bottom Simplest and Full with DragPane  Vertical 0.1 0.8" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        "Hinted combining Tabbed Bottom Simplest and Full with TwoPane using Not (Role \"gimp-toolbox\")" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["bro", "cns", "music", "4", "5"]
 
-- dynamicLog pretty printer for dzen:
myDzenPP_ h = defaultPP
    { ppCurrent = wrap ("^p(2)^ib(1)^fg(" ++ myFocusedBGColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()``^fg(" ++ myNormalFGColor ++ ")^p(2)") ("^p(2)^fg(" ++ myFocusedBGColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId
    , ppVisible = wrap ("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()``^fg(" ++ myNormalFGColor ++ ")^p(2)") ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId
    , ppHidden = wrap ("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg()^bg()^p()``^p(2)") ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^p()^ib(0)^fg()^bg()^p()") . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    , ppHiddenNoWindows = \wsId -> if wsId `notElem` staticWs then "" else wrap ("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myDzenFGColor ++ ")^bg()^p()``^p(2)") ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . dropIx $ wsId
    , ppUrgent = wrap (("^p(2)^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^fg(" ++ myUrgentFGColor ++ ")^bg(" ++ myNormalBGColor ++ ")^p()``^fg(" ++ myUrgentFGColor ++ ")^p(2)")) ("^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()^bg()^p()") . \wsId -> dropIx wsId
    , ppSep = " "
    , ppWsSep = ""
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap ("^ib(1)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_left.xbm)^r(1300x12)^p(-1300)^p(2)^fg()< ") (" >^p(2)^fg(" ++ myPatternColor ++ ")^i(" ++ myIconDir ++ "/corner_right.xbm)^fg(" ++ myNormalBGColor ++ ")^r(1300x12)^p(-1300)^ib(0)^fg()")
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-full.xbm)"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-tall-right.xbm)"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-mirror-bottom.xbm)"
        --"Hinted combining Tabbed Bottom Simplest and Full with DragPane  Vertical 0.1 0.8" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        "Hinted combining Tabbed Bottom Simplest and Full with TwoPane using Not (Role \"gimp-toolbox\")" -> "^fg(" ++ myIconFGColor ++ ")^i(" ++ myIconDir ++ "/layout-gimp.xbm)"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
    where
    dropIx wsId = if (':' `elem` wsId) then drop 2 wsId else wsId
    staticWs = ["bro", "cns", "music", "4", "5"]
