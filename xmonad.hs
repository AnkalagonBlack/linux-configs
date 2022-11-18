import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Actions.UpdatePointer
import XMonad.Util.Dzen
import XMonad.Actions.Volume

import qualified XMonad.Actions.CycleWS as CW 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

unfloat = ask >>= doF . W.sink

-- unfloat = mod-t
--
terminus = "-*-terminus-*-*-*-*-24-*-*-*-*-*-*-*"
alert = dzenConfig centered . show
centered =
        onCurr (center 800 30)
    >=> font "-*-terminus-*-r-*-*-64-*-*-*-*-*-*-*"
    >=> addArgs ["-fg", "#80c0ff"]
    >=> addArgs ["-bg", "#000040"]


myKeys c = mkKeymap c $
           [ ("M-<Return>",   spawn $ XMonad.terminal c)
	   , ("M-p",          spawn "dmenu_run -fn 'Inconsolata 12'")
           , ("M-<Space>",    sendMessage NextLayout)
           , ("M-<Tab>",      windows W.focusDown)
           , ("M-S-<Return>", windows W.swapMaster)
           , ("M-S-k",        windows W.swapDown)
           , ("M-S-c",        kill)
           , ("M-S-q",        io (exitWith ExitSuccess))
           , ("M-b",          sendMessage ToggleStruts)
           , ("M-h",          sendMessage Shrink)
           , ("M-l",          sendMessage Expand)
           , ("M-n",          refresh)
           , ("M-r",          spawn "scrot -q 1 $HOME/screenshots/%Y-%m-%d-%H:%M:%S.png")
           , ("M-q",          broadcastMessage ReleaseResources >> restart "xmonad" True)
           , ("M-t",          withFocused $ windows . W.sink)
           , ("M-x",          shellPrompt defaultXPConfig)
           , ("M-u",          spawn "sudo pm-suspend")
           , ("M-S-i",          spawn "sudo pm-hibernate")
           , ("M-S-l",          spawn "slock")
	   , ("M-e",	      CW.nextScreen)
           , ("M-w",          CW.prevScreen)
           , ("M-[",          alert "ZZZZZZ")
           , ("M-]",          alert ">")]
           ++
           [(m ++ k, windows $ f w)
                | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
           , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]

-- addKeys = [((mod4Mask .|. m, k), windows $ f i)
--                | (i, k) <- zip (map show [1..9]) numPadKeys
--                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--
numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert]                            -- 0


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myLayoutHook = smartBorders $ avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 4/100

myManageHook =  manageDocks <+> composeAll
               [ floatC "MPlayer"
               , floatC "Gimp"
               , moveToC "Emacs" "5"
               , moveToC "google-chrome" "1"
               , moveToC "Firefox" "1"
               , moveToC "Navigator" "1"
               , moveToC "jetbrains-idea-ce" "6"
               , moveToC "jetbrains-idea" "6"
               , moveToC "Steam" "7"
               , moveToC "jetbrains-pycharm-ce" "4"
               , moveToC "jetbrains-pycharm" "4"
               , moveToC "jetbrains-clion" "4"
               , moveToC "TelegramDesktop" "5"
               , moveToC "Slack - GridGain" "5"
               , moveToC "Slack - JetBrains" "5"
               , moveToC "skype" "3"
               , moveToC "Skype" "3"
               , moveToC "URxvt" "2"
               , moveToC "GitKraken" "6"
               , moveToC "discord" "8"
	       , resource =? "stalonetray" --> doIgnore
	       , className =? "csgo_linux64" --> unfloat
               ]
    where moveToC c w = className =? c --> doF (W.shift w)
          moveToT t w = title     =? t --> doF (W.shift w)
          floatC  c   = className =? c --> doFloat

myLogHook xmobar = (dynamicLogWithPP $ defaultPP {
                     ppOutput = hPutStrLn xmobar
                   , ppTitle = xmobarColor "white" "" . shorten 110
                   , ppCurrent = xmobarColor "white" "black" . pad
                   , ppHidden = pad
                   , ppHiddenNoWindows = \w -> xmobarColor "#444" "" (" " ++ w ++ " ")
                   , ppSep = xmobarColor "#555" "" " / "
                   , ppWsSep = ""
                   , ppLayout = \x -> case x of
                                        "Tall" -> "T"
                                        "Mirror Tall" -> "M"
                                        "Full" -> "F"
                                        _ -> "?"
                   }) >> updatePointer (0.25, 0.25) (0.25, 0.25) 

main = do xmobar <- spawnPipe "xmobar"
          xmonad $ docks defaultConfig {
                       terminal           = "urxvt",
                       focusFollowsMouse  = True,
                       borderWidth        = 2,
                       modMask            = mod4Mask,
                       workspaces         = [ show x | x <- [1..9] ],
                       normalBorderColor  = "#444",
                       focusedBorderColor = "#f00",
                       keys               = myKeys,
                       mouseBindings      = myMouseBindings,
                       layoutHook         = myLayoutHook,
                       manageHook         = myManageHook,
                       logHook            = myLogHook xmobar
                     } --`additionalKeys` addKeys

