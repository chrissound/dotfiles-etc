import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Hooks.Minimize
import XMonad.Layout.Minimize


import qualified Data.Map as M
import XMonad.Prompt
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Layout.DragPane

main = xmonad $ navigation2D def
  (xK_k, xK_h, xK_j, xK_l)
  [
    (mod4Mask,               windowGo  ),
    (mod4Mask .|. shiftMask, windowSwap)
  ]
  True
  $ desktopConfig
    { borderWidth        = 4
    , terminal           = "lxterminal"
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , modMask     = mod4Mask
    , keys = myKeys <+> keys def
    , workspaces = ["a", "b", "c", "d"]
    , layoutHook = desktopLayoutModifiers $ Tall 1 (5/100) (1/2) ||| Full
    }

myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList
            [-- ((modm, xK_u), windows W.focusDown)
            --, ((modm, xK_e), windows W.focusUp)
            --, ((modm .|. shiftMask, xK_u), windows W.swapDown)
            --, ((modm .|. shiftMask, xK_e), windows W.swapUp)
              ((modm, xK_e), prevWS)
            , ((modm, xK_u), nextWS)
            , ((modm, xK_l), withFocused $ windows . W.sink)
            , ((modm .|. shiftMask, xK_i), sendMessage Shrink)
            , ((modm .|. shiftMask, xK_d), sendMessage Expand)
            , ((modm, xK_p), spawn "rofi -combi-modi drun,window -show combi -modi combi")
            , ((modm .|. shiftMask, xK_z), devSession)
            , ((modm, xK_slash), spawn $ XMonad.terminal conf)
            ]

devSession :: X ()
devSession = spawn " ~/Scripts/devEnv.sh ~/Scripts/openTerminal.sh"
