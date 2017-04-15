{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.ResizableTile
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D

import XMonad.Layout.Spacing
import MiddleColumn
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.ThreeColumns
import XMonad.Actions.UpdatePointer

main :: IO ()
main = xmonad $ ewmh $ navigation2D def
  (xK_k, xK_h, xK_j, xK_l)
  [
    (mod4Mask,               windowGo  ),
    (mod4Mask .|. shiftMask, windowSwap)
  ]
  True
  $ desktopConfig
    { borderWidth        = 3
    , terminal           = "lxterminal"
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , modMask     = mod4Mask
    , keys = myKeys <+> keys def
    , workspaces = ["a", "b", "c", "d", "e"]
    , layoutHook = desktopLayoutModifiers $
      spacing 5 (MiddleColumn 0.25 1 0.040 0.25) |||
      ThreeCol 1 (3/100) (1/2) |||
      ResizableTall 2 (5/100) (1/2) [0.3, 0.5, 2] |||
      tallLayout |||
      Full
    , handleEventHook = handleEventHook def <+> fullscreenEventHook
    -- , logHook = dynamicLog
    }

  where tallLayout = Tall 1 (5/100) (1/2)

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList
            [-- ((modm, xK_u), windows W.focusDown)
            --, ((modm, xK_e), windows W.focusUp)
            --, ((modm .|. shiftMask, xK_u), windows W.swapDown)
            --, ((modm .|. shiftMask, xK_e), windows W.swapUp)
              ((modm, xK_backslash), windows W.focusDown)
            , ((modm, xK_f), updatePointer (0.5, 0.5) (0, 0))
            , ((modm, xK_e), prevWS)
            , ((modm, xK_u), nextWS)
            , ((modm, xK_l), withFocused $ windows . W.sink)
            , ((modm, xK_i), sendMessage Shrink)
            , ((modm, xK_d), sendMessage Expand)
            , ((modm .|. shiftMask, xK_i), sendMessage MirrorShrink)
            , ((modm .|. shiftMask, xK_d), sendMessage MirrorExpand)
            , ((modm, xK_p), spawn "rofi  -combi-modi drun,window -show combi -modi combi")
            , ((modm .|. shiftMask, xK_z), devSessionPrompt)
            , ((modm, xK_slash), spawn $ XMonad.terminal conf)
            --, ((modm, (fromIntegral button3)), (\w -> focus w >> Flex.mouseResizeWindow w))
            ]

devSessionPrompt :: X ()
devSessionPrompt = spawn "rofi -normal-window -show fb -modi fb:~/Scripts/rofi/xmonadRofi.sh"
