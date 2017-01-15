{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import qualified Data.Map as M
import XMonad.Prompt
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS

main = xmonad =<< xmobar desktopConfig
    { borderWidth        = 10
    , terminal           = "termite"
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , modMask     = mod4Mask
    , layoutHook         = avoidStruts $ layoutHook desktopConfig
    , keys = myKeys <+> keys def
    , workspaces = ["1", "2", "3"]
    }

myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList
            [ ((modm, xK_u), windows W.focusDown)
            , ((modm, xK_e), windows W.focusUp)
            , ((modm .|. shiftMask, xK_u), windows W.swapDown)
            , ((modm .|. shiftMask, xK_e), windows W.swapUp)
            , ((modm, xK_h), nextWS)
            , ((modm, xK_t), prevWS)
            , ((modm, xK_l), withFocused $ windows . W.sink)
            , ((modm, xK_i), sendMessage Shrink)
            , ((modm, xK_d), sendMessage Expand)
            ]
