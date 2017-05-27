{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Graphics.X11.Xlib.Types
import GHC.Int
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.GridSelect
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion
import qualified XMonad.StackSet as SS

myModMask = mod4Mask

baseConfig = desktopConfig

getRectangle :: a -> Int32 -> (a, Rectangle)
getRectangle (a, index) = (a, Rectangle width width x y) where
    width = fromIntegral(100) :: GHC.Int.Int32
    x = fromIntegral(width * index) :: Dimension
    y = x 

data MyFull a = MyFull deriving (Show, Read)
instance LayoutClass MyFull a where
    description MyFull = "My experiment"
    pureLayout _ r s = [
        [(SS.focus s, Rectangle 50 50 0 0)] ++ zip (SS.up s) [1..(length (SS.up s))]
    ]

main = xmonad =<< xmobar defaultConfig {
    terminal = "lxterminal",
    workspaces = ["a","b","c"],
    modMask = mod4Mask,
    layoutHook = simpleDeco shrinkText defaultTheme (MyFull) ||| MyFull ||| Accordion
    --layoutHook = simpleDeco shrinkText defaultTheme (layoutHook defaultConfig) ||| Full ||| Accordion
} 

