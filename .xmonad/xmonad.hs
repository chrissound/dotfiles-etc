{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

import qualified Data.Map                         as M
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D
import           XMonad.Config.Desktop
import           XMonad.Layout.ResizableTile
import qualified XMonad.StackSet                  as W

import qualified Data.Map.Strict                  as Map
import           FocusWindow
import           MiddleColumn
import           System.Process
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.GridSelect
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FloatNext
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Prompt
import           XMonad.Prompt.Workspace          (workspacePrompt)
import qualified XMonad.Util.ExtensibleState      as XS

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
    , focusedBorderColor = "#00FF00"
    , modMask     = mod4Mask
    , keys = myKeys <+> keys def
    , workspaces = [
         "1 (Core)"
        ,"2 (Etc)"
        ,"3 (Work)"
        ,"4 (CCreator)"
        ,"5 (ReportMan)"
        ,"6 (IDE)"
        ,"7 (???)"
    ]
    , layoutHook = desktopLayoutModifiers $
      (spacing 3 (MiddleColumn 0.25 1 0.040 0.25)) |||
      spacing 3 (MiddleColumn 0.25 1 0.040 0.5) |||
      spacing 3 (MiddleColumn 0.25 1 0.040 0.75) |||
      spacing 3 (MiddleColumn 0.25 1 0.040 0.33) |||
      ThreeCol 1 (3/100) (1/2) |||
      ResizableTall 2 (5/100) (1/2) [0.3, 0.5, 2] |||
      tallLayout |||
      Full
    , handleEventHook = handleEventHook def <+> fullscreenEventHook
    , manageHook = floatNextHook <+> manageHook desktopConfig
    }

  where tallLayout = Tall 1 (5/100) (1/2)

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
  ++
    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
  ++
            [-- ((modm, xK_u), windows W.focusDown)
            --, ((modm, xK_e), windows W.focusUp)
            --, ((modm .|. shiftMask, xK_u), windows W.swapDown)
            --, ((modm .|. shiftMask, xK_e), windows W.swapUp)
              ((modm, xK_backslash), windows W.focusDown)
            , ((modm, xK_g), goToSelected def)
            , ((modm, xK_f), updatePointer (0.5, 0.5) (0, 0))
            , ((modm, xK_e), prevWS)
            , ((modm, xK_u), nextWS)
            , ((modm, xK_l), withFocused $ windows . W.sink)
            , ((modm, xK_i), sendMessage Shrink)
            , ((modm, xK_d), sendMessage Expand)
            , ((modm .|. shiftMask, xK_i), sendMessage MirrorShrink)
            , ((modm .|. shiftMask, xK_d), sendMessage MirrorExpand)
            , ((modm, xK_p),              spawn "rofi  -combi-modi run -show combi -modi combi")
            , ((modm.|. shiftMask, xK_p), spawn "rofi  -combi-modi drun -show combi -modi combi")
            , ((modm, xK_w),              spawn "rofi  -combi-modi window -show combi -modi combi")
            , ((modm .|. shiftMask, xK_z), devSessionPrompt)
            , ((modm .|. shiftMask .|. controlMask, xK_r), Main.renameWorkspace def)
            , ((modm .|. shiftMask, xK_r), Main.renameWorkspace def)
            --, ((modm .|. shiftMask, xK_r), devWorkspacePrompt)
            , ((modm .|. shiftMask, xK_a), addWorkspacePrompt def)
            , ((modm, xK_slash), spawn $ XMonad.terminal conf)
            --, ((modm, (fromIntegral button3)), (\w -> focus w >> Flex.mouseResizeWindow w))
            , ((modm, xK_backslash), windows W.focusDown)
            , ((modm .|. controlMask, xK_h), windows (focusWindow 0))
            , ((modm .|. controlMask, xK_t), windows (focusWindow 1))
            , ((modm .|. controlMask, xK_n), windows (focusWindow 2))
            , ((modm .|. controlMask, xK_s), windows (focusWindow 3))
            , ((modm .|. controlMask, xK_g), windows (focusWindow 4))
            , ((modm .|. controlMask, xK_c), windows (focusWindow 5))
            , ((modm .|. controlMask, xK_r), windows (focusWindow 6))
            , ((modm .|. controlMask, xK_l), windows (focusWindow 7))
            , ((modm .|. controlMask, xK_z), focusWindowDraw)
            , ((modm .|. controlMask, xK_f), toggleFloatAllNew)
            ]
data DynamicWorkspaceState = DynamicWorkspaceState {workspaceIndexMap :: Map.Map WorkspaceIndex WorkspaceTag}
  deriving (Typeable, Read, Show)
instance ExtensionClass DynamicWorkspaceState where
  initialValue = DynamicWorkspaceState Map.empty
  extensionType = PersistentExtension
type WorkspaceTag = String
-- | The workspace index is mapped to a workspace tag by the user and
-- can be updated.

renameWorkspace :: XPConfig -> X ()
renameWorkspace conf = do
  let updateIndexMap old new = do
          wmap <- XS.gets workspaceIndexMap
          XS.modify $ \s -> s {workspaceIndexMap = Map.map (\t -> if t == old then new else t) wmap} :: DynamicWorkspaceState
  workspacePrompt conf renameWorkspaceByName
  x <- get
  mapM_ (\x' -> do
            let grrr = W.tag . W.workspace . W.current $ windowset x
            let c = W.tag . W.workspace . W.current $ windowset x
            updateIndexMap c (grrr ++ "("++show x' ++ ")")
            ) ([1..length $ W.visible $ windowset x])
  -- let wId = W.tag $ W.workspace $ W.current $ windowset x
  -- z <- getWsIndex
  -- liftIO $ appendFile "/tmp/xmonadChris\n" $ wId
  -- case (z wId) of
  --   Just x' -> renameWorkspaceByName $ "("++show (x' + 1) ++") " ++ wId
  --   Nothing -> renameWorkspaceByName "erm"


devSessionPrompt :: X ()
devSessionPrompt = spawn "rofi -normal-window -show fb -modi fb:~/Scripts/rofi/xmonadRofi.sh"

devWorkspacePrompt :: X ()
devWorkspacePrompt = do
  x <- liftIO $ readProcess "rofi" ["-normal-window", "-show fb -modi fb:~/Scripts/rofi/xmonadSpace.sh"] ""
  renameWorkspaceByName x
