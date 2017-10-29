{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

import qualified Data.Map                         as M
import           XMonad
import           XMonad.Actions.Navigation2D
import           XMonad.Config.Desktop
import qualified XMonad.StackSet                  as W
import           MiddleColumn
import qualified Data.Map.Strict                  as Map
import           System.Process
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.DynamicWorkspaces
import           XMonad.Actions.GridSelect
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FloatNext
import           XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import WindowColumn
import WindowColumn as Column (Column(..))

defaultThreeColumn :: (Float, Float, Float)
defaultThreeColumn = (0.15, 0.65, 0.2)

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
        ,"2 (Work)"
        ,"3 (Email)"
        ,"4 (Net)"
        ,"5 (Distr)"
        ,"6 (Distr 2)"
        ,"7 (Net 2)"
        ,"8 (Work 2)"
    ]
    , layoutHook = desktopLayoutModifiers $
      mkToggle (single FULL) (
        spacing 3 (getMiddleColumnSaneDefault 2 0.15 defaultThreeColumn) |||
        spacing 3 (getMiddleColumnSaneDefault 2 0.5 defaultThreeColumn) |||
        spacing 3 (getMiddleColumnSaneDefault 3 0.75 (0.27333, 0.45333, 0.27333)) |||
        spacing 3 (getMiddleColumnSaneDefault 3 0.75 (0.33333, 0.33333, 0.33333))
      )
    , handleEventHook = handleEventHook def <+> fullscreenEventHook
    , manageHook = floatNextHook <+> manageHook desktopConfig
    }

myKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
  ++
    zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
  ++
            [
            -- GridSelecet
              ((modm, xK_g),               goToSelected def)
            , ((modm .|. shiftMask, xK_g), bringSelected def)
            -- Resize left / right column width individually
            , ((mod1Mask, xK_i), sendMessage IncrementLeftColumnContainerWidth)
            , ((mod1Mask, xK_u), sendMessage DecrementLeftColumnContainerWidth)
            , ((mod1Mask, xK_d), sendMessage IncrementRightColumnContainerWidth)
            , ((mod1Mask, xK_h), sendMessage DecrementRightColumnContainerWidth)
            , ((mod1Mask, xK_y), sendMessage ResetColumnContainerWidth)
            -- Modify column pin count
            , ((modm , xK_period), sendMessage IncrementLeftColumnContainer)
            , ((modm , xK_y), sendMessage IncrementRightColumnContainer)
            , ((modm  .|. shiftMask, xK_y), sendMessage ResetColumnContainer)
            -- Modify master window count
            , ((modm, xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
            , ((modm, xK_apostrophe), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area
            -- Swop column positions
            , ((modm .|. shiftMask, xK_i), sendMessage SwopLeftColumn)
            , ((modm .|. shiftMask, xK_d), sendMessage SwopRightColumn)
            , ((mod1Mask, xK_r), sendMessage ResetColumn)
            -- Modify main column width
            , ((modm, xK_i), sendMessage Shrink)
            , ((modm, xK_d), sendMessage Expand)
            -- Message sending
            , ((modm, xK_b), sendMessage ToggleStruts)
            , ((mod1Mask, xK_f), sendMessage $ Toggle FULL)
            -- Rofi
            , ((modm .|. shiftMask, xK_p),              spawn "rofi  -combi-modi run -show combi -modi combi")
            , ((modm, xK_p), spawn "rofi  -combi-modi drun -show combi -modi combi")
            , ((modm, xK_w),              spawn "rofi  -combi-modi window -show combi -modi combi")
            , ((modm .|. shiftMask, xK_z), devSessionPrompt)
            -- Sticky window
            , ((mod1Mask, xK_l), windows copyToAll) -- @@ Make focused window always visible
            , ((mod1Mask, xK_c),  killAllOtherCopies) -- @@ Toggle window state back
            -- Focus nth window
            , ((modm .|. controlMask, xK_h), sendMessage $ FocusLeft (1 :: Int))
            , ((modm .|. controlMask, xK_t), sendMessage $ FocusLeft (2 :: Int))
            , ((modm .|. controlMask, xK_n), sendMessage $ FocusLeft (3 :: Int))
            , ((modm .|. controlMask, xK_s), sendMessage $ FocusLeft (4 :: Int))
            , ((modm .|. controlMask, xK_g), sendMessage $ FocusRight (1 :: Int))
            , ((modm .|. controlMask, xK_c), sendMessage $ FocusRight (2 :: Int))
            , ((modm .|. controlMask, xK_r), sendMessage $ FocusRight (3 :: Int))
            , ((modm .|. controlMask, xK_l), sendMessage $ FocusRight (4 :: Int))
            -- Swap master window with nth side window
            , ((mod1Mask .|. shiftMask, xK_h), sendMessage $ SwopLeft (1 :: Int))
            , ((mod1Mask .|. shiftMask, xK_t), sendMessage $ SwopLeft (2 :: Int))
            , ((mod1Mask .|. shiftMask, xK_n), sendMessage $ SwopLeft (3 :: Int))
            , ((mod1Mask .|. shiftMask, xK_s), sendMessage $ SwopLeft (4 :: Int))
            , ((mod1Mask .|. shiftMask, xK_g), sendMessage $ SwopRight (1 :: Int))
            , ((mod1Mask .|. shiftMask, xK_c), sendMessage $ SwopRight (2 :: Int))
            , ((mod1Mask .|. shiftMask, xK_r), sendMessage $ SwopRight (3 :: Int))
            , ((mod1Mask .|. shiftMask, xK_l), sendMessage $ SwopRight (4 :: Int))
            --
            , ((mod1Mask .|. shiftMask, xK_f), submap . M.fromList $
                [ ((mod1Mask .|. shiftMask, xK_h), sendMessage $ SwopTo 1 2 Column.Left)
                , ((mod1Mask .|. shiftMask, xK_t), sendMessage $ SwopTo 2 2 Column.Left)
                , ((mod1Mask .|. shiftMask, xK_n), sendMessage $ SwopTo 3 2 Column.Left)
                , ((mod1Mask .|. shiftMask, xK_s), sendMessage $ SwopTo 4 2 Column.Left)
                , ((mod1Mask .|. shiftMask, xK_g), sendMessage $ SwopTo 1 2 Column.Right)
                , ((mod1Mask .|. shiftMask, xK_c), sendMessage $ SwopTo 2 2 Column.Right)
                , ((mod1Mask .|. shiftMask, xK_r), sendMessage $ SwopTo 3 2 Column.Right)
                , ((mod1Mask .|. shiftMask, xK_l), sendMessage $ SwopTo 4 2 Column.Right)
                ]
            )
            -- Dynamic workspaces
            , ((modm .|. shiftMask, xK_r), renameWorkspace def)
            , ((modm .|. shiftMask, xK_a), addWorkspacePrompt def)
            -- Workspace navigation
            --, ((modm, xK_e), prevWS)
            --, ((modm, xK_u), nextWS)
            , ((modm, xK_e), moveTo Prev NonEmptyWS)
            , ((modm, xK_u), moveTo Next NonEmptyWS)
            -- Misc
            , ((modm, xK_slash), spawn $ XMonad.terminal conf)
            , ((modm, xK_f), updatePointer (0.5, 0.5) (0, 0))
            , ((modm, xK_l), withFocused $ windows . W.sink)
            , ((mod1Mask, xK_F3), spawn "amixer -q sset Master 5%-")
            , ((mod1Mask, xK_F4), spawn "amixer -q sset Master 5%+")
            , ((mod1Mask, xK_F2), spawn "amixer -q sset Master toggle")
            --, ((modm .|. shiftMask, xK_r), devWorkspacePrompt)
            --, ((modm, (fromIntegral button3)), (\w -> focus w >> Flex.mouseResizeWindow w))
            , ((modm, xK_backslash), windows W.focusDown)
            ]
data DynamicWorkspaceState = DynamicWorkspaceState {workspaceIndexMap :: Map.Map WorkspaceIndex WorkspaceTag}
  deriving (Typeable, Read, Show)
instance ExtensionClass DynamicWorkspaceState where
  initialValue = DynamicWorkspaceState Map.empty
  extensionType = PersistentExtension
type WorkspaceTag = String
-- | The workspace index is mapped to a workspace tag by the user and
-- can be updated.

-- renameWorkspace :: XPConfig -> X ()
-- renameWorkspace conf = do
--   let updateIndexMap old new = do
--           wmap <- XS.gets workspaceIndexMap
--           XS.modify $ \s -> s {workspaceIndexMap = Map.map (\t -> if t == old then new else t) wmap} :: DynamicWorkspaceState
--   workspacePrompt conf renameWorkspaceByName
--   x <- get
--   mapM_ (\x' -> do
--             let grrr = W.tag . W.workspace . W.current $ windowset x
--             let c = W.tag . W.workspace . W.current $ windowset x
--             updateIndexMap c (grrr ++ "("++show x' ++ ")")
--             ) ([1..length $ W.visible $ windowset x])
--   -- let wId = W.tag $ W.workspace $ W.current $ windowset x
--   -- z <- getWsIndex
--   -- liftIO $ appendFile "/tmp/xmonadChris\n" $ wId
--   -- case (z wId) of
--   --   Just x' -> renameWorkspaceByName $ "("++show (x' + 1) ++") " ++ wId
--   --   Nothing -> renameWorkspaceByName "erm"


devSessionPrompt :: X ()
devSessionPrompt = spawn "rofi -normal-window -show fb -modi fb:~/Scripts/rofi/xmonadRofi.sh"

devWorkspacePrompt :: X ()
devWorkspacePrompt = do
  x <- liftIO $ readProcess "rofi" ["-normal-window", "-show fb -modi fb:~/Scripts/rofi/xmonadSpace.sh"] ""
  renameWorkspaceByName x
