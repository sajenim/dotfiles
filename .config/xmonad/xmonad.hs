--------------------------------
-- Author: minnie             --
-- Server: fuchsia.kanto.dev  --
--------------------------------

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- Module Imports.
import XMonad hiding ( (|||) )
-- Configurations.
import XMonad.Config.Desktop                            -- Desktop environment integration.
-- Actions.
import XMonad.Actions.CycleWS                           -- Bindings to cycle between workspaces and screens.
-- Utililities.
import XMonad.Util.EZConfig                             -- Keybinding Configuration.
import XMonad.Util.SpawnOnce                            -- Spawn Program Once on Startup.
-- Layouts.
import XMonad.Layout.Spacing                            -- Add gaps between windows.
import XMonad.Layout.BinarySpacePartition hiding (Swap) -- Split the focused window in half, based off of BSPWM.
import XMonad.Layout.Renamed                            -- Modify the description of a layout.
import XMonad.Layout.LayoutCombinators                  -- Combine multiple layouts into one composite layout.
import XMonad.Layout.PerWorkspace                       -- Configure layouts on a per-workspace basis.'
-- Hooks.
import XMonad.Hooks.EwmhDesktops                        -- Tell panel applications about its workspaces and the windows there in.
import XMonad.Hooks.ManageDocks                         -- Tools to automatically manage dock type programs.
import XMonad.Hooks.DynamicLog                          -- Output status information to an external status bar program such as xmobar.
import XMonad.Hooks.ManageHelpers                       -- Provides helper functions to be used in manageHook.
-- Required for DecorationStyle. 
import qualified XMonad.StackSet as W                   -- Encodes a window manager abstraction.
import XMonad.Layout.Decoration                         -- Creating decorated layouts.
import XMonad.Util.Types                                -- Miscellaneous commonly used types.

-- The Main Function.
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . docks
     $ myConfig

-- My Default Options.
myModMask     = mod1Mask
myTerminal    = "st -e tmux"
myBorderWidth = 0
myWorkspaces  = ["dev","media","web","games", "artwork", "extra"]
            
-- The Main Configuration.
myConfig = def
  -- myDefaults.
  { modMask     = myModMask
  , terminal    = myTerminal
  , borderWidth = myBorderWidth
  , workspaces  = myWorkspaces
  -- myHooks.
  , manageHook  = myManageHook
  , layoutHook  = myLayoutHook
  } `additionalKeys` addKeys `removeKeys` delKeys

-- The Keybinding Configuration.
-- Define numpad keys.
numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert]                            -- 0
-- Remove default bindings.
delKeys = [ (myModMask, xK_space) ]
-- Create additional bindings.
addKeys =
  -- Spawn programs.
  [ ((myModMask, xK_Return), spawn myTerminal         ) -- Open the terminal.
  , ((myModMask, xK_slash ), spawn "dmenu_run"        ) -- Open application launcher.
  , ((myModMask, xK_Escape), spawn "systemctl suspend") -- Put computer to sleep.
  -- Miscellaneous binds.
  , ((myModMask, xK_q), sendMessage Rotate      ) -- Rotate layout horizontal | vertical.
  , ((myModMask, xK_s), sendMessage ToggleStruts) -- Toggle bar hide | show.
  -- Window navigation.
  , ((myModMask, xK_j), windows W.focusDown  ) -- Move focus to next window.
  , ((myModMask, xK_k), windows W.focusUp    ) -- Move focus to previous window.
  , ((myModMask, xK_m), windows W.focusMaster) -- Move focus to master.
  -- Workspace navigation
  , ((myModMask, xK_equal), moveTo Next hiddenWS) -- Cycle to next hidden workspace.
  , ((myModMask, xK_minus), moveTo Prev hiddenWS) -- Cycle to previous hidden workspace.
  -- Window swapping.
  , ((myModMask .|. shiftMask, xK_j), windows W.swapDown  ) -- Swap with the next window.
  , ((myModMask .|. shiftMask, xK_k), windows W.swapUp    ) -- Swap with the previous window.
  , ((myModMask .|. shiftMask, xK_m), windows W.swapMaster) -- Swap with the master window.
  -- Binary space partition.
  , ((myModMask .|. controlMask, xK_h), sendMessage $ ExpandTowards L) -- Expand window left.
  , ((myModMask .|. controlMask, xK_j), sendMessage $ ExpandTowards D) -- Expand window down.
  , ((myModMask .|. controlMask, xK_k), sendMessage $ ExpandTowards U) -- Expand window up.
  , ((myModMask .|. controlMask, xK_l), sendMessage $ ExpandTowards R) -- Expand window right.
   -- Some special keys.
  , ((myModMask, xK_Delete   ), kill                  ) -- Kill the focused program.
  , ((myModMask, xK_Tab      ), nextScreen            ) -- Cycle monitor focus.
  , ((myModMask, xK_BackSpace), sendMessage NextLayout) -- Cycle layouts.
  ]
  ++ 
  -- More workspace navigation (enable numpad).
  [((m .|. myModMask, k), windows $ f i)
      | (i, k) <- zip myWorkspaces numPadKeys
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

-- The Manage Hook.
myManageHook :: ManageHook
myManageHook = composeAll
  [ isDialog  --> doFloat ]

-- The Layout Hook.
myLayoutHook = avoidStruts (myBSP ||| myFull)
  where
    -- Our pretty layouts.
    myBSP = renamed [Replace "bsp"] . myDecorate . myGaps $ emptyBSP
    myFull = renamed [Replace "full"] $ Full
    -- The configuration for our gaps.
    myGaps = spacingRaw False (Border 40 40 40 40) True (Border 10 10 10 10) True

-- My Wild Rose Theme.
myTheme :: Theme
myTheme = def
  { activeColor         = "#904b5b"
  , inactiveColor       = "#444c42"
  , activeBorderColor   = "#904b5b"
  , inactiveBorderColor = "#444c42"
  , decoWidth           = 20
  }

-- The Decoration Style.
myDecorate :: Eq a => l a -> ModifiedLayout (Decoration SideDecoration DefaultShrinker) l a
myDecorate = decoration shrinkText myTheme (SideDecoration L)

data SideDecoration a = SideDecoration Direction2D
  deriving (Show, Read)

instance Eq a => DecorationStyle SideDecoration a where

  shrink b (Rectangle _ _ dw dh) (Rectangle x y w h)
    | SideDecoration U <- b = Rectangle x (y + fi dh) w (h - dh)
    | SideDecoration R <- b = Rectangle x y (w - dw) h
    | SideDecoration D <- b = Rectangle x y w (h - dh)
    | SideDecoration L <- b = Rectangle (x + fi dw) y (w - dw) h

  pureDecoration b dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case b of
      SideDecoration U -> Rectangle x y w dh
      SideDecoration R -> Rectangle (x + fi (w - dw)) y dw h
      SideDecoration D -> Rectangle x (y + fi (h - dh)) w dh
      SideDecoration L -> Rectangle x y dw h
    | otherwise = Nothing
