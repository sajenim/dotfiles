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
import XMonad.Util.Loggers
-- Layouts.
import XMonad.Layout.Spacing                            -- Add gaps between windows.
import XMonad.Layout.BinarySpacePartition hiding (Swap) -- Split the focused window in half, based off of BSPWM.
import XMonad.Layout.Renamed                            -- Modify the description of a layout.
import XMonad.Layout.LayoutCombinators                  -- Combine multiple layouts into one composite layout.
import XMonad.Layout.PerWorkspace                       -- Configure layouts on a per-workspace basis.
-- Hooks.
import XMonad.Hooks.EwmhDesktops                        -- Tell panel applications about its workspaces and the windows there in.
import XMonad.Hooks.ManageDocks                         -- Tools to automatically manage dock type programs.
import XMonad.Hooks.DynamicLog                          -- Output status information to an external status bar program such as xmobar.
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
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
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

-- My Default Options.
myModMask     = mod1Mask
myTerminal    = "st -e tmux"
myBorderWidth = 0
-- Define our workspace names here.
myWorkspaces  = 
  -- Default Misc Workspaces.
  ["1","2","3","4","5","6","7","8","9"
  -- Extra Dedicated Workspaces.
  ,"Dev","Media","Web","Games"
  ]
            
-- The Main Configuration.
myConfig = def
  -- myDefaults
  { modMask     = myModMask
  , terminal    = myTerminal
  , borderWidth = myBorderWidth
  , workspaces  = myWorkspaces
  -- myHooks
  , manageHook  = myManageHook
  , layoutHook  = myLayoutHook
  }

  -- The Keybinding Configuration.
  `additionalKeys`
  -- Holy Trinity.
  [ ((myModMask, xK_Return), spawn $ myTerminal) -- Open the terminal.
  , ((myModMask, xK_Delete), kill              ) -- Kill the focused program.
  , ((myModMask, xK_slash ), spawn "dmenu_run" ) -- Open application launcher.
  -- Miscellaneous Binds.
  , ((myModMask, xK_Tab), nextScreen              ) -- Cycle monitor focus.
  , ((myModMask, xK_q  ), sendMessage Rotate      ) -- Rotate layout horizontal | vertical.
  , ((myModMask, xK_s  ), sendMessage ToggleStruts) -- Toggle bar hide | show.
  -- Window Navigation.
  , ((myModMask, xK_j), windows W.focusDown  ) -- Move focus to next window.
  , ((myModMask, xK_k), windows W.focusUp    ) -- Move focus to previous window.
  , ((myModMask, xK_m), windows W.focusMaster) -- Move focus to master.
  -- Window Swapping.
  , ((myModMask .|. shiftMask, xK_j), windows W.swapDown  ) -- Swap with the next window.
  , ((myModMask .|. shiftMask, xK_k), windows W.swapUp    ) -- Swap with the previous window.
  , ((myModMask .|. shiftMask, xK_m), windows W.swapMaster) -- Swap with the master window.
  -- Binary Space Partition.
  , ((myModMask .|. controlMask, xK_h), sendMessage $ ExpandTowards L) -- Expand window left.
  , ((myModMask .|. controlMask, xK_j), sendMessage $ ExpandTowards D) -- Expand window down.
  , ((myModMask .|. controlMask, xK_k), sendMessage $ ExpandTowards U) -- Expand window up.
  , ((myModMask .|. controlMask, xK_l), sendMessage $ ExpandTowards R) -- Expand window right.
  -- Workspace Navigation. (Move to workspace)
  , ((myModMask, xK_1), windows $ W.view "1") -- Focus ws1
  , ((myModMask, xK_2), windows $ W.view "2") -- Focus ws2
  , ((myModMask, xK_3), windows $ W.view "3") -- Focus ws3
  , ((myModMask, xK_4), windows $ W.view "4") -- Focus ws4
  , ((myModMask, xK_5), windows $ W.view "5") -- Focus ws5
  , ((myModMask, xK_6), windows $ W.view "6") -- Focus ws6
  , ((myModMask, xK_7), windows $ W.view "7") -- Focus ws7
  , ((myModMask, xK_8), windows $ W.view "8") -- Focus ws8
  , ((myModMask, xK_9), windows $ W.view "9") -- Focus ws9
  -- Workspace Navigation. (Move window to workspace)
  , ((myModMask .|. shiftMask, xK_1), windows $ W.shift "1") -- Move to ws1
  , ((myModMask .|. shiftMask, xK_2), windows $ W.shift "2") -- Move to ws2
  , ((myModMask .|. shiftMask, xK_3), windows $ W.shift "3") -- Move to ws3
  , ((myModMask .|. shiftMask, xK_4), windows $ W.shift "4") -- Move to ws4
  , ((myModMask .|. shiftMask, xK_5), windows $ W.shift "5") -- Move to ws5
  , ((myModMask .|. shiftMask, xK_6), windows $ W.shift "6") -- Move to ws6
  , ((myModMask .|. shiftMask, xK_7), windows $ W.shift "7") -- Move to ws7
  , ((myModMask .|. shiftMask, xK_8), windows $ W.shift "8") -- Move to ws8
  , ((myModMask .|. shiftMask, xK_9), windows $ W.shift "9") -- Move to ws9
  -- Dedicated Workspaces.
  , ((myModMask, xK_F1), windows $ W.view "Dev"  ) -- Open developer workspace.
  , ((myModMask, xK_F2), windows $ W.view "Media") -- Open discord workspace.
  , ((myModMask, xK_F3), windows $ W.view "Web"  ) -- Open Firefox workspace.
  , ((myModMask, xK_F4), windows $ W.view "Games") -- Open Steam workspace.
  -- Jump to Layouts.
  , ((myModMask, xK_F5), sendMessage $ JumpToLayout "bsp" ) -- Open bsp layout.
  , ((myModMask, xK_F8), sendMessage $ JumpToLayout "Full") -- Open full layout.
  -- Fkey Miscellaneous.
  , ((myModMask, xK_F12), spawn "systemctl suspend") -- Put computer to sleep
  ]

-- The Manage Hook.
myManageHook :: ManageHook
myManageHook = composeAll
  [ isDialog  --> doFloat ]

-- The Layout Hook.
myLayoutHook = myBSP ||| Full
  where
    -- Our pretty default layout.
    myBSP = renamed [Replace "bsp"] . myDecorate . myGaps $ emptyBSP
    -- The configuration for our gaps.
    myGaps = spacingRaw True (Border 40 40 40 40) True (Border 10 10 10 10) True

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

-- Xmobar Pretty Printing.
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
