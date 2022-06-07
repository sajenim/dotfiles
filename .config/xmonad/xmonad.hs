--------------------------------
-- Author: minnie             --
-- Server: fuhchsia.kanto.dev --
--------------------------------

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- Module Imports.
import XMonad
-- Configurations.
import XMonad.Config.Desktop                            -- Desktop environment integration.
-- Actions.
import XMonad.Actions.CycleWS                           -- Bindings to cycle between workspaces and screens.
-- Utililities.
import XMonad.Util.EZConfig                             -- Keybinding Configuration.
import XMonad.Util.SpawnOnce                            -- Spawn Program Once on Startup.
-- Layouts.
import XMonad.Layout.Spacing                            -- Add gaps between windows.
import XMonad.Layout.WindowNavigation                   -- Allows easy navigation of a workspace.
import XMonad.Layout.BinarySpacePartition hiding (Swap) -- Split the focused window in half, based off of BSPWM.
-- Hooks.
import XMonad.Hooks.EwmhDesktops                        -- Tell panel applications about its workspaces and the windows there in.
import XMonad.Hooks.ManageDocks                         -- Tools to automatically manage dock type programs.
import XMonad.Hooks.ManageHelpers                       -- Provides helper functions to be used in manageHook.
-- Required for DecorationStyle. 
import qualified XMonad.StackSet as W                   -- Encodes a window manager abstraction.
import XMonad.Layout.Decoration                         -- Creating decorated layouts.
import XMonad.Util.Types                                -- Miscellaneous commonly used types.

-- The Startup Hook.
myStartupHook = do
  spawnOnce "xrandr --output HDMI-A-0 --mode 1920x1080 --rotate left --output DisplayPort-0 --mode 2560x1440 --right-of HDMI-A-0"
  spawnOnce "xdotool key Num_Lock"
  spawnOnce "~/.fehbg"

-- The Main Function.
main = do
  xmonad $ ewmhFullscreen . ewmh . docks $ myConfig

-- My Default Options.
myModMask     = mod1Mask
myTerminal    = "st -e tmux"
myBorderWidth = 0
myWorkspaces  = ["1","2","3","4","5","6","7","8","9"]
            
-- The Main Configuration.
myConfig = def
  -- myDefaults
  { modMask     = myModMask
  , terminal    = myTerminal
  , borderWidth = myBorderWidth
  , workspaces  = myWorkspaces
  -- myHooks
  , startupHook = myStartupHook
  , manageHook  = myManageHook
  , layoutHook  = myLayoutHook
  }

  -- The Keybinding Configuration.
  `additionalKeys`
  [ ((myModMask, xK_Delete   ), kill                    )
  , ((myModMask, xK_Return   ), spawn $ myTerminal      )
  , ((myModMask, xK_BackSpace), spawn "dmenu_run"  ) 
  , ((myModMask, xK_Tab      ), sendMessage $ Rotate    )
  , ((myModMask, xK_F11      ), sendMessage NextLayout  )
  , ((myModMask, xK_F12      ), sendMessage ToggleStruts)
  -- Window Navigation.
  , ((myModMask, xK_Up   ), sendMessage $ Go U)
  , ((myModMask, xK_Down ), sendMessage $ Go D)
  , ((myModMask, xK_Left ), sendMessage $ Go L)
  , ((myModMask, xK_Right), sendMessage $ Go R)
  -- Workspace Navigation. (Move to workspace)
  , ((myModMask, xK_KP_End      ), windows $ W.view "1") -- Numpad 1
  , ((myModMask, xK_KP_Down     ), windows $ W.view "2") -- Numpad 2
  , ((myModMask, xK_KP_Page_Down), windows $ W.view "3") -- Numpad 3
  , ((myModMask, xK_KP_Left     ), windows $ W.view "4") -- Numpad 4
  , ((myModMask, xK_KP_Begin    ), windows $ W.view "5") -- Numpad 5
  , ((myModMask, xK_KP_Right    ), windows $ W.view "6") -- Numpad 6
  , ((myModMask, xK_KP_Home     ), windows $ W.view "7") -- Numpad 7
  , ((myModMask, xK_KP_Up       ), windows $ W.view "8") -- Numpad 8
  , ((myModMask, xK_KP_Page_Up  ), windows $ W.view "9") -- Numpad 9
  -- Workspace Navigation. (Move window to workspace)
  , ((myModMask .|. shiftMask, xK_KP_End      ), windows $ W.shift "1") -- Numpad 1
  , ((myModMask .|. shiftMask, xK_KP_Down     ), windows $ W.shift "2") -- Numpad 2
  , ((myModMask .|. shiftMask, xK_KP_Page_Down), windows $ W.shift "3") -- Numpad 3
  , ((myModMask .|. shiftMask, xK_KP_Left     ), windows $ W.shift "4") -- Numpad 4
  , ((myModMask .|. shiftMask, xK_KP_Begin    ), windows $ W.shift "5") -- Numpad 5
  , ((myModMask .|. shiftMask, xK_KP_Right    ), windows $ W.shift "6") -- Numpad 6
  , ((myModMask .|. shiftMask, xK_KP_Home     ), windows $ W.shift "7") -- Numpad 7
  , ((myModMask .|. shiftMask, xK_KP_Up       ), windows $ W.shift "8") -- Numpad 8
  , ((myModMask .|. shiftMask, xK_KP_Page_Up  ), windows $ W.shift "9") -- Numpad 9
  -- Window Swapping.
  , ((myModMask .|. shiftMask, xK_Up   ), sendMessage $ Swap U)
  , ((myModMask .|. shiftMask, xK_Down ), sendMessage $ Swap D)
  , ((myModMask .|. shiftMask, xK_Left ), sendMessage $ Swap L)
  , ((myModMask .|. shiftMask, xK_Right), sendMessage $ Swap R)
  -- Binary Space Partition.
  , ((myModMask .|. controlMask, xK_Up   ), sendMessage $ ExpandTowards U)
  , ((myModMask .|. controlMask, xK_Down ), sendMessage $ ExpandTowards D)
  , ((myModMask .|. controlMask, xK_Left ), sendMessage $ ExpandTowards L)
  , ((myModMask .|. controlMask, xK_Right), sendMessage $ ExpandTowards R)
  ]
  
-- The Layout Hook.
myLayoutHook = (myModifier emptyBSP ||| avoidStruts Full)
  where
    myModifier = avoidStruts . myDecorate . myGaps . windowNavigation
       where
         myGaps = spacingRaw True (Border 40 40 40 40) True (Border 10 10 10 10) True

-- The Manage Hook.
myManageHook = manageDocks

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
