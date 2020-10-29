--------------------------------------------------------------------------------
-- | xmonad.hs
--
-- This configuration file for xmonad is adapted from DT's dotfiles.
-- See: https://gitlab.com/dwt1/dotfiles
--
-- hlint xmonad.hs
--------------------------------------------------------------------------------
-- Examples:
-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips
-- https://wiki.haskell.org/Xmonad/Config_archive/adamvo%27s_xmonad.hs
-- https://wiki.haskell.org/Xmonad/Config_archive/dmwit%27s_xmonad.hs
-- https://gitlab.com/dwt1/dotfiles  (xmonad and xmobar configuration)
-- https://xiangji.me/2018/11/19/my-xmonad-configuration/
--
{-# OPTIONS_GHC -Wall -fwarn-unused-imports #-}

import           System.Exit
import           System.IO                        (hPutStrLn)

-- import Data.Typeable
import qualified Data.Map                         as M

import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Actions.MouseResize
import           XMonad.Actions.NoBorders
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Actions-OnScreen.html
--import XMonad.Actions.OnScreen
import           XMonad.Actions.RotSlaves         (rotAllDown, rotSlavesDown)
import           XMonad.Actions.SwapWorkspaces
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog          (PP (..), dynamicLogWithPP,
                                                   shorten, wrap, xmobarColor,
                                                   xmobarPP)
-- import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Hooks.ManageDocks         (ToggleStruts (..),
                                                   avoidStruts, docksEventHook,
                                                   manageDocks)
import           XMonad.Hooks.ManageHelpers
-- import XMonad.Hooks.ScreenCorners
-- import XMonad.Hooks.SetWMName
-- import XMonad.Layout.BinarySpacePartition (emptyBSP)
-- import XMonad.Layout.Grid (Grid(..))
import           XMonad.Layout.IndependentScreens
-- import XMonad.Layout.LayoutModifier (ModifiedLayout)
-- import           XMonad.Layout                   
import           XMonad.Layout.NoBorders          (noBorders)
import           XMonad.Layout.ResizableTile      (ResizableTall (..))
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ToggleLayouts      (ToggleLayout (..),
                                                   toggleLayouts)
import           XMonad.Layout.WindowArranger     (windowArrange)
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Shell
import qualified XMonad.StackSet                  as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run                  (runInTerm, spawnPipe)
import           XMonad.Util.SpawnOnce

------------------------------------------------------------------------
-- DEFINITIONS
------------------------------------------------------------------------

myTerminal :: String
myTerminal = "xterm"

myFloatingTerminal :: String
myFloatingTerminal = "xterm -title \"floatterm\""

-- https://www.reddit.com/r/xmonad/comments/hm2tg0/how_to_toggle_floating_state_on_a_window/
toggleFloat :: Window -> X()
toggleFloat w = windows (\s -> if M.member w (W.floating s)
                               then W.sink w s
                               else W.float w (W.RationalRect 0 0 (1/2) (4/5)) s)

mySpacing :: Integer
mySpacing = 5

myBorderWidth :: Dimension
myBorderWidth = 2

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

myEditor :: String
myEditor = "bin/ec"

myGHCI :: String
myGHCI = "ghci"

myPython :: String
myPython = "python"

myFileManager :: String
myFileManager = "pcmanfm"

myKeepassXc :: String
myKeepassXc = "keepassxc"

myThunderbird :: String
myThunderbird = "thunderbird"

myFirefox :: String
myFirefox = "firefox"

myOpera :: String
myOpera = "opera --disable-seccomp-filter-sandbox"

myChromium :: String
myChromium = "chromium"

myGoogleChrome :: String
myGoogleChrome = "google-chrome-stable"

myTorBrowser :: String
myTorBrowser = "tor-browser"

-- myRedshiftOn :: String
-- myRedshiftOn = "redshift"

-- myRedshiftOff :: String
-- myRedshiftOff = "redshift ; redshift -x"

myScreensaverOn :: String
myScreensaverOn = "xscreensaver -no-splash &"

-- logCommand :: String
-- logCommand = "echo \"" ++ (show (typeOf defaults)) ++ "\" > /tmp/XMONAD.txt"

runItOnce :: String -> X ()
runItOnce cmd = spawn $ "~/bin/runonce " ++ cmd

-- killItOnce :: String -> X ()
-- killItOnce cmd = spawn $ "~/bin/killonce " ++ cmd

------------------------------------------------------------------------
-- WORKSPACES
------------------------------------------------------------------------
-- Workspaces are clickable meaning that the mouse can be used to switch
-- workspaces. This requires xdotool. You need to use UnsafeStdInReader instead
-- of simply StdInReader in xmobar config so you can pass actions to it.

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myFirstWorkspace :: Integer
myFirstWorkspace = 1

myLastWorkspace :: Integer
myLastWorkspace = 9

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
               $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  where
        clickable l = [ "<action=xdotool key super+" ++ show n ++
                        ">" ++ "<fn=2>" ++ ws ++ "</fn>"
                        ++ "</action>" |
                      (i,ws) <- zip [myFirstWorkspace .. myLastWorkspace] l,
                      let n = i ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length .
  W.integrate' . W.stack . W.workspace .
  W.current . windowset

------------------------------------------------------------------------
-- KEY BINDINGS
------------------------------------------------------------------------
-- Add some extra key bindings; M1 is Alt key.
myKeys :: [(String, X ())]
myKeys =
      [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))
      , ("M-p", shellPrompt myXPConfig)
      , ("M-t", withFocused toggleFloat)
      , ("M-<Esc>", sendMessage (Toggle "Full") >> sendMessage ToggleStruts)
      , ("M-f", sendMessage (Toggle "Full"))
      , ("M-<Backspace>", kill)
      , ("M-b", withFocused toggleBorder)
      , ("M-C-<Return>", spawn myFloatingTerminal)
      , ("M-/ e", spawn myEditor)
      , ("M-/ h", runInTerm "" myGHCI)
      , ("M-/ i", runInTerm "" myPython)
      , ("M-/ k", spawn myKeepassXc)
      , ("M-/ m", spawn myFileManager)
      , ("M-/ c", spawn myChromium)
      , ("M-/ g", spawn myGoogleChrome)
      , ("M-/ f", spawn myFirefox)
      , ("M-/ o", spawn myOpera)
      , ("M-/ p", spawn myTorBrowser)
      , ("M-/ t", spawn myThunderbird)
      , ("M-/ s h", spawn "pactl set-card-profile 0 output:hdmi-stereo")
      , ("M-/ s a", spawn "pactl set-card-profile 0 output:analog-stereo")
      , ("M-S-<Left>", sendMessage Shrink)
      , ("M-S-<Right>", sendMessage Expand)
      , ("M-<Left>", windows W.focusUp)
      , ("M-<Right>", windows W.focusDown)
      , ("M-C-<Down>", windows W.swapDown >> windows W.focusUp)
      , ("M-C-<Up>", windows W.swapUp >> windows W.focusDown)
      , ("M-M1-<Up>", rotSlavesDown)
      , ("M-M1-<Down>", rotAllDown)
      , ("M-=", toggleWS)
      , ("M-<Tab>", nextWS)
      , ("M-S-<Tab>", prevWS)
      , ("M-C-<Tab>", shiftToNext >> nextWS)
      , ("M-C-S-<Tab>", shiftToPrev >> prevWS)
      , ("M-M1-<Left>", prevWS)
      , ("M-M1-<Right>", nextWS)
      , ("M-C-M1-<Left>", shiftToPrev >> prevWS)
      , ("M-C-M1-<Right>", shiftToNext >> nextWS)
      , ("M-C-<Left>", prevScreen)
      , ("M-C-<Right>", nextScreen)
      , ("M-S-C-<Left>", shiftPrevScreen)
      , ("M-S-C-<Right>", shiftNextScreen)
      , ("M-S-C-<Up>", swapPrevScreen)
      , ("M-S-C-<Down>", swapNextScreen)
      , ("M-`", sendMessage NextLayout)
      -- , ("M-S-`", setLayout $ layoutHook conf)
      , ("M-<Space>", goToSelected def) --defaultGSConfig)
      -- , ("M-C-u", sendMessage Arrange)
      -- , ("M-C-d", sendMessage DeArrange)
      -- , ("M-r", runItOnce myRedshiftOn)
      -- , ("M-S-r", killItOnce myRedshiftOff)
      -- , ("M-l", spawn logCommand)
      , ("M-0", spawn "xscreensaver-command -lock")
      , ("M-C-0", spawn "xscreensaver-command -lock & systemctl suspend")
      , ("M-C-S-0", spawn "systemctl hibernate")
      , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
      , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
      , ("<XF86AudioMute>", spawn "amixer set Master toggle")
      , ("<XF86AudioPlay>", spawn "clementine -t")
      , ("<XF86AudioPrev>", spawn "clementine -r")
      , ("<XF86AudioNext>", spawn "clementine -f")
      ]

      -- Appending swap workspace keybindings (Mod+Control+# swaps with current WS).
      ++ [("M-C-" ++ k, windows $ swapWithCurrent w)
           | (w, k) <- zip myWorkspaces (map show [myFirstWorkspace .. myLastWorkspace])]

myStartupHook :: X ()
myStartupHook = do
  -- addScreenCorners [(SCUpperLeft, goToSelected def)] --defaultGSConfig)]
  spawnOnce "xsetroot -solid black"
  -- runItOnce myRedshiftOn
  spawnOnce myScreensaverOn
  runItOnce "emacs --daemon"

--------------------------------------------------------------------------------
main :: IO ()
main = do
  numberOfScreens <- countScreens
  if numberOfScreens > (1 :: Integer)
   then do
    spawn "xrandr --output LVDS-1 --primary --auto --output HDMI-1 --auto --left-of LVDS-1"
    spawn "pactl set-card-profile 0 output:hdmi-stereo"
    -- spawn "pactl set-card-profile 0 output:analog-stereo"
   else do
    spawn "xrandr --output LVDS-1 --primary --auto"
    spawn "pactl set-card-profile 0 output:analog-stereo"

  -- TODO -p position parameter for both instances.
  -- Launching instances of xmobar on their monitors. TODO check if only one monitor.
  xmproc0 <- spawnPipe "xmobar -b -p \"xpos=0 , width=1920, height=24\" -x 1 /home/mdo/.config/xmobar/xmobarrc0.hs"
  -- xmproc1 <- spawnPipe "xmobar -b -p \"xpos=1920 , ypos=744, width=1366, height=24\" -x 0 /home/mdo/.config/xmobar/xmobarrc1.hs"
  xmproc1 <- spawnPipe "xmobar -b -p \"xpos=1920 , width=1366, height=24\" -x 0 /home/mdo/.config/xmobar/xmobarrc1.hs"

  xmonad $ def {
    terminal = myTerminal,
    borderWidth = myBorderWidth,
    focusedBorderColor = myFocusedBorderColor,
    modMask = mod4Mask, -- Use the "Win" key for the mod key
    workspaces = myWorkspaces,
    manageHook = myManageHook <+> manageHook desktopConfig <+> manageDocks,
    handleEventHook = docksEventHook, -- <+> screenCornerEventHook,
    layoutHook = desktopLayoutModifiers myLayouts, -- $ screenCornerLayoutHook myLayouts,
    logHook = dynamicLogWithPP xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x  -- >> hPutStrLn xmproc2 x
          , ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
          , ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
          , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
          , ppHiddenNoWindows = xmobarColor "#b3afc2" ""        -- Hidden workspaces (no windows)
          , ppTitle = xmobarColor "#ffffff" "" . shorten 60     -- Title of active window in xmobar
          , ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
          , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
          , ppExtras  = [windowCount]                           -- # of windows current workspace
          , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        },
    startupHook = myStartupHook
    }
    `additionalKeysP` myKeys

--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- Use the 'M-<Esc>' key binding defined above to toggle between the
-- current layout and a full screen layout. Use 'M-f' key binding for
-- a full screen layout with xmobar visible at the top.
myLayouts = avoidStruts
  $ mouseResize
  $ windowArrange
  $ toggleLayouts (noBorders Full) others
  where
    others = spacingRaw True
                        (Border 0 mySpacing mySpacing mySpacing)
                        True
                        (Border mySpacing mySpacing mySpacing mySpacing)
                        True
             $ ResizableTall 1 (1.5/100) (2/3) []
                 ||| Mirror (ResizableTall 1 (1.5/100) (2/3) [])
                 ||| noBorders simpleTabbed
                 -- ||| emptyBSP
                 -- ||| Grid

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig :: XPConfig
myXPConfig = def
  { position = Top
  , alwaysHighlight = True
  , promptBorderWidth = 0
  , font = "xft:monospace:size=9"
  }

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created. The list given to
-- @composeOne@ is processed from top to bottom. The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook :: ManageHook
myManageHook = composeOne
  [ className =? "mpv" -?> doFloat
    , title =? "ghci" -?> doFloat
    , title =? "python" -?> doFloat
    , title =? "floatterm" -?> doFloat
    , className =? "Pinentry" -?> doFloat
    , className =? "Pavucontrol"  -?> doFloat
    , className =? "Clementine" -?> doFloat
    -- , className =? "Pcmanfm" -?> doFloat
    , isDialog -?> doCenterFloat

    -- Move transient windows to their parent:
    , transience
  ]
