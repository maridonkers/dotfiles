--------------------------------------------------------------------------------
-- | xmonad.hs
--
-- This configuration file for xmonad is adapted from DT's dotfiles.
-- See: https://gitlab.com/dwt1/dotfiles
--
--------------------------------------------------------------------------------
import System.Exit
import System.IO (hPutStrLn)

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap,
                                xmobarPP, xmobarColor,
                                shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook,
                                 manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.NoBorders
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.Search as S
import XMonad.Hooks.ScreenCorners
import XMonad.Util.SpawnOnce
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)

------------------------------------------------------------------------
-- VARIABLES
------------------------------------------------------------------------

myTerminal :: String
myTerminal = "xterm"

mySpacing :: Int
mySpacing = 5

myBorderWidth :: Dimension 
myBorderWidth = 2

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

myEditor :: String
myEditor = "bin/ec"

myFileManager :: String
myFileManager = "pcmanfm"

myRedshiftOn :: String
myRedshiftOn = "redshift"

myScreensaverOn :: String
myScreensaverOn = "xscreensaver -no-splash &"

myRedshiftOff :: String
myRedshiftOff = "redshift ; redshift -x"

runItOnce :: String -> X ()
runItOnce cmd = spawn $ "~/bin/runonce " ++ cmd

killItOnce :: String -> X ()
killItOnce cmd = spawn $ "~/bin/killonce " ++ cmd

------------------------------------------------------------------------
-- SEARCH ENGINES
------------------------------------------------------------------------
-- Xmonad has several search engines available to use located in
-- XMonad.Actions.Search. Additionally, you can add other search engines
-- such as those listed below.
-- archwiki, ebay,
news, reddit, urban :: S.SearchEngine

-- archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
-- ebay     = S.searchEngine "ebay" "https://www.ebay.com/sch/i.html?_nkw="
news     = S.searchEngine "news" "https://news.google.com/search?q="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
urban    = S.searchEngine "urban" "https://www.urbandictionary.com/define.php?term="

searchList :: [(String, S.SearchEngine)]
searchList = [ --("a", archwiki)
             ("d", S.duckduckgo)
             --, ("e", ebay)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("n", news)
             , ("r", reddit)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("b", S.wayback)
             , ("u", urban)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]

------------------------------------------------------------------------
-- KEY BINDINGS
------------------------------------------------------------------------
-- Add some extra key bindings; M1 is Alt key.
myKeys =
      [("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))
      , ("M-p", shellPrompt myXPConfig)
      , ("M-<Esc>", sendMessage (Toggle "Full") >> sendMessage ToggleStruts)
      , ("M-f", sendMessage (Toggle "Full"))
      , ("M-<Backspace>", kill)
      , ("M-b", withFocused toggleBorder)
      , ("M-e", spawn myEditor)
      , ("M-S-<Left>", sendMessage Shrink)
      , ("M-S-<Right>", sendMessage Expand)
      , ("M-<Left>", windows W.focusDown)
      , ("M-<Right>", windows W.focusUp)
      , ("M-C-<Down>", windows W.swapDown >> windows W.focusUp)
      , ("M-C-<Up>", windows W.swapUp >> windows W.focusDown)
      , ("M-M1-<Up>", rotSlavesDown)
      , ("M-M1-<Down>", rotAllDown)
      , ("M-<Tab>", nextWS)
      , ("M-S-<Tab>", prevWS)
      , ("M-C-<Tab>", shiftToNext >> nextWS)
      , ("M-C-S-<Tab>", shiftToPrev >> prevWS)
      , ("M-M1-<Left>", prevWS)
      , ("M-M1-<Right>", nextWS)
      , ("M-C-M1-<Left>", shiftToPrev >> prevWS)
      , ("M-C-M1-<Right>", shiftToNext >> nextWS)
      , ("M-S-<Space>", sendMessage NextLayout)
      , ("M-<Space>", goToSelected defaultGSConfig)
      -- , ("M-C-u", sendMessage Arrange)
      -- , ("M-C-d", sendMessage DeArrange)
      , ("M-m", spawnOnce myFileManager)
      , ("M-r", runItOnce myRedshiftOn)
      , ("M-S-r", killItOnce myRedshiftOff)
      , ("M-S-0", spawn "xscreensaver-command -lock")
      , ("M-C-0", spawn "xscreensaver-command -lock & systemctl suspend")
      , ("M-C-S-0", spawn "systemctl hibernate")
      , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
      , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
      , ("<XF86AudioMute>", spawn "amixer set Master toggle")
      , ("<XF86AudioPlay>", spawn "clementine -t")
      , ("<XF86AudioPrev>", spawn "clementine -r")
      , ("<XF86AudioNext>", spawn "clementine -f")
      ]
      -- Appending search engine prompts to keybindings list.
      -- Look at "search engines" section of this config for values for "k".
      ++ [("M-s " ++ k, S.promptSearch npXPConfig f) | (k,f) <- searchList ]
      ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]

myStartupHook :: X ()
myStartupHook = do
  addScreenCorners [(SCUpperLeft, goToSelected defaultGSConfig)]
  spawnOnce "xsetroot -solid black"
  runItOnce myRedshiftOn
  runItOnce myScreensaverOn
  spawnOnce "emacs --daemon &"

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

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
               $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ "<fn=2>" ++ ws ++ "</fn>" ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--------------------------------------------------------------------------------
main = do
  -- Launching instances of xmobar on their monitors.
  xmproc0 <- spawnPipe "xmobar -x 0 /home/mdo/.config/xmobar/xmobarrc0.hs"
  -- xmproc0 <- spawnPipe "xmobar -x 0 /home/dt/.config/xmobar/xmobarrc0.hs"
  -- xmproc1 <- spawnPipe "xmobar -x 1 /home/dt/.config/xmobar/xmobarrc1.hs"

  xmonad $ defaults xmproc0

    `additionalKeysP` myKeys

--------------------------------------------------------------------------------
-- | Customized defaults.
-- Main desktop configuration with some overrides.
defaults xmproc0 = desktopConfig {
    terminal = myTerminal,
    borderWidth = myBorderWidth,
    focusedBorderColor = myFocusedBorderColor,
    modMask = mod4Mask, -- Use the "Win" key for the mod key
    workspaces = myWorkspaces,
    manageHook = myManageHook <+> manageHook desktopConfig <+> manageDocks,
    handleEventHook = docksEventHook <+> screenCornerEventHook,
    layoutHook = desktopLayoutModifiers $ screenCornerLayoutHook $ myLayouts,
    logHook = dynamicLogWithPP xmobarPP
        { ppOutput = \x -> hPutStrLn xmproc0 x --  >> hPutStrLn xmproc1 x  >> hPutStrLn xmproc2 x
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
  
--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- Use the 'M-<Esc>' key binding defined above to toggle between the
-- current layout and a full screen layout.
myLayouts = avoidStruts
  $ mouseResize
  $ windowArrange
  $ toggleLayouts (noBorders Full) others
  where
    others = spacing mySpacing $
               ResizableTall 1 (1.5/100) (3/5) []
                 ||| Full
                 ||| emptyBSP
                 ||| Grid

--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig = def
  { position = Top
  , alwaysHighlight = True
  , promptBorderWidth = 0
  , font = "xft:monospace:size=9"
  }

  -- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
npXPConfig :: XPConfig
npXPConfig = myXPConfig
  { autoComplete = Nothing
  }


--------------------------------------------------------------------------------
-- | Manipulate windows as they are created. The list given to
-- @composeOne@ is processed from top to bottom. The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook = composeOne
  [ className =? "mpv" -?> doFloat
    , isDialog -?> doCenterFloat

    -- Move transient windows to their parent:
    , transience
  ]
