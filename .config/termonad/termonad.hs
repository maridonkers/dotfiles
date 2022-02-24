{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fwarn-unused-imports #-}

-- | This is an example Termonad configuration that shows how to use the
-- 'ColourExtension' to set colours for your terminal.  See the project-wide
-- README.md for how to use Termonad Haskell configuration scripts.

module Main where

import           Termonad               (CursorBlinkMode (CursorBlinkModeOff),
                                         FontConfig, FontSize (FontSizePoints),
                                         Option (Set),
                                         ShowScrollbar (ShowScrollbarNever),
                                         ShowTabBar (ShowTabBarNever, ShowTabBarIfNeeded),
                                         TMConfig, confirmExit, cursorBlinkMode,
                                         defaultConfigOptions,
                                         defaultFontConfig, defaultTMConfig,
                                         fontConfig, fontFamily, fontSize,
                                         options, showMenu, showScrollbar,
                                         showTabBar, start)
import           Termonad.Config.Colour (AlphaColour, ColourConfig, List8,
                                         Palette (ExtendedPalette),
                                         addColourExtension, createColour,
                                         createColourExtension, cursorBgColour,
                                         defaultColourConfig,
                                         defaultLightColours, foregroundColour,
                                         mkList8, palette, unsafeMkList8)

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "Noto Sans Mono"
    , fontSize = FontSizePoints 12
    }

-- | This is our main 'TMConfig'.  It holds all of the non-colour settings
-- for Termonad.
--
-- This shows how a few settings can be changed.
-- https://hackage.haskell.org/package/termonad-1.0.0.0/docs/Termonad-Config.html
myTMConfig :: TMConfig
myTMConfig =
  defaultTMConfig
    { options =
        defaultConfigOptions
          { fontConfig = fontConf
          , showScrollbar = ShowScrollbarNever
          -- , showTabBar = ShowTabBarNever
          , showTabBar = ShowTabBarIfNeeded
          , confirmExit = False
          , showMenu = False
          , cursorBlinkMode = CursorBlinkModeOff
          }
    }

-- This is our 'ColourConfig'.  It holds all of our colour-related settings.
myColourConfig :: ColourConfig (AlphaColour Double)
myColourConfig =
  defaultColourConfig
    -- Set the cursor background colour.  This is the normal colour of the
    -- cursor.
    { cursorBgColour = Set (createColour 220 200  20) -- yellow
    -- Set the default foreground colour of text of the terminal.
    -- , foregroundColour = Set (createColour 185 128 0) -- amber
    , foregroundColour = Set (createColour 225 225 225) -- whiteish
    -- Set the extended palette that has 8 colours standard colors and then 8
    -- light colors.
    , palette = ExtendedPalette myStandardColours
                                (maybe defaultLightColours id myLightColours)
    }
  where
    -- This is a an example of creating a linked-list of colours,
    -- This function uses an unsafe method for generating the list.
    -- An exception will be thrown if your list does not have 8 elements.
    myStandardColours :: List8 (AlphaColour Double)
    myStandardColours = unsafeMkList8
      [ createColour  0  0  0    -- black (used as background colour)
      , createColour 180  30  20 -- red
      , createColour  40 160  20 -- green
      , createColour 180 160  20 -- dark yellow
      -- , createColour  40  30 120 -- dark purple
      , createColour  64  204 188 -- turquoiseish for directory in LS_COLORS (and JSaddle output)
      , createColour 180  30 120 -- bright pink
      , createColour  40 160 120 -- teal
      , createColour 180 160 120 -- light brown
      ]

    -- This is an example of creating a linked-list of colours with a type
    -- safe method. mkList8 produces a Maybe value which must be handled explicitely.
    myLightColours :: Maybe (List8 (AlphaColour Double))
    myLightColours = mkList8
        [ createColour  70  60  50 -- brown
        , createColour 220  30  20 -- light red
        , createColour  40 210  20 -- light green
        , createColour 220 200  20 -- yellow
        , createColour  40  30 180 -- purple
        , createColour 140  30 80  -- dark pink
        , createColour  50 200 160 -- light teal
        , createColour 220 200 150 -- light brown
        ]

main :: IO ()
main = do
  -- First, create the colour extension based on 'myColourConfig'.
  myColourExt <- createColourExtension myColourConfig

  -- Update 'myTMConfig' with our colour extension.
  let newTMConfig = addColourExtension myTMConfig myColourExt

  -- Start Termonad with our updated 'TMConfig'.
  start newTMConfig
