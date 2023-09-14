-- Path .config/xmonad/xmobar/xmobar.hs
----------------------------------------------------------------------
-- Last modified 1 June 2023
-- Author Nicholas Glazer <glazer.nicholas@gmail.com>
-- Repo https://github.com/nicholasglazer/miozu
----------------------------------------------------------------------
-- The --template option controls how the plugin is displayed. Text
-- color can be set by enclosing in <fc></fc> tags. For more details
-- see http://projects.haskell.org/xmobar/#system-monitor-plugins.
--
-- Some commands are commented because they have no use now,
-- but might be used in the future.
-- Colors are the part of the https://github.com/miozutheme
----------------------------------------------------------------------
Config {
  -- appearance
    font             = "xft:IBM Plex Sans:size=10:antialias=true:hinting=true:bold,xft:Inconsolata:size=9:bold:antialias=trueI"
  , additionalFonts  = ["xft:Inter:pixelsize=12" , "xft:Roboto:pixelsize=12"]
  , bgColor          = "#000000"
  , alpha            = 0        -- 0 is transparent, 255 is opaque. 255 -> xmobar bgcolor, xmonad.hs xmobarPP bgcolor on
  , fgColor          = "#565E78"
  , position         = TopSize C 100 24
  , textOffset       = 0
  , border           = NoBorder -- TopB, TopBM, BottomB, BottomBM, FullB, FullBM or NoBorder (default). TopBM=TopBorderMargin
  -- layout
  , sepChar          = "%"     -- delineator between plugin names and straight text
  , alignSep         = "}{"    -- separator between left-right alignment
  , template         = " %nowplaying% } %XMonadLog% { <action=`setxkbmap -layout 'us,ua' -variant 'dvorak,' -option 'grp:alt_shift_toggle,caps:escape'`>%kbd%</action> %date% "
  -- general behavior
  , lowerOnStart     = True  -- send to bottom of window stack on start
  , hideOnStart      = False -- start with window unmapped (hidden)
  , allDesktops      = False -- show on all desktops
  , overrideRedirect = True  -- set this option to True
  , pickBroadest     = True  -- choose widest display (multi-monitor)
  , persistent       = False -- enable/disable hiding (True = disabled)

  , commands = [ Run Com "/home/n/.config/xmonad/xmobar/now-playing-bt.sh" [] "nowplaying" 10  -- Display current playing track using playctl
               , Run Date "%a<fc=#83D2FC> %H:%M</fc> " "date" 10                               -- time and date indicator
               , Run Kbd [ ("us(dvorak)" , "<fc=#FF9982> Dv</fc>")
                         , ("ua"         , "<fc=#FF9837> Ua</fc>")
                         , ("us"         , "<fc=#E8D176> Us</fc>") ]                            -- keyboard layout indicator
               , Run XMonadLog                                                                  -- Xmonad output
        ]
   }


-----------------------------------------
-- For those who need to monitor network activity (dynamic interface resolution)
 --  <action=`urxvt -e iwctl`>%dynnetwork%</action> add this line to the template
 --, Run DynNetwork     [ "--template" , "<fc=#3ae03a><rx></fc> <fc=#ff0000><tx></fc>"
   --                   , "--Low"      , "10000"       -- units: B/s
   --                   , "--High"     , "1000000"       -- units: B/s
   --                   , "--low"      , "#797979"
   --                   , "--normal"   , "#fd971f"
   --                   , "--high"     , "#A6E22E"
   --                   ] 10
-----------------------------------------
-- If you are using a battery e.g. laptop
-- uncomment and add this to the array, also modify template stiing with '... %battery% ...'
-- battery monitor
  --, Run Battery
  --, "-L", "20"
  --, "-H", "85"
  --, "-l", "#ff0000"
  -- , "-n", "#fd971f"
  --  , "-h", "#3ae03a"
  --   , "--" -- battery specific options
  --     -- discharging status
  --   , "-o"  , " <left>% (<timeleft>) <watts>W"
  --     -- AC "on" status
  --   , "-O"  , "<left>%<fc=#3ae03a><watts>W</fc>"
  --     -- charged status
  --   , "-i"  , "<fc=#3ae03a><watts></fc>"
  -- , "--off-icon-pattern", "<fn=1>\xf1e6</fn>"
  --   , "--on-icon-pattern", "<fn=1>\xf1e6</fn>"
  --   , "-H"   , "16"
  --   , "-L"   , "10"
  --   , "-p"	, "#3ae03a" -- positive power (battery charging)
  --   , "-h"	, "#e03a3a" -- power higher than the -H threshold
  --   , "-m"	, "#fd971f" -- power lower than the -H threshold
  --   , "-l"	, "#a6e22e" -- power lower than the -L threshold
  --   ] 10
-----------------------------------------
-- Not using mpd anymore, but i'll leave it here just in case
-- Run MPD            [ "--template", "<fn=1>\xf58f</fn> <fc=#f92672><title></fc> - <fc=#66d9ef><artist></fc> |<flags>| <fc=#A6E22E> <remaining> </fc><statei>"
--                    , "--" , "-P", "<fn=1>\xf04b</fn>"
--                    , "-Z" ,       "<fn=1>\xf04c</fn>"
--                    , "-S" ,       "<fn=1>\xf04d</fn>"
--                    ] 10
