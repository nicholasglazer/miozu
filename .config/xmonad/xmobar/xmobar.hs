Config {
  -- appearance
  font               = "xft:IBM Plex Sans:size=10:antialias=true:hinting=true:bold,xft:Inconsolata:size=9:bold:antialias=true"
  , additionalFonts  = ["xft:Inter:pixelsize=12" , "xft:Roboto:pixelsize=12"]
  , bgColor          = "#00000000"
  , alpha            = 0        -- 0 is transparent, 255 is opaque. 255 -> xmobar bgcolor, xmonad.hs xmobarPP bgcolor on
  , fgColor          = "#565E78"
  , position         = TopSize C 100 24
  , textOffset       = 4
  , border           = NoBorder -- TopB, TopBM, BottomB, BottomBM, FullB, FullBM or NoBorder (default). TopBM=TopBorderMargin
   -- layout
  , sepChar          = "%"     -- delineator between plugin names and straight text
  , alignSep         = "}{"    -- separator between left-right alignment
  , template         = " <action=`mpc toggle`>%mpd%</action> } %StdinReader% { <action=`setxkbmap -layout 'dvorak, us, ru' -option 'grp:alt_shift_toggle,caps:escape'`>%kbd%</action> %date% "
   -- general behavior
  , lowerOnStart     = True    -- send to bottom of window stack on start
  , hideOnStart      = False   -- start with window unmapped (hidden)
  , allDesktops      = True    -- show on all desktops
  , overrideRedirect = True    -- in some situations you might need to set this option to False.
  , pickBroadest     = False   -- choose widest display (multi-monitor)
  , persistent       = True    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
  , commands =
        -- music monitor
        [Run MPD            [ "--template", "<fn=1>\xf58f</fn> <fc=#f92672><title></fc> - <fc=#66d9ef><artist></fc> |<flags>| <fc=#A6E22E> <remaining> </fc><statei>"
                             , "--" , "-P", "<fn=1>\xf04b</fn>"
                             , "-Z" ,       "<fn=1>\xf04c</fn>"
                             , "-S" ,       "<fn=1>\xf04d</fn>"
                             ] 10
        -- network activity monitor (dynamic interface resolution)
        --  <action=`urxvt -e iwctl`>%dynnetwork%</action> add this line to the template
        --, Run DynNetwork     [ "--template" , "<fc=#3ae03a><rx></fc> <fc=#ff0000><tx></fc>"
          --                   , "--Low"      , "10000"       -- units: B/s
          --                   , "--High"     , "1000000"       -- units: B/s
          --                   , "--low"      , "#797979"
          --                   , "--normal"   , "#fd971f"
          --                   , "--high"     , "#A6E22E"
          --                   ] 10
     -- uncomment and add this to the template '... %battery% ...' if you are using a laptop
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
        -- time and date indicator
        , Run Date           "%a<fc=#4385E7> %H:%M</fc> " "date" 10
        -- keyboard layout indicator
        , Run Kbd            [ ("us(dvorak)" , "<fc=#c43444> Dv</fc>")
                             , ("ru"         , "<fc=#8683ff> Ru</fc>")
                             , ("us"         , "<fc=#ff9922> Us</fc>")
                             ]
        -- xmonad workspace : title
        , Run StdinReader
        ]
   }
