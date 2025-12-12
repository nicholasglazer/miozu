----------------------------------------------------------------------
-- Top XMobar configuration with centered workspaces
----------------------------------------------------------------------
Config {
  -- appearance
    font             = "xft:IBM Plex Sans:size=10:antialias=true:hinting=true:bold,xft:Inconsolata:size=9:bold:antialias=true"
  , additionalFonts  = ["xft:Inter:pixelsize=12" , "xft:Roboto:pixelsize=12"]
  , bgColor          = "#000000"
  , alpha            = 0        -- 0 is transparent, 255 is opaque
  , fgColor          = "#565E78"
  , position         = TopSize C 100 24
  , textOffset       = 0
  , border           = NoBorder
  -- layout
  , sepChar          = "%"     -- delineator between plugin names and straight text
  , alignSep         = "}{"    -- separator between left-right alignment
  , template         = " } %UnsafeXMonadLog% { %battery% <action=`setxkbmap -layout 'us,ua' -variant 'dvorak,' -option 'grp:alt_shift_toggle,caps:escape'`>%kbd%</action> %date% "
  -- general behavior
  , lowerOnStart     = True  -- send to bottom of window stack on start
  , hideOnStart      = False -- start with window unmapped (hidden)
  , allDesktops      = False -- show on all desktops
  , overrideRedirect = True  -- set this option to True
  , pickBroadest     = True  -- choose widest display (multi-monitor)
  , persistent       = False -- enable/disable hiding (True = disabled)

  , commands = [ 
      Run UnsafeXMonadLog  -- For workspace display
    , Run UnsafeStdinReader  -- For window titles
    , Run Date "%a<fc=#83D2FC> %H:%M</fc> " "date" 10
    , Run Kbd [ ("us(dvorak)" , "<fc=#FF9982> Dv</fc>")
              , ("ua"         , "<fc=#FF9837> Ua</fc>")
              , ("us"         , "<fc=#E8D176> Us</fc>") ]
    , Run Battery [ "--template" , "<acstatus>"
                  , "--Low"      , "20"        -- units: %
                  , "--High"     , "85"        -- units: %
                  , "--low"      , "#ff0000"   -- red for low battery
                  , "--normal"   , "#fd971f"   -- orange for normal
                  , "--high"     , "#3ae03a"   -- green for high battery
                  , "--"         -- battery specific options
                    -- discharging status
                  , "-o"  , "<left>% (<timeleft>) <watts>W"
                    -- AC "on" status (charging)
                  , "-O"  , "<left>%<fc=#3ae03a><watts>W</fc>"
                    -- charged status
                  , "-i"  , "<fc=#3ae03a><watts></fc>"
                  ] 50
    ]
  }