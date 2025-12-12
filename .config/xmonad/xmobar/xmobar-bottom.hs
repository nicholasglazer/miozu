----------------------------------------------------------------------
-- Bottom XMobar configuration for system monitoring
-- Shows CPU temp/load and GPU temp/load similar to MangoHUD
----------------------------------------------------------------------
Config {
  -- appearance
    font             = "xft:IBM Plex Sans:size=10:antialias=true:hinting=true:bold,xft:Inconsolata:size=9:bold:antialias=true"
  , additionalFonts  = ["xft:Inter:pixelsize=12" , "xft:Roboto:pixelsize=12"]
  , bgColor          = "#000000"
  , alpha            = 0        -- 0 is transparent, 255 is opaque
  , fgColor          = "#565E78"
  , position         = BottomSize C 100 24
  , textOffset       = 0
  , border           = NoBorder
  -- layout
  , sepChar          = "%"     -- delineator between plugin names and straight text
  , alignSep         = "}{"    -- separator between left-right alignment
  , template         = " CPU: %cpu% <fc=#83D2FC>%cputemp%</fc> | RAM: <fc=#FF9982>%memory%</fc> } %nowplaying% { GPU: <fc=#E8D176>%gpuutil%</fc> <fc=#83D2FC>%gputemp%</fc> "
  -- general behavior
  , lowerOnStart     = True  -- send to bottom of window stack on start
  , hideOnStart      = False  -- start with window visible
  , allDesktops      = True  -- show on all desktops
  , overrideRedirect = True  -- set this option to True
  , pickBroadest     = True  -- choose widest display (multi-monitor)
  , persistent       = True  -- always visible when shown

  , commands = [ 
      -- Now playing info
      Run Com "/home/ng/.config/xmonad/xmobar/now-playing-bt.sh" [] "nowplaying" 10
      
      -- CPU load with color gradients
    , Run Cpu [ "--template" , "<total>%"
              , "--Low"      , "30"
              , "--High"     , "70"
              , "--ppad"     , "3"
              , "--width"    , "3"
              , "-L"         , "30"
              , "-H"         , "70"
              , "--low"      , "#3ae03a"
              , "--normal"   , "#fd971f"
              , "--high"     , "#ff0000"
              ] 10
              
    -- CPU temperature from sensors (thinkpad)
    , Run Com "sh" ["-c", "sensors thinkpad-isa-0000 2>/dev/null | grep 'CPU:' | awk '{print $2}' | tr -d '+' || echo 'N/A'"] "cputemp" 30
                   
    -- Memory usage
    , Run Memory [ "--template" , "<usedratio>%"
                 , "--Low"      , "30"
                 , "--High"     , "80"
                 , "--low"      , "#3ae03a"
                 , "--normal"   , "#fd971f"
                 , "--high"     , "#ff0000"
                 ] 10
                 
    -- GPU utilization (NVIDIA)
    , Run Com "sh" ["-c", "nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits 2>/dev/null | awk '{print $1\"%\"}' || echo 'N/A'"] "gpuutil" 30
    
    -- GPU temperature (NVIDIA) 
    , Run Com "sh" ["-c", "nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader,nounits 2>/dev/null | awk '{print $1\"°C\"}' || echo 'N/A'"] "gputemp" 30
    ]
  }