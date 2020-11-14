-- http://projects.haskell.org/xmobar/

Config { font = "xft:Mononoki Nerd Font:pixelsize=12:antialias=true:hinting=true"
       , additionalFonts = [ "xft:FontAwesome:pixelsize=13", "xft:Ubuntu:weight=bold:pixelsize=11:antialias=true:hinting=true" ]
       , bgColor = "#292d3e"
       , fgColor = "#FFB86C"
       --TODO via -p parameter to xmobar.
       -- , position = Static { xpos = 0 , ypos = 744, width = 1366, height = 24 }
       -- , position = Static { xpos = 1920 , ypos = 744, width = 1366, height = 24 }
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       -- RefreshRate is in 1/10th of seconds.
       , commands = [ 
                      -- Time and date
                      Run Date "%a, %b %d %Y (%H:%M)" "date" 300
                      -- Network up and down
                    , Run Network "wlp3s0" ["-t", "<rx>kb <tx>kb"] 10
                      -- Cpu usage in percent
                    , Run MultiCpu ["-t", "<total0>% <total1>% <total2>% <total3>%","-H","50","--high","red"] 10
                      -- Ram used number and percent
                    , Run Memory ["-t", "<used>M (<usedratio>%)"] 10
                      -- Disk space free
                    , Run DiskU [("/", "/: <free>"),
                                 -- ("/boot", "boot: <free>"),
                                 ("/home", "home: <free>")
                                ] [] 600
                    -- , Run DiskIO [("sda", "sda:<read> <write>"), ("sdb", "sdb:<read> <write>")] [] 10
                      -- Runs a standard shell command 'uname -r' to get kernel version
                    -- , Run Com "uname" ["-r"] "" 36000
                      -- Prints out the left side items such as workspaces, layout, etc.
                      -- The workspaces are 'clickable' in my configs.
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <fc=#666666>|</fc> %UnsafeStdinReader% }{ <fc=#666666>| </fc><fc=#FFB86C>%multicpu% </fc><fc=#666666>| </fc><fc=#FF5555>%memory% </fc><fc=#666666>| </fc><fc=#c3e88d>%wlp3s0% </fc><fc=#666666>| </fc><fc=#82AAFF>%disku%  </fc><fc=#666666>| </fc>><fc=#8BE9FD>%date%</fc> "
       }
