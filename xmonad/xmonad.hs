import XMonad
import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Util.Run(spawnPipe)
import System.IO

main = do
  xmproc <- spawnPipe myStatusBar
  xmonad kde4Config { modMask = myMask
		    , borderWidth = myBorderWidth
		    , focusedBorderColor = myFocusedBorderColor
    		    , manageHook = manageHook kdeConfig <+> myManageHook
		    , logHook = dynamicLogWithPP $ myXmobarPP xmproc
    		    }

myMask = mod4Mask
myBorderWidth = 4
myFocusedBorderColor = "#0000FF"
myActiveColor = "#000080"
myUrgentBorderColor = "#00FFFF"
myUrgentColor = "#008080"
myTabFont = "xft:Ubuntu:style=Bold:size=10"
myFont = "xft:Ubuntu:style=Bold:size=10"
myStatusBar = "/home/jay/.cabal/bin/xmobar"

myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat    | c <- myFloatClasses]
    , [ title       =? t --> doFloat    | t <- myFloatTitles]
    ]
  where myFloatClasses = ["plasmashell"
                         ,"Plasma"
                   	 ,"krunner"
                   	 ,"Kmix"
                   	 ,"Klipper"
                   	 ,"Plasmoidviewer"
                   	 ,"yakuake"
			 ,"Processing"
			 ,"processing-app-Base"
			 ,"VirtualBox Manager"
			 ,"VirtualBox Machine"
			 ]
        myFloatTitles = ["plasma-desktop"]

myXmobarPP h = xmobarPP { ppCurrent = xmobarColor "white" "#0000c0" . wrap "[" "]"
                        , ppUrgent = xmobarColor "white" "#33ff00" . wrap ">" "<"
                        , ppSep = " | "
                        , ppOutput = hPutStrLn h
                        , ppTitle = xmobarColor "white" "" . shorten 50
                        , ppLayout = xmobarColor "gray" "" . myLayoutPrinter
                        }

myLayoutPrinter :: String -> String
myLayoutPrinter "Tall" = "<icon=/home/jay/.xmonad/tall.xpm/>"
myLayoutPrinter "TwoPane" = "<icon=/home/jay/.xmonad/twopane.xpm/>"
myLayoutPrinter "combining Full and Tabbed Simplest with TwoPane" = "<icon=/home/jay/.xmonad/combo.xpm/>"
myLayoutPrinter "Tabbed Simplest" = "<icon=/home/jay/.xmonad/tabbed.xpm/>"
myLayoutPrinter "Full" = "<icon=/home/jay/.xmonad/full.xpm/>"
myLayoutPrinter x = "[[" ++ x ++ "]]"
