import XMonad
import XMonad.Actions.Submap
import XMonad.Actions.WindowBringer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Tabbed
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.StackSet as W
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.NamedScratchpad
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Exit (exitWith, ExitCode(..))
import System.IO

main = do
  xdzenproc <- spawnPipe myStatusBar
  xmonad $ desktopConfig { terminal = myTerminal
                         , modMask = myMask
                         , borderWidth = myBorderWidth
                         , layoutHook = desktopLayoutModifiers $ myLayoutHook
                         , manageHook = myManageHook <+> manageHook defaultConfig
                         , logHook = dynamicLogWithPP $ myDzenPP xdzenproc
                         } `additionalKeys` myKeys

myTerminal = "urxvtc"
myMask = mod4Mask
myBorderWidth = 2
myTabFont = "xft:Ubuntu Mono:style=Bold:size=10"
myFont = "xft:Ubuntu Mono:style=Bold:size=10"
myStatusBar = "dzen2 -x 0 -y 0 -ta l -w 1920 -h 24 -dock -fn '" ++ myFont ++ "'"

myLayoutHook = tiled ||| tabbedLayout ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    delta = 2 / 100
    ratio = 1 / 2
    tabbedLayout = tabbed shrinkText myTabConfig

myTabConfig = def { fontName = myTabFont
                  , decoHeight = 24
                  }

myScratchpads = [ NS "htop" "urxvtc -e htop" (title =? "htop") (customFloating $ W.RationalRect (1/4) (1/8) (1/2) (3/4))
                , NS "calc" "urxvtc -e bc -l" (title =? "bc") (customFloating $ W.RationalRect 0 (3/4) (1/4) (1/4))
                --, NS "spotify" "spotify" (className =? "Spotify") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
                , NS "urxvt" "urxvtc -name scratch_urxvt" (resource =? "scratch_urxvt") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
                ]
  
myManageHook = composeAll
    [ className =? "Spotify" --> doFloat
    , manageDocks
    , namedScratchpadManageHook myScratchpads
    ]

myDzenPP h = dzenPP { ppCurrent = dzenColor "white" "red" . wrap "[" "]"
                    , ppHidden = dzenColor "gray" "" . wrap "-" "-"
                    , ppUrgent = dzenColor "white" "orange" . wrap ">" "<"
                    , ppSep = " | "
                    , ppOutput = hPutStrLn h
                    , ppTitle = dzenColor "white" "" . shorten 50
                    , ppLayout = dzenColor "gray" "" . myLayoutPrinter
                    }

myLayoutPrinter :: String -> String
myLayoutPrinter "Tall" = "^i(/home/jay/.xmonad/tall-l.xbm)"
myLayoutPrinter "Tabbed Simplest" = "^i(/home/jay/.xmonad/tabbed-l.xbm)"
myLayoutPrinter "Full" = "^i(/home/jay/.xmonad/full-l.xbm)"
myLayoutPrinter x = x

myKeys = [ ((myMask .|. controlMask, xK_Return), spawn "urxvtc -e tmux")
         , ((myMask, xK_p), spawn "rofi -show run")
         , ((myMask, xK_f), spawn "nautilus --no-desktop")
         , ((myMask .|. shiftMask, xK_q), confirmPrompt myXPConfig "exit" $ io (exitWith ExitSuccess))
         , ((myMask .|. controlMask, xK_g), gotoMenu)
         , ((myMask .|. controlMask, xK_b), bringMenu)
         , ((myMask .|. controlMask, xK_l), spawn "i3lock -e -i /tmp/lockscreen.png")
         , ((myMask, xK_s), submap . M.fromList $
           [ ((0, xK_c), namedScratchpadAction myScratchpads "calc")
           , ((0, xK_h), namedScratchpadAction myScratchpads "htop")
           , ((0, xK_s), namedScratchpadAction myScratchpads "spotify")
           , ((0, xK_t), namedScratchpadAction myScratchpads "urxvt")
           ])
         , ((0, xF86XK_AudioPlay), spawn "/home/jay/.local/bin/spcli play")
         , ((0, xF86XK_AudioNext), spawn "/home/jay/.local/bin/spcli next")
         , ((0, xF86XK_AudioPrev), spawn "/home/jay/.local/bin/spcli prev")
         , ((0, xF86XK_AudioLowerVolume), spawn "/home/jay/.local/bin/pavol down")
         , ((0, xF86XK_AudioRaiseVolume), spawn "/home/jay/.local/bin/pavol up")
         , ((0, xF86XK_AudioMute), spawn "/home/jay/.local/bin/pavol mute")
         , ((0, xF86XK_HomePage), spawn "/home/jay/.local/bin/xhtop")
        ]

myXPConfig :: XPConfig
myXPConfig = def { font = myFont
                 , height = 24
                 , borderColor = "#FF0000"
                 , bgColor = "#C00000"
                 , fgColor = "#FFFFFF"
                 , position = CenteredAt (1/2) (3/16)
                 }