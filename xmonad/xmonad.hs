import XMonad
import XMonad.Actions.Submap
import XMonad.Actions.WindowBringer
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.Combo
import XMonad.Layout.LayoutBuilder
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
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
  xmproc <- spawnPipe myStatusBar
  xmonad $ desktopConfig { terminal = myTerminal
                         , modMask = myMask
                         , borderWidth = myBorderWidth
                         , focusedBorderColor = myFocusedBorderColor
                         , startupHook = setWMName "LG3D"
                         , layoutHook = desktopLayoutModifiers $ myLayoutHook
                         , manageHook = myManageHook <+> manageHook defaultConfig
                         , logHook = dynamicLogWithPP $ myXmobarPP xmproc
                         } `additionalKeys` myKeys

myTerminal = "st"
myMask = mod4Mask
myBorderWidth = 2
myFocusedBorderColor = "#0000FF"
myActiveColor = "#000080"
myUrgentBorderColor = "#00FFFF"
myUrgentColor = "#008080"
myTabFont = "xft:Ubuntu:style=Bold:size=10"
myFont = "xft:Ubuntu:style=Bold:size=10"
myStatusBar = "/home/jay/.cabal/bin/xmobar"

myLayoutHook = tiled ||| tallAndTabbed ||| tabbedLayout ||| Full
  where
    tiled = Tall nmaster delta ratio
    twoPane = TwoPane delta ratio
    tabbedLayout = tabbed shrinkText myTabConfig
    -- tallAndTabbed = windowNavigation(combineTwo (twoPane) (tabbedLayout) (Full))
    -- tallAndTabbed = Tall *||* tabbedLayout 
    tallAndTabbed = windowNavigation(layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) Full
                    $ layoutAll (relBox 0.5 0 1 1) tabbedLayout)
    nmaster = 1
    delta = 2 / 100
    ratio = 1 / 2
 
myTabConfig = def { activeBorderColor = myFocusedBorderColor
                  , activeColor = myActiveColor
                  , urgentBorderColor = myUrgentBorderColor
                  , urgentColor = myUrgentColor
                  , fontName = myTabFont
                  , decoHeight = 24 
                  }

myScratchpads = [ NS "htop" "st -e htop" (title =? "htop") (customFloating $ W.RationalRect (1/4) (1/8) (1/2) (3/4))
                , NS "calc" "st -e bc -l" (title =? "bc") (customFloating $ W.RationalRect 0 (3/4) (1/4) (1/4))
                , NS "st" "st -n scratch_term" (resource =? "scratch_term") (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
                , NS "xclock" "xclock" (className =? "XClock") (customFloating $ RationalRect (7/16) (2/5) (1/8) (1/5))
                ]

myManageHook = composeAll
    [ className =? "" --> doFloat
    , className =? "Processing" --> doFloat
    , className =? "processing-app-Base" --> doFloat
    , className =? "VirtualBox Manager" --> doFloat
    , className =? "VirtualBox Machine" --> doFloat
    , manageDocks
    , namedScratchpadManageHook myScratchpads
    ]

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
myLayoutPrinter "layoutN Full layoutAll Tabbed Simplest" = "<icon=/home/jay/.xmonad/combo.xpm/>"
myLayoutPrinter "combining Tabbed Simplest and Full with TwoPane" = "<icon=/home/jay/.xmonad/combo.xpm/>"
myLayoutPrinter "Tabbed Simplest" = "<icon=/home/jay/.xmonad/tabbed.xpm/>"
myLayoutPrinter "Full" = "<icon=/home/jay/.xmonad/full.xpm/>"
myLayoutPrinter x = "[[" ++ x ++ "]]"

myKeys = [ ((myMask .|. controlMask, xK_Return), spawn "st")
         , ((myMask, xK_p), spawn "rofi -show drun")
         , ((myMask .|. controlMask, xK_p), spawn "rofi -show run")
         , ((myMask, xK_f), spawn "dolphin")
         , ((myMask .|. shiftMask, xK_q), confirmPrompt myXPConfig "exit" $ io (exitWith ExitSuccess))
         , ((myMask .|. controlMask, xK_g), gotoMenu)
         , ((myMask .|. controlMask, xK_b), bringMenu)
         , ((myMask .|. mod1Mask, xK_l), spawn "i3lock -e -f -i /home/jay/.cache/lockscreen.png")
         , ((myMask .|. shiftMask, xK_h), sendMessage $ Swap L)
         , ((myMask .|. shiftMask, xK_l), sendMessage $ Swap R)
         , ((myMask .|. controlMask, xK_h), sendMessage $ Move L)
         , ((myMask .|. controlMask, xK_l), sendMessage $ Move R)
         -- Scratch pad submap
         , ((myMask, xK_s), submap . M.fromList $
           [ ((0, xK_k), namedScratchpadAction myScratchpads "xclock")
           , ((0, xK_s), namedScratchpadAction myScratchpads "spotify")
           ])
         , ((0, xK_F10), namedScratchpadAction myScratchpads "calc")
         , ((0, xK_F11), namedScratchpadAction myScratchpads "htop")
         , ((0, xK_F12), namedScratchpadAction myScratchpads "st")
         -- Multimedia and other special keys
         , ((0, xF86XK_AudioPlay), spawn "/home/jay/.local/bin/sp play")
         , ((0, xF86XK_AudioNext), spawn "/home/jay/.local/bin/sp next")
         , ((0, xF86XK_AudioPrev), spawn "/home/jay/.local/bin/sp prev")
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
