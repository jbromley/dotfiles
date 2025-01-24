import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.ThreeColumns
import XMonad.Operations
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

main :: IO ()
main = xmonad 
     . ewmhFullscreen 
     . ewmh 
     . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig 

myConfig = def 
    { modMask = mod4Mask
    , layoutHook = myLayout
    , manageHook = myManageHook
    , terminal = "kitty"
    -- , startupHook = myStartupHook
    }
    `additionalKeysP`
    [ ("M-d", spawn "dmenu_run -fn 'Lilex:style=Bold:size=10'")
    , ("M-S-z", spawn "i3lock-fancy -p")
    , ("M-C-s", unGrab *> spawn "scrot -s")
    , ("M-f", spawn "firefox")
    , ("M-S-Return", spawn "kitty")]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
  where
    tiled = Tall nmaster delta ratio 
    threeCol = ThreeColMid nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100
    
myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

  -- myStartupHook :: X ()
  -- myStartupHook = do 
  --   spawnOnce "~/.fehbg"
  --   spawnOnce "xinput set-prop 'Logitech M510' 290 1"
  --   spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x5f5f5f --height 18"
  --   spawnOnce "xsetroot -cursor_name left_ptr"
  --   spawnOnce "nm-applet --sm-disable"

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]
