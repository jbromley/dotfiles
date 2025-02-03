import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Combo
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Operations
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig

myConfig =
  def
    { modMask = mod4Mask,
      layoutHook = myLayout,
      manageHook = myManageHook,
      terminal = "kitty",
      startupHook = myStartupHook
    }
    `additionalKeysP` [ ("M-d", spawn "dmenu_run -fn 'Lilex:style=Bold:size=10'"),
                        ("M-S-z", spawn "i3lock -i ~/.cache/lockscreen.png"),
                        ("M-C-s", unGrab *> spawn "scrot -s"),
                        ("M-f", spawn "firefox"),
                        ("M-p", spawn "rofi -show drun"),
                        ("M-o", spawn "rofi -show run"),
                        ("M-i", spawn "rofi -show window"),
                        ("<XF86AudioMute>", spawn "pavol toggle"),
                        ("<XF86AudioLowerVolume>", spawn "pavol down"),
                        ("<XF86AudioRaiseVolume>", spawn "pavol up"),
                        ("M-<Right>", sendMessage $ Move R),
                        ("M-<Left>", sendMessage $ Move L),
                        ("M-<Up>", sendMessage $ Move U),
                        ("M-<Down>", sendMessage $ Move D)
                      ]

myLayout = tiled ||| windowNavigation tabbedTabbed ||| fullTabbed ||| Full ||| threeCol
  where
    tiled = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta ratio
    fullTabbed = renamed [Replace "Tabbed Full"] $ tabbed shrinkText tabConfig
    tabbedTabbed = renamed [Replace "Tabbed Panes"] $ combineTwo (TwoPane (3 / 100) (1 / 2)) (tabbed shrinkText tabConfig) (tabbed shrinkText tabConfig)
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100
    tabConfig =
      def
        { fontName = "xft:Lilex:style=Bold:size=9",
          activeColor = "#400000",
          activeBorderColor = "#ff0000",
          inactiveColor = "#200000",
          inactiveBorderColor = "#400000",
          urgentBorderColor = "#ff5b00"
        }

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- \| Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "~/.fehbg"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "picom"
  spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --transparent true --alpha 64 --tint 0x000000"
  spawnOnce "volumeicon"
  spawnOnce "nm-applet --sm-disable"
  spawnOnce "~/.local/bin/natural_scroll.sh"

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      isDialog --> doFloat
    ]
