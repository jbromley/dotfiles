case $(hostname) in 
  llama)
    xinput set-prop 'Logitech M510' 290 1
    ;;
  yemaya)
    xinput set-prop 'Logitech USB Receiver Mouse' 290 1
    ;;
  *)
    ;;
esac
[ -x ~/.fehbg ] && ~/.fehbg
picom &
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --transparent true --alpha 64 --tint 0x000000 &
xsetroot -cursor_name left_ptr
if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi
exec xmonad
