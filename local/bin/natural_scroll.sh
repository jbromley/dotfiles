#! /usr/bin/bash
case $(hostname) in
llama)
  xinput set-prop 'Logitech M510' 'libinput Natural Scrolling Enabled' 1
  ;;
yemaya)
  xinput set-prop 'Logitech USB Receiver Mouse' 'libinput Natural Scrolling Enabled' 1
  ;;
*)
  echo "Unknown host: $(hostname)"
  ;;

esac
