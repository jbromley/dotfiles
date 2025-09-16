#! /usr/bin/env bash
for ptr in 'Logitech M510' 'Logitech USB Receiver Mouse'; do
    if xinput | grep -q "${ptr}"; then
        xinput set-prop "${ptr}" "libinput Natural Scrolling Enabled" 1
    fi
done
