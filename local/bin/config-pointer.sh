#!/bin/bash

function configure_mouse {
    local dev="$1"
    xinput set-prop "${dev}" 'libinput Natural Scrolling Enabled' 1
}

function configure_trackpad {
    local dev="$1"
    xinput set-prop "${dev}" 'libinput Tapping Enabled' 1
    xinput set-prop "${dev}" 'libinput Tapping Drag Enabled' 1
    xinput set-prop "${dev}" 'libinput Natural Scrolling Enabled' 1
    xinput set-prop "${dev}" 'libinput Disable While Typing Enabled' 1
}

# Define the device-function mapping
declare -A device_config
device_config=(
    ["Logitech M510"]="configure_mouse"
    ["Logitech USB Receiver Mouse"]="configure_mouse"
    ["ETPS/2 Elantech Touchpad"]="configure_trackpad"
)

# Store the output of `xinput` once
xinput_devices=$(xinput list --name-only)

# Loop through the devices
for device in "${!device_config[@]}"; do
    if echo "$xinput_devices" | grep -q "$device"; then
        # Call the corresponding function
        ${device_config[$device]} "$device"
    fi
done
