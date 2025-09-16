#! /usr/bin/env bash
# Terminate currently running polybars
polybar-msg cmd quit

# Launch polybar
echo "---" | tee -a /tmp/polybar.log
polybar main_bar | tee -a /tmp/polybar.log & disown
