#! /usr/bin/env bash
polybar-msg cmd quit

MONITOR=$(polybar -m | tail -1 | sed -e 's/:.*$//g')
export MONITOR

echo "---" >> /tmp/polybar-main.log
polybar main >> /tmp/polybar-main.log 2>&1
