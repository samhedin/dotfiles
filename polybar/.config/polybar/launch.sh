#!/usr/bin/env bash

# Terminate already running bar instances
pkill polybar
# If all your bars have ipc enabled, you can also use
# polybar-msg cmd quit

# Launch bar1 and bar2
echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
MONITOR=DP-2 polybar example >>/tmp/polybar1.log 2>&1 & disown
MONITOR=HDMI-1 polybar example >>/tmp/polybar2.log 2>&1 & disown

echo "Bars launched..."
