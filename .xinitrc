#!/bin/bash

# Load resources

xrdb -merge .Xresources

# Set up an icon tray
#stalonetray &

# Fire up apps

eval `dbus-launch --sh-syntax`

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

if [ -x /usr/bin/nm-applet ] ; then
	   nm-applet --sm-disable &
   fi

amixer set Headphone unmute
amixer set Headphone 100%`
xbindkeys
feh --bg-scale ~/Downloads/aZcB3op.jpg &
start-pulseaudio-x11
exec xmonad
