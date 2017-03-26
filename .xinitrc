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

xbindkeys
feh --bg-center ~/Downloads/pZRkMzP.jpg &
pulseaudio --start &
exec xmonad
