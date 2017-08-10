#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias steam='steam-native'
alias ls='ls --color=auto'
alias bl='xbacklight -set'
export PAGER='vimpager'
alias less=$PAGER

PS1='[\u@\h \W]\$ '
eval $(thefuck --alias)
export PATH=$PATH:/home/autopilot/.local/bin
