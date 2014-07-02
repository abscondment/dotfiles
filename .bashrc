# /etc/bashrc

# System wide functions and aliases
# Environment stuff goes in /etc/profile

# by default, we want this to get set.
# Even for non-interactive, non-login shells.
if [ $UID -gt 99 ] && [ "`id -gn`" = "`id -un`" ]; then
    umask 002
else
    umask 022
fi

# are we an interactive shell?
if [ "$PS1" ]; then
    case $TERM in
        xterm*)
            if [ -e /etc/sysconfig/bash-prompt-xterm ]; then
                PROMPT_COMMAND=/etc/sysconfig/bash-prompt-xterm
            else
                PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
            fi
            ;;
        screen)
            if [ -e /etc/sysconfig/bash-prompt-screen ]; then
                PROMPT_COMMAND=/etc/sysconfig/bash-prompt-screen
            else
                PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\033\\"'
            fi
            ;;
        *)
            [ -e /etc/sysconfig/bash-prompt-default ] && PROMPT_COMMAND=/etc/sysconfig/bash-prompt-default
            ;;
    esac
    # Turn on checkwinsize
    shopt -s checkwinsize
    [ "$PS1" = "\\s-\\v\\\$ " ] && PS1="[\u@\h \W]\\$ "
fi

if ! shopt -q login_shell ; then # We're not a login shell
    for i in /etc/profile.d/*.sh; do
        if [ -r "$i" ]; then
            . $i
        fi
    done
    unset i
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Screen settings
alias screen="screen -e ^Ll"

# Term term term
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

case "$TERM" in
    xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD/$HOME/~}\007"'
        ;;
    *)
        ;;
esac


# Oh my, colors!
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

alias ls="ls -lG"
# alias xcodebuild="xcodebuild -activetarget -activeconfiguration -sdk iphonesimulator4.0"

export EDITOR=emacs
export JAVA_OPTS=-Xmx1536m

# TODO: figure out if this file is being read in OSX or Linux,
#       and choose dircolors or LSCOLORS based on that.
#

# Linux:
#eval `adircolors -b`

# OSX:
# a     black
# b     red
# c     green
# d     brown
# e     blue
# f     magenta
# g     cyan
# h     light grey
# A     bold black, usually shows up as dark grey
# B     bold red
# C     bold green
# D     bold brown, usually shows up as yellow
# E     bold blue
# F     bold magenta
# G     bold cyan
# H     bold light grey; looks like bright white
# x     default foreground or background

# The order of the attributes are as follows:

# 1.   directory
dir_c=Ex
# 2.   symbolic link
sym_c=Gx
# 3.   socket
socket_c=dx
# 4.   pipe
pipe_c=bx
# 5.   executable
x_c=Cx
# 6.   block special
bspec_c=Dx
# 7.   character special
cspec_c=Dx
# 8.   executable with setuid bit set
x_setuid_c=hb
# 9.   executable with setgid bit set
x_setgid_c=ga
# 10.  directory writable to others, with sticky bit
dir_w_sticky_c=ac
# 11.  directory writable to others, without sticky bit
dir_w_c=ac

export LSCOLORS="$dir_c$sym_c$socket_c$pipe_c$x_c$bspec_c$cspec_c$x_setuid_c$x_setgid_c$dir_w_sticky_c$dir_w_c"

# ssh-agent

# start agent and set environment variables, if needed
agent_started=0
if ! env | grep -q SSH_AGENT_PID >/dev/null; then
    echo "Starting ssh agent"
    eval $(ssh-agent -s)
    ssh-add ~/.ssh/id_madronalabs
    agent_started=1
fi

# GIT SECRETS
source "$HOME/.secrets/git.bashrc"

# AWS SECRETS
source "$HOME/.secrets/ec2/aws.bashrc"
source "$HOME/.secrets/ec2/heroku.bashrc"

export EC2_PRIVATE_KEY="$(/bin/ls $HOME/.secrets/ec2/readabl/pk-*.pem)"
export EC2_CERT="$(/bin/ls $HOME/.secrets/ec2/readabl/cert-*.pem)"
export EC2_HOME="$(find /usr/local/Cellar/ec2-api-tools -type d -depth 1 | sort | tail -1)/libexec"
export EC2_REGION="us-west-1"

# ami
export EC2_AMITOOL_HOME="/usr/local/Cellar/ec2-ami-tools/1.3-45758/jars"

# rds
export AWS_RDS_HOME="/usr/local/Cellar/rds-command-line-tools/1.3.003/jars"
# cloud-watch
export AWS_CLOUDWATCH_HOME="/usr/local/Cellar/cloud-watch/1.0.20.0/libexec"
export SERVICE_HOME="$AWS_CLOUDWATCH_HOME"

#export ANDROID_HOME="/usr/local/Cellar/android-sdk/r18"
export ANDROID_HOME="/usr/local/opt/android-sdk"
export PATH=$ANDROID_HOME/bin:$ANDROID_HOME/platform-tools:/usr/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/Users/brendan/bin

export JAVA6_HOME="/System/Library/Frameworks/JavaVM.framework/Home"
export JAVA7_HOME="$(/usr/libexec/java_home)"
export JAVA_HOME=$JAVA7_HOME

# text stuff
export TT_HOME="$HOME/code/tamingtext-book"
export MAHOUT_HOME="$HOME/code/mahout"

# breaks mirah
# export JRUBY_OPTS="--1.9"

export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"

export PATH="$HOME/.rbenv/bin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"
eval "$(rbenv init -)"
