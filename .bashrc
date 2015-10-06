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

if [ -f "$HOME/.bashrc.fns" ]; then
    . "$HOME/.bashrc.fns"
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
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] 👻  \$ '
if [[ $PLATFORM == 'osx' ]];
then
    # if [ "$PS1" ]; then
    #     # only echo when interactive
    #     echo "running CURL_CA_BUNDLE hack :("
    # fi
    export CURL_CA_BUNDLE=/usr/local/etc/openssl/cert.pem
    alias ls="ls -lG"
else
    alias ls="ls -lG --color=auto"
fi


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

# alias xcodebuild="xcodebuild -activetarget -activeconfiguration -sdk iphonesimulator4.0"

export EDITOR=emacs
#export JAVA_OPTS=-Xmx1536m

# ssh-agent
# start agent and set environment variables, if needed
test -r ~/.agent && . ~/.agent > /dev/null 2>&1
ssh-add -l > /dev/null 2>&1
test ${?} = 2 && ssh-agent -s > ~/.agent 2>/dev/null
##
## Add keys to forward:
if [ -d ~/.ssh/id_madronalabs ];
then
    ssh-add ~/.ssh/id_madronalabs > /dev/null 2>&1
fi
ls -x1 ~/.ssh/id_* | grep -v '.pub$' | while read k
do
    ssh-add $k > /dev/null 2>&1
done
##
##
ln -sf $SSH_AUTH_SOCK ~/.ssh-auth-sock > /dev/null 2>&1
ssh-add -l > /dev/null 2>&1
test $? = 1 && ssh-add > /dev/null 2>&1

portslay () {
   # portslay:  kill the task active on the specified TCP port
   kill -9 `lsof -i tcp:$1 | tail -1 | awk '{ print $2;}'`
}

# GIT SECRETS
source "$HOME/.secrets/git.bashrc"

# MOAR SECRETS
source "$HOME/.secrets/chef/chef.bashrc"
source "$HOME/.secrets/ec2/aws.bashrc"
source "$HOME/.secrets/ec2/heroku.bashrc"
source "$HOME/.secrets/ec2/replyyes.bashrc"

#export ANDROID_HOME="/usr/local/Cellar/android-sdk/r18"
export ANDROID_HOME="/usr/local/opt/android-sdk"
export PATH="$ANDROID_HOME/bin:$ANDROID_HOME/platform-tools:/usr/local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:$HOME/bin"


if [[ $PLATFORM == 'osx' ]];
then
    export JAVA6_HOME="/System/Library/Frameworks/JavaVM.framework/Home"
    export JAVA7_HOME="$(/usr/libexec/java_home)"
    export JAVA_HOME=$JAVA7_HOME
    export SPARK_HOME="/usr/local/Cellar/$(readlink "$(which spark-shell)" | xargs dirname)/../libexec"
fi

# text stuff
export TT_HOME="$HOME/code/tamingtext-book"
export MAHOUT_HOME="$HOME/code/foss-clones/mahout"
export MAHOUT_CONF_DIR="$MAHOUT_HOME/conf"
export MAHOUT_LOCAL="true"


export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"

export PATH="$HOME/.rbenv/bin:$PATH"
export PATH="$PATH:$HOME/code/foss-clones/buck/bin"

if [[ $(type -t rbenv) ]] ;
then
  eval "$(rbenv init -)"
fi
export PATH="$HOME/.cabal/bin:$PATH"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

if [ -e /Users/brendan/.nix-profile/etc/profile.d/nix.sh ]
then
    . /Users/brendan/.nix-profile/etc/profile.d/nix.sh
    export NIXPKGS=/Users/brendan/code/foss-clones/nixpkgs
fi # added by Nix installer

export HISTFILESIZE=10000
export PKG_CONFIG_PATH=~/.nix-profile/lib/pkgconfig:/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH


export PATH=/Users/brendan/torch/install/bin:$PATH  # Added automatically by torch-dist
export LD_LIBRARY_PATH=/Users/brendan/torch/install/lib:$LD_LIBRARY_PATH  # Added automatically by torch-dist
export DYLD_LIBRARY_PATH=/Users/brendan/torch/install/lib:$DYLD_LIBRARY_PATH  # Added automatically by torch-dist

export PGUSER=postgres
