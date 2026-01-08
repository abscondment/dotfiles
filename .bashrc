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
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] 👻 \$ '

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
export HISTSIZE=20000

# alias xcodebuild="xcodebuild -activetarget -activeconfiguration -sdk iphonesimulator4.0"

export EDITOR=emacs

# ssh-agent
SSH_ENV=$HOME/.ssh/environment

# start the ssh-agent
function start_agent {
    echo "Initializing new SSH agent..."
    # spawn ssh-agent
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add
}

if [ -f "${SSH_ENV}" ]; then
     . "${SSH_ENV}" > /dev/null
     ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi

## Add keys to forward:

ssh_add_nodupe () {
    k=$(ls -x1 $1)
    exists=$(ssh-add -l | grep "$k")
    if [ ! -n "$exists" ]; then
        ssh-add $k > /dev/null 2>&1
    fi
}

ls -x1 ~/.ssh/id_* | grep -v '.pub$' | while read k
do
    ssh_add_nodupe $k
done

portslay () {
   # portslay:  kill the task active on the specified TCP port
   kill -9 `lsof -i tcp:$1 | tail -1 | awk '{ print $2;}'`
}

ag1_tools () {
    ag1tools=$(kubectl --context ag1-va-rms -n rms get pods -l app=tools -o json | jq '.items[0].metadata.name' -r)
    kubectl --context ag1-va-rms -n rms exec -it ${ag1tools} -- /bin/bash -c "tmux attach || tmux"
}

shuf() { awk 'BEGIN {srand(); OFMT="%.17f"} {print rand(), $0}' "$@" |
               sort -k1,1n | cut -d ' ' -f2-; }

# add private bashrcs
while read -r f ; do source "$f" ;  done < <(find "$HOME/.secrets/" -iname '*.bashrc')

export PATH="$PATH:$HOME/go/bin:/opt/homebrew/bin"

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

export PATH="$HOME/bin:$PATH"

nix? () {
    nix-env -qa \* -P | fgrep -i "$1"
}

nix-show-upgrades () {
    nix-channel --update && nix-env --upgrade --dry-run
}

# add ls alias after nix so that we get --color on OSX
alias ls="ls -l --color=always"

export PGUSER="$(whoami)"

# add node-installed binaries to PATH
enable_npm_bin () {
    npm_binary=$(which npm)
    if [ -n "$npm_binary" ]
    then
        export PATH="$PATH:$(dirname $(readlink $npm_binary))"
    fi
}

export NVM_DIR="$HOME/.nvm"

# One of these loads nvm
# [ -s "$NVM_DIR/nvm.sh" ]#  && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"

# One of these loads nvm bash_completion
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"
