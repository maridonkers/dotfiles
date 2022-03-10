# https://askubuntu.com/questions/432217/prevent-duplicate-entries-in-path
[[ ":$PATH:" =~ ":~/bin:" ]] || PATH="$PATH:~/bin"
[[ ":$PATH:" =~ ":~/.local/bin" ]] || PATH="~/.local/bin:$PATH"
[[ ":$PATH:" =~ ":~/.cabal/bin" ]] || PATH="$PATH:~/.cabal/bin"

export TERM="xterm-256color"              # getting proper colors
export HISTCONTROL=ignoredups:erasedups   # no duplicate entries

# IHP
export IHP_TELEMETRY_DISABLED=1
export IHP_BROWSER=firefox
export IHP_EDITOR="emacs"
eval "$(direnv hook bash)"

# https://www.atlassian.com/git/tutorials/dotfiles
alias config='git --git-dir=/home/mdo/.cfg/ --work-tree=/home/mdo'

alias sless="vim -R"  
alias mdless="mdcat -p"

alias ls="ls --color=never"
alias l="exa"
alias ll="exa -l"

alias ps="ps -ww"
alias scl="screen -ls"
alias scs="screen -S"
alias scz="screen -ls | grep '(Detached)'"
alias scr="screen -DR"

export LESSOPEN="| highlight --out-format=xterm256 %s"
export LESS=' -R '

# Security CAM.
export SC_IPADDRESS="192.168.1.4"

export COWPATH="${HOME}/lib/cowfiles.nixos:${HOME}/lib/cowfiles"
#~/bin/randomcowsay

# https://gitlab.com/dwt1/dotfiles/-/blob/master/.bashrc
if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
else
    PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
fi
