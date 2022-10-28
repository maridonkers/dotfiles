# https://askubuntu.com/questions/432217/prevent-duplicate-entries-in-path
[[ ":$PATH:" =~ ":~/bin:" ]] || PATH="$PATH:~/bin"
[[ ":$PATH:" =~ ":~/.local/bin" ]] || PATH="~/.local/bin:$PATH"
[[ ":$PATH:" =~ ":~/.cabal/bin" ]] || PATH="$PATH:~/.cabal/bin"

export TERM="xterm-256color"              # getting proper colors
export HISTCONTROL=ignoredups:erasedups   # no duplicate entries

# PAGER is set to `less` by default but I'll tell you when I want to paginate!
#unset PAGER
export PAGER=""

# IHP
export IHP_TELEMETRY_DISABLED=1
export IHP_BROWSER=firefox
export IHP_EDITOR="emacs"
eval "$(direnv hook bash)"

# https://www.atlassian.com/git/tutorials/dotfiles
alias config='git --git-dir=/home/mdo/.cfg/ --work-tree=/home/mdo'

alias sless="vim -R"  
alias mdless="mdcat -p"
alias lcat="less -EX"
alias bl="LESSOPEN='' less"  

alias ls="ls --color=never"
alias ll="exa"
alias l="exa -l"

alias lsdict="dict -h dict.org -I"
alias dicten="dict -h dict.org -d gcide"
alias dictnlen="dict -h dict.org -d fd-nld-eng"
alias dictennl="dict -h dict.org -d fd-eng-nld"
alias dictesen="dict -h dict.org -d fd-spa-eng"
alias dictfrnl="dict -h dict.org -d fd-fra-nld"

alias emi="offlineimap --info"
alias emc="offlineimap --info 2>&1 | grep 'offlineimap.error.OfflineImapError:'"
alias emo="offlineimap -q"
alias emof="offlineimap"
alias emgf="offlineimap && notmuch new"
alias emg="offlineimap -q && notmuch new"

alias emn="notmuch new"
alias emf="notmuch search"
alias eml="notmuch search date:today"
alias emlt="notmuch search date:today"
alias emly="notmuch search date:yesterday..today"
alias emlw="notmuch search date:week"
alias emlm="notmuch search date:month"
alias ems="notmuch show --include-html --entire-thread=true"
alias emsp="notmuch show --part "

alias ps="ps -ww"
alias scl="screen -ls"
alias scs="screen -S"
alias scz="screen -ls | grep '(Detached)'"
alias scr="screen -DR"

alias lsblks="lsblk -o +FSSIZE,FSAVAIL,PTTYPE,HOTPLUG,UUID"

alias sophia="ssh -i ~/.ssh/id_rsa-sophia mdo@192.168.1.43"

alias nixsearch="nix search nixpkgs"

alias france24="mpv https://www.youtube.com/watch?v=gxG3pdKvlIs &"
alias franceinfo="mpv https://www.youtube.com/watch?v=Z-Nwo-ypKtM &"
alias euronews="mpv https://www.youtube.com/watch?v=MsN0_WNXvh8 &"

export LESSOPEN="| highlight --out-format=xterm256 --style=clarity %s"
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
