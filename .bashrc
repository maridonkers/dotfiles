# https://askubuntu.com/questions/432217/prevent-duplicate-entries-in-path
[[ ":$PATH:" =~ ":~/bin:" ]] || PATH="$PATH:~/bin"
[[ ":$PATH:" =~ ":~/.local/bin" ]] || PATH="~/.local/bin:$PATH"
[[ ":$PATH:" =~ ":~/.cargo/bin" ]] || PATH="$PATH:~/.cargo/bin"
[[ ":$PATH:" =~ ":~/.cabal/bin" ]] || PATH="$PATH:~/.cabal/bin"

export TERM="xterm-256color"              # getting proper colors
export HISTCONTROL=ignoredups:erasedups   # no duplicate entries

# Rust
# https://internals.rust-lang.org/t/cargo-sparse-protocol-feedback-thread/18234
export CARGO_REGISTRIES_CRATES_IO_PROTOCOL="sparse"

# IHP
export IHP_TELEMETRY_DISABLED=1
export IHP_BROWSER=firefox
export IHP_EDITOR="emacs"
eval "$(direnv hook bash)"

# https://www.atlassian.com/git/tutorials/dotfiles
alias config='git --git-dir=/home/mdo/.cfg/ --work-tree=/home/mdo'

alias b="bat -n"
alias sless="vim -R"
alias mdless="mdcat -p"
alias lcat="less -EX"
alias bl="LESSOPEN='' less"  

alias ls="ls --color=never"
alias ll="exa"
alias l="exa -l"

alias en="trans -s en"
alias nlen="trans -s nl -t en"
alias ennl="trans -s en -t nl"
alias esnl="trans -s es -t nl"
alias esen="trans -s es -t en"
alias frnl="trans -s fr -t nl"
alias pten="trans -s pt -t en"
alias ptnl="trans -s pt -t nl"
alias fren="trans -s fr -t en"

export LYNX_LSS=$HOME/lynx.lss

# EMH emh: displays help for command line e-mail commands
alias emh="grep '^# EMH ' ~/.bashrc | sed -e 's/^# EMH //'"
# EMH emo: gets new e-mails via mbsync (beware: not added to notmuch; use emn command to add)
alias emo="mbsync -a"
# EMH emg: gets new e-mails via mbsync and adds them to notmuch
alias emg="mbsync -a && notmuch new"

# EMH emn: adds new e-mails to notmuch
alias emn="notmuch new"
# EMH emf: searches e-mail for search term; see man notmuch-search-terms
alias emf="notmuch search"
# EMH emlt: list e-mails for today
alias emlt="notmuch search date:today"
# EMH emly: list e-mails from yesterday to today
alias emly="notmuch search date:yesterday..today"
# EMH emld: list e-mails from # days to today
function emld() { # get list of ID's in e-mail
  notmuch search date:$1d..today
}
# EMH emlw: list e-mails for week to today
alias emlw="notmuch search date:week..today"
# EMH emlm: list e-mails for month to today
alias emlm="notmuch search date:month..today"
# EMH ems: show e-mail content, including textual HTML dump for the entire thread (if applicable)
alias ems="notmuch show --include-html --entire-thread=true"
# EMH emsl: list mime IDs
function emsl() { # get list of ID's in e-mail
  ems $1 | grep '{ ID:'
}
# EMH emsi: show image content by ID in feh image viewer
function emsi() { # show image by ID in e-mail
  notmuch show --part=$1 $2 | feh -
}
# EMH emsp: show PDF content by ID in zathura PDF viewer
function emsp() { # show PDF by ID in e-mail
  notmuch show --part=$1 $2 | zathura -
}
# EMH emsm: show mime content by ID (likely clutters terminal, hence store in file).
function emsm() { # show mime part by ID in e-mail
  notmuch show --part=$1 $2
}
# EMH emsa: show attachment by ID and type (likely clutters terminal, hence store in file).
# EMH       attachment{ ID: 5, Filename: Brief voor - ambassade def2.docx, Content-id: ...
# EMH       verify type of attachment by ID and type in e-mail; e.g.: emsa 5 def2.docx thread:0000000000005dea
function emsa() { # show attachment by ID and type in e-mail; e.g.: emsa 5 docx thread:0000000000005dea
  notmuch show --part=$1 attachment:$2 and $3
}
# EMH emsav: verify attachment type by ID and type (reported is file type).
# EMH        attachment{ ID: 5, Filename: Brief voor - ambassade def2.docx, Content-id: ...
# EMH        verify type of attachment by ID and type in e-mail; e.g.: emsav 5 def2.docx thread:0000000000005dea
function emsav() {
  notmuch show --part=$1 attachment:$2 and $3 | file -
}
# EMH emsww: show HTML content by ID in lynx command line webbrowser
function emsww() {
  notmuch show --part=$1 $2 | lynx -stdin
}
# EMH emsw: show HTML content (ID determined automatically) in lynx command line webbrowser
function emsw() {
  IDX=`emsl $1 | grep 'Content-type: text/html' | sed -e "s/^.*ID: \([0-9]\+\),.*$/\1/"`
  if [ "$IDX" != "" ]
  then
    notmuch show --part=$IDX $1 | lynx -stdin
  else
    echo "NO HTML CONTENT"
  fi
}

alias ps="ps -ww"
alias scl="screen -ls"
alias scs="screen -S"
alias scz="screen -ls | grep '(Detached)'"
alias scr="screen -DR"

alias speedtest="speedtest -p"

alias lsblks="lsblk -o +FSSIZE,FSAVAIL,PTTYPE,HOTPLUG,UUID"

alias externip="dig +short myip.opendns.com @resolver1.opendns.com"

alias sophia="ssh -i ~/.ssh/id_rsa-sophia mdo@192.168.1.43"

alias nixsearch="nix search nixpkgs"
alias nix-env="PAGER= nix-env"

alias pt-euronews="mpv https://www.youtube.com/watch?v=fLtn2L7OdeI &"

alias es-dw="mpv https://youtube.com/watch?v=RTjbYKBB828 &"
alias es-canal24="mpv https://youtube.com/watch?v=xR-4NDFsYNk &"
alias es-france24="mpv https://www.youtube.com/watch?v=XDJPzMznAjU &"
alias es-euronews="mpv https://www.youtube.com/watch?v=O9mOtdZ-nSk &"

alias fr-france24="mpv https://www.youtube.com/watch?v=gxG3pdKvlIs &"
alias fr-franceinfo="mpv https://www.youtube.com/watch?v=Z-Nwo-ypKtM &"
alias fr-euronews="mpv https://www.youtube.com/watch?v=MsN0_WNXvh8 &"

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
