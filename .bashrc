# https://askubuntu.com/questions/432217/prevent-duplicate-entries-in-path
[[ ":$PATH:" =~ ":~/bin:" ]] || PATH="$PATH:~/bin"
[[ ":$PATH:" =~ ":~/.racket/7.5/bin" ]] || PATH="$PATH:~/.racket/7.5/bin"

export PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
export TERM="xterm-256color"              # getting proper colors
export HISTCONTROL=ignoredups:erasedups   # no duplicate entries

# https://www.atlassian.com/git/tutorials/dotfiles
alias config='git --git-dir=/home/mdo/.cfg/ --work-tree=/home/mdo'

# https://gitlab.com/dwt1/dotfiles/-/blob/master/.bashrc
if [[ ${EUID} == 0 ]] ; then
    PS1='\[\033[01;31m\][\h\[\033[01;36m\] \W\[\033[01;31m\]]\$\[\033[00m\] '
else
    PS1='\[\033[01;32m\][\u@\h\[\033[01;37m\] \W\[\033[01;32m\]]\$\[\033[00m\] '
fi
