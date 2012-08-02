# users generic .zshrc file for zsh(1)

## Environment variable configuration
#
# LANG
#
export LANG=ja_JP.UTF-8
case ${UID} in
0)
    LANG=C
    ;;
esac


# auto change directory
#
setopt auto_cd

# auto directory pushd that you can get dirs list by cd -[tab]
#
setopt auto_pushd

# command correct edition before each completion attempt
#
setopt correct

# compacked complete list display
#
setopt list_packed

# no remove postfix slash of command line
#
setopt noautoremoveslash

# no beep sound when complete list displayed
#
setopt nolistbeep


## Keybind configuration
#
# emacs like keybind (e.x. Ctrl-a gets to line head and Ctrl-e gets
#   to end) and something additions
#
bindkey -e
bindkey "^u" backward-kill-line
bindkey "^[[1~" beginning-of-line # Home gets to line head
bindkey "^[[4~" end-of-line # End gets to line end
bindkey "^[[3~" delete-char # Del
bindkey "\e[1;5C" forward-word  # ctrl-right
bindkey "\e[1;5D" backward-word # ctrl-left

# historical backward/forward search with linehead string binded to ^P/^N
#
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
bindkey "\\ep" history-beginning-search-backward-end
bindkey "\\en" history-beginning-search-forward-end

# reverse menu completion binded to Shift-Tab
#
bindkey "\e[Z" reverse-menu-complete


## Command history configuration
#
HISTFILE=${HOME}/.zsh_history
HISTSIZE=50000
SAVEHIST=50000
setopt hist_ignore_dups     # ignore duplication command history list
setopt share_history        # share command history data


## Completion configuration
#
fpath=(${HOME}/.zsh/functions/Completion ${fpath})
autoload -U compinit
compinit

## zsh editor
#
#autoload zed


## Prediction configuration
#
#autoload predict-on
#predict-off


## terminal configuration
#
case "${TERM}" in
screen)
    TERM=xterm
    ;;
esac

# colors of ls command, mimic bash
export LS_COLORS="no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.svgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:"



# set terminal title including current directory
#
case "${TERM}" in
xterm|xterm-color|kterm|kterm-color)
    precmd() {
        echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
    }
    ;;
esac


## load user .zshrc configuration file
#
[ -f ${HOME}/.zshrc.mine ] && source ${HOME}/.zshrc.mine


## Alias configuration
#
# expand aliases before completing
#
setopt complete_aliases     # aliased ls needs if file/dir completions work


PROMPT="%U%B`uname -n`%(!.#.$)%b%u "
RPROMPT="[%~]"


case "${OSTYPE}" in
freebsd*|darwin*)
    alias ls="ls -G -w"
    alias grep="grep -n --color=auto"
    ;;
linux*)
    alias ls="ls -F --color=auto"
    alias grep="grep -n --color=auto"
    ;;
esac


export PATH=~/bin:$PATH


alias la='ls -al'
alias sl='ls'
alias du="du -h"
alias df="df -h"
alias vi='vim'
alias temp='cd ~/temp'
alias conffiles='cd ~/code/conffiles'
alias cdu='cd ..'
alias cduu='cd ../..'
alias cduuu='cd ../../..'
alias cduuuu='cd ../../../..'
alias cduuuuu='cd ../../../../..'
alias emnw='/usr/bin/emacs -nw'


# aliases for git commands
alias gst='git status'
compdef _git gst=git-status
alias gbr='git branch -a'
compdef _git gbr=git-branch
alias glog='git log'
compdef _git glog=git-log
alias gdiff='git diff'
compdef _git gdiff=git-diff
alias gdiffc='git diff --cached'
compdef _git gdiff=git-diff
alias gci='git commit'
compdef _git gci=git-commit
alias gco='git checkout'
compdef _git gco=git-checkout
alias gadd='git add'
compdef _git gadd=git-add


# Ruby
alias irb='irb -r irb/completion'


# D programming
PATH=/opt/dmd/bin:$PATH


# Haskell
PATH=~/.cabal/bin:$PATH


# OS-specific settings
case "${OSTYPE}" in
freebsd*|darwin*)
  function emc(){
    /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -e '(elscreen-create)' 2>/dev/null && /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n $* 2>/dev/null || open -a /Applications/Emacs.app $* &
  }
  ;;
linux*)
  function emc(){
    /usr/bin/emacsclient -n -a /usr/bin/emacs $* > /dev/null 2>&1 &
  }
  # disable screen lock by C-s
  stty -ixon
  ;;
esac

