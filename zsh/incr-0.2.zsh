# Incremental completion for zsh
# Original author: y.fujii <y-fujii at mimosa-pudica.net>, public domain
# Some additions (and bugs!) by skirino

autoload -U compinit
zle -N self-insert self-insert-incr
zle -N backward-delete-char-incr
zle -N backward-kill-line-incr
zle -N expand-or-complete-prefix-incr
zle -N accept-line-incr

compinit

bindkey -M emacs '^h' backward-delete-char-incr
bindkey -M emacs '^?' backward-delete-char-incr
bindkey -M emacs '^U' backward-kill-line-incr
bindkey -M emacs '^i' expand-or-complete-prefix-incr
bindkey -M emacs '^M' accept-line-incr

unsetopt automenu
compdef -d scp
compdef -d tar
compdef -d make
compdef -d java
compdef -d svn
compdef -d cvs
compdef -d rake


now_predict=0

function limit-completion
{
    if ((compstate[nmatches] <= 1)); then
        zle -M ""
    elif ((compstate[list_lines] > 10)); then
        compstate[list]=""
        zle -M "too many matches."
    fi
}

function correct-prediction
{
    if ((now_predict == 1)); then
        if [[ "$BUFFER" != "$buffer_prd" ]] || ((CURSOR != cursor_org)); then
            now_predict=0
        fi
    fi
}

function remove-prediction
{
    if ((now_predict == 1)); then
        BUFFER="$buffer_org"
        now_predict=0
    fi
}

function show-prediction
{
    # assert(now_predict == 0)
    if ((PENDING == 0)) && ((CURSOR > 1)) && [[ "$PREBUFFER" == "" ]] && [[ "$BUFFER[CURSOR]" != " " ]]; then
        cursor_org="$CURSOR"
        buffer_org="$BUFFER"
        comppostfuncs=(limit-completion)
        zle complete-word
        cursor_prd="$CURSOR"
        buffer_prd="$BUFFER"
        if [[ "$buffer_org[1,cursor_org]" == "$buffer_prd[1,cursor_org]" ]]; then
            CURSOR="$cursor_org"
            if [[ "$buffer_org" != "$buffer_prd" ]] || ((cursor_org != cursor_prd)); then
                now_predict=1
            fi
        else
            BUFFER="$buffer_org"
            CURSOR="$cursor_org"
        fi
        echo -n "\e[32m"
    else
        zle -M ""
    fi
}

function self-insert-incr
{
    correct-prediction
    remove-prediction
    if zle .self-insert; then
        show-prediction
    fi
}

function backward-delete-char-incr
{
    correct-prediction
    remove-prediction
    if zle backward-delete-char; then
        show-prediction
    fi
}

function expand-or-complete-prefix-incr
{
    correct-prediction
    if ((now_predict == 1)); then
        CURSOR="$cursor_prd"
        now_predict=0
        comppostfuncs=(limit-completion)
        zle list-choices
    else
        remove-prediction
        zle expand-or-complete-prefix
    fi
}

function backward-kill-line-incr
{
    correct-prediction
    remove-prediction
    if zle backward-kill-line; then
        show-prediction
    fi
}

function accept-line-incr
{
    remove-prediction
    echo -n "\e[39m"
    zle accept-line
}
