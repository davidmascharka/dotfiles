# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# show username@hostname and the full path
# the \[ \] pairs enclose non-printable characters so bash does not count them
# toward a line width so wrapping is nice
purple=$(tput setaf 5) # 1-7 are red, green, yellow, blue, purple, cyan, white
reset=$(tput sgr0) # \e[0m
PS1='\[$purple\][\u@\h \w]\$ \[$reset\]'

# nobody uses capslock --- make caps and ctrl both ctrl
/usr/bin/setxkbmap -option "ctrl:nocaps"

# be super lazy starting emacs
alias em='emacsclient -t -a ""'

export EDITOR='emacs -nw'

# get nice colors using ls
alias ls='ls --color=auto'

# when I fuck up ls it still works
alias l='ls'
alias sl='ls'
alias s='ls'

# get nice colors with grep
alias grep='grep --color'

# easily and nicely open files
alias open='xdg-open'
