export PATH="$HOME/.rbenv/bin:$HOME/Applications:$HOME/Applications/azavea/bin:$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH"

# Old exports
# export PATH="$HOME/Applications/sbt/bin:$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH:$HOME/Applications"
# export PATH="/Users/jwalgran/Applications/azavea/bin:/Users/jwalgran/Applications/sbt/bin:/Users/jwalgran/Applications/play:$PATH:/usr/local/sbin"

export NODE_PATH="/usr/local/lib/node_modules"

eval "$(rbenv init -)"

# Jumping with symbolic links
# http://jeroenjanssens.com/2013/08/16/quickly-navigate-your-filesystem-from-the-command-line.html
export MARKPATH=$HOME/.marks
function jump {
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark {
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark {
    rm -i "$MARKPATH/$1"
}
function marks {
    ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}
_completemarks() {
  local curw=${COMP_WORDS[COMP_CWORD]}
  local wordlist=$(find $MARKPATH -type l -printf "%f\n")
  COMPREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
  return 0
}

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# http://brettterpstra.com/a-simple-but-handy-bash-function-console
function console () {
  if [[ $# > 0 ]]; then
    query=$(echo "$*"|tr -s ' ' '|')
    tail -f /var/log/system.log|grep -i --color=auto -E "$query"
  else
    tail -f /var/log/system.log
  fi
}

# see: http://stufftohelpyouout.blogspot.com/2010/01/show-name-of-git-branch-in-prompt.html
# see also: http://superuser.com/questions/31744/how-to-get-git-completion-bash-to-work-on-mac-os-x
# see also: http://stackoverflow.com/questions/347901/what-are-your-favorite-git-features-or-tricks
# This assumes you have installed Homebrew ( http://github.com/mxcl/homebrew )
# and then installed Git via Homebrew with the default installation location:
# ruby -e "$(curl -fsS http://gist.github.com/raw/323731/install_homebrew.rb)"
# brew install wget
# brew install git
# brew update
if [ -f $(brew --prefix)/etc/bash_completion.d/git-completion.bash ]; then
    . $(brew --prefix)/etc/bash_completion.d/git-completion.bash;
  PS1='[\h \W$(__git_ps1 " (%s)")]\$ ';
fi

# {{{
# Node Completion - Auto-generated, do not touch.
shopt -s progcomp
for f in $(command ls ~/.node-completion); do
  f="$HOME/.node-completion/$f"
  test -f "$f" && . "$f"
done
# }}}

# Fix terminal colors
# http://it.toolbox.com/blogs/lim/how-to-fix-colors-on-mac-osx-terminal-37214
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

alias t="tree -L 1"
alias tt="tree -L 2"
alias ttt="tree -L 3"
