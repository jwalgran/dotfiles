#http://osxdaily.com/2013/02/05/improve-terminal-appearance-mac-os-x/

export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ls='ls -G'

#http://backup.noiseandheat.com/blog/2011/12/os-x-lion-terminal-colours/
export GREP_OPTIONS='--color=auto'

if type "brew" > /dev/null; then
  if type "grc" > /dev/null; then
    source "`brew --prefix`/etc/grc.bashrc"
  fi
fi
