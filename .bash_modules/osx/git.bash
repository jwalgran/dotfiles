if [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
  . /usr/local/etc/bash_completion.d/git-completion.bash;
fi

if [ -f /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash ]; then
    . /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-completion.bash;
fi

if [ -f /Library/Developer/CommandLineTools/usr/share/git-core/git-prompt.sh ]; then
  . /Library/Developer/CommandLineTools/usr/share/git-core/git-prompt.sh;
  PS1='[\h \W$(__git_ps1 " (%s)")]\$ ';
fi

if [ -f /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-prompt.sh ]; then
    . /Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-prompt.sh;
    PS1='[\h \W$(__git_ps1 " (%s)")]\$ ';
fi
