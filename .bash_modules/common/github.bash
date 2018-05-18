function ppr() {
  git branch -D pr-$1
  git fetch origin pull/$1/head:pr-$1
  git checkout pr-$1
  git fetch origin develop
  git merge develop -m "Merge develop"
}
