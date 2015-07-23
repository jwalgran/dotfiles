function listening {
  lsof -n -i4TCP:$1 | grep LISTEN
}
