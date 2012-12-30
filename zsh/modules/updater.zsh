function update() {
  if [[ ! -f $HOME/.infect ]]; then
    cd -q $(<$HOME/.infect)
    ./infect
    cd -q -
  else
    zerror "Infect file missing."
  fi
}
