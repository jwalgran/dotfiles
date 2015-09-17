# Assumes boot2docker
alias dm=docker-machine

function dme () {
    if [ -z "$1" ]
    then
        machine="default"
    else
        machine=$1
    fi
    eval "$(docker-machine env $machine)"
}
