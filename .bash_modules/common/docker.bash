function docker-rmuntagged {
    docker rmi $(docker images | grep "^<none>" | awk '{print $3}')
}

alias dc=docker-compose
