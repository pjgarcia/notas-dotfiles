###############################
# Comandos y funciones utiles #
###############################

Blue="\033[0;34m"
Green="\033[0;32m"
Red="\033[0;31m"
None="\033[0m"

function git_info_prompt() {
    # TODO agregar actualizacion con 'git fetch'
    git st &> /dev/null
    if [ $? -eq 0 ]; then
	branch=$(git branch | grep -e '*' | cut -d " " -f 2)
	git st | grep -i "your branch is up-to-date" &> /dev/null
	if [ $? -eq 0 ]; then
	    color=$Green
	fi
	git st | grep -i "your branch is ahead" &> /dev/null
	if [ $? -eq 0 ]; then
	    color=$Blue
	fi
	git st | grep -i "your branch is behind" &> /dev/null
	if [ $? -eq 0 ]; then
	    color=$Red
	fi
	echo -e "${color} (${branch}) ${None}"
    fi
}

PS1="\[\033[01;32m\]@\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$(git_info_prompt)$ "


function holapibe() {
    echo "\$FUNCNAME: '$FUNCNAME'"
    echo "\$0: '$0'"
    echo "Hola Pibeee!"
}

extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1        ;;
            *.tar.gz)    tar xzf $1     ;;
            *.bz2)       bunzip2 $1       ;;
            *.rar)       rar x $1     ;;
            *.gz)        gunzip $1     ;;
            *.tar)       tar xf $1        ;;
            *.tbz2)      tar xjf $1      ;;
            *.tgz)       tar xzf $1       ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1  ;;
            *.7z)        7z x $1    ;;
            *)           echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

compress () {
    FILE=$1
    shift
    case $FILE in
	*.tar.bz2) tar cjf $FILE $*  ;;
	*.tar.gz)  tar czf $FILE $*  ;;
	*.tgz)     tar czf $FILE $*  ;;
	*.zip)     zip $FILE $*      ;;
	*.rar)     rar $FILE $*      ;;
	*)         echo "Filetype not recognized" ;;
    esac
}
