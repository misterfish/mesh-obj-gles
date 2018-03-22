# --- hush all 'info' statements
_quiet_info=no
# --- hush all non-warn & non-error output (including info statements)
_quiet_say=no

quiet-say () {
    _quiet_say=yes
}

noisy-say () {
    _quiet_say=no
}

quiet-info () {
    _quiet_info=yes
}

noisy-info () {
    _quiet_info=no
}

bullet () {
    echo ٭
}

beep () {
    echo ""
}

color () {
    c=$1
    shift

    doit=no

    if [ -t 0 -o -n "${force_colors:-}" ]; then
        doit=yes
    fi

    # --- difficult to make it work right for printf.
    if [ "$doit" = 'yes' ]; then
        echo -n [${c}m"${@}"[0m
    else
        echo -n "$@"
    fi
}

green () {
    color 32 "$@"
}
bright-green () {
    color 92 "$@"
}
yellow () {
    color 33 "$@"
}
bright-yellow () {
    color 93 "$@"
}
red () {
    color 31 "$@"
}
bright-red () {
    color 91 "$@"
}
blue () {
    color 34 "$@"
}
bright-blue () {
    color 94 "$@"
}
magenta () {
    color 35 "$@"
}
bright-magenta () {
    color 95 "$@"
}
cyan () {
    color 36 "$@"
}
bright-cyan () {
    color 96 "$@"
}

doit () {
    echo "$@"
    "$@"
}

shell-quote () {
    for i in "$@"; do
        # --- ] has to be the first thing in the list.
        if [[ "$i" =~ [][:space:]\;\$\!\&\*\(\)\{\[\}\<\>\?\~\`\'\"] ]] ; then
            # --- if it contains a special char or a space, do the quoting.
            #
            # simple algorithm: substitute every single quote with single
            # quote + backslash + single quote + single quote, then surround
            # the entire thing with single quotes.
            #
            # regex preceded by / to mean global.
            #
            # (yes really).
            printf -- "%s%s%s" \' "${i//\'/\'\\\'\'}"  \'
        else
            printf -- "%s" "$i"
        fi
    done
}

shell-quote-each () {
    declare q
    local i
    for i in "$@"; do
        push q "$(shell-quote "$i")"
    done
    echo -n "${q[@]}"
}

cmd () {
    local b
    b=$(green "$(bullet)")
    first="$1"
    shift
    sayf "$b %s %s" "$(cyan "$(shell-quote "$first")")" "$(shell-quote-each "$@")"
    "$first" "$@"
}

cmd_eval () {
    green "$(bullet) "
    echo "$@"
    eval "$@"
}

# --- expects arrayname to be simple word -- no shell quote performed.
push () {
    name=$1
    eval "$name=(\"\${$name[@]}\" \"$2\")"
}

# --- only works with single char join char.
# --- name, joinchar = ' '
join () {
    local name="$1"
    local saveifs="$IFS"
    local ret

    IFS="${2:- }"
    eval "ret=\"\${${name}[*]}\""
    IFS="$saveifs"
    echo "$ret"
}

assert_arg () {
    var=$1
    if [ -z "$var" ]; then
        echo "error assert_arg"
        exit 1
    fi
    eval "val=\$$var"
    if [ -z "$val" ]; then
        if [ -n "$USAGE" ]; then
            echo $USAGE
        fi
        exit 1
    fi
}

say () {
    if [ "$_quiet_say" = yes ]; then
        return
    fi
    echo "$@"
}

sayf () {
    local e
    printf -v e "$@"
    say "$e"
}

info () {
    if [ "$_quiet_info" = yes ]; then
        return
    fi
    local b
    b=$(bright-blue "$(bullet)")
    if [ "$1" = '-n' ]; then
        shift
        say -n "$b $@"
    else
        say "$b $@"
    fi
}

error () {
    printf >&2 "%s Error: %s\n" $(red "$(bullet)") "$*"
    exit 1
}

warn () {
    printf >&2 "%s %s\n" $(bright-red "$(bullet)") "$*"
}

infof () {
    local e
    local one="$1"
    shift
    printf -v e "$one" "$@"
    info "$e"
}

errorf () {
    local e
    local one="$1"
    shift
    printf -v e "$one" "$@"
    error "$e"
}

warnf () {
    local e
    local one="$1"
    shift
    printf -v e "$one" "$@"
    warn "$e"
}

press_enter () {
    perl -e "print 'Press enter to continue. '; <STDIN>"
}

shout () {
    "$@" >/dev/null
}

sherr () {
    "$@" 2>/dev/null
}

quiet () {
    shout sherr "$@"
}

redirect-out () {
    local file=$1
    shift
    "$@" > "$file"
}
redirect-out-append () {
    local file=$1
    shift
    "$@" >> "$file"
}
redirect-err () {
    local file=$1
    shift
    "$@" 2> "$file"
}
redirect-err-append () {
    local file=$1
    shift
    "$@" 2>> "$file"
}
redirect-out-and-err () {
    local file=$1
    shift
    "$@" > "$file" 2>&1
}
redirect-out-and-err-append () {
    local file=$1
    shift
    "$@" >> "$file" 2>&1
}
redirect-err-to-out () {
    "$@" 2>&1
}

waitfor () {
    local proc
    for proc in "$@"; do
        while ! quiet ps -C "$proc"; do
            info "Still waiting for %s\n" $(yellow "$proc")
            sleep 1
        done
    done
}

# --- dies.
chd () {
    local dir="$1"
    shift
    if [ ! -e "$dir" ]; then
        errorf "Dir %s doesn't exist" "$(red "$dir")"
    fi
    redirect-out /dev/null pushd "$dir"
    infof "[ %s ] %s" "$(yellow 'chdir')" "$dir"
    if [ $? != 0 ]; then
        errorf "Couldn't cd to dir %s" "$(red "$dir")"
    fi

    if [ "$@" ]; then
        cmd "$@"
        cd -
    fi
}

mkd () {
    local dir="$1"
    cmd mkdir -p "$dir"
}

# --- dies.
mkchd () {
    local dir="$1"
    mkd "$dir"
    chd "$dir"
}

# --- dies.
chd-back () {
    chd-back-n 1
}

# --- dies.
chd-back-n () {
    local n="$1"
    local dir
    dir=$(dirs +$n)
    infof "[ %s %s ] %s" "$(yellow 'chdir-back')" "$(bright-red "$n")" "$dir"
    redirect-out /dev/null popd
}

# --- usage: e.g. cwd .. command
# --- dies if unable to cd; otherwise returns exit val of call.
cwd () {
    local dir="$1"
    chd "$dir"
    shift
    "$@"
    ret=$?
    cd -
    if [ "$ret" = 0 ]; then
        true
    else
        false
    fi
}

pipe () {
    local rt="$1"
    shift
    sayf "%s [ %s ] %s | %s" "$(green "$(bullet)")" "$(yellow pipe)" "$*" "$(green "$rt")"
    "$@" | "$rt"
}

forkit () {
    sayf "%s [ %s ] %s" "$(green "$(bullet)")" "$(yellow fork)" "$(shell-quote-each "$@")"
    "$@" &
}

# --- assembles its arguments by joining on /.
# --- croaks if:
# - any of the arguments is empty,
# - the first argument begins with a slash
# - or the final string exists but is not a dir.
# --- as an exception you can use '.' as the final element to allow an
# absolute path.
# --- no-op if the final string doesn't exist.

_safe-rm-dir () {
    local allow_absolute=no
    allow_absolute="$1"
    shift

    local dir=''
    local refuse=no
    local reason=
    local i
    local first=yes

    for i in "$@"; do
        if [ "$i" = "" ]; then
            refuse=yes
            reason="path element empty"
        fi
        if [ "$first" = yes ]; then
            first=no
            dir="$i"
        else
            dir="$dir/$i"
        fi
    done

    if [ "$refuse" = no ]; then
        if [[ "$allow_absolute" != yes && "$dir" =~ ^/ ]]; then
            refuse=yes
            reason="path is absolute"
        elif [ ! -e "$dir" ]; then
            return 0
        elif [ ! -d "$dir" ]; then
            refuse=yes
            reason="not a dir"
        fi
    fi
    if [ "$refuse" = yes ]; then
        error "$(printf "Refusing to remove %s (%s)" $(bright-red "$dir") "$reason")"
    fi
    cmd rm -rf "$dir"
}

safe-rm-dir () {
    _safe-rm-dir no "$@"
}

safe-rm-dir-allow-absolute () {
    _safe-rm-dir yes "$@"
}

xport () {
    local var="$1"
    local val="$2"

    info "$(printf "[ %s ] %s %s" "$(yellow env)" "$(bright-red "$var")" "$val" )"
    read $var <<< "$val"
    export $var
}

xport-prepend () {
    local var="$1"
    local val="$2"
    info "$(printf "[ %s ] %s %s" "$(yellow env-prepend)" "$(bright-red "$var")" "$val" )"
    local cur=$(eval "echo \$$var")
    local concat="$val:$cur"
    xport "$var" "$concat"
}

xport-append () {
    local var="$1"
    local val="$2"
    info "$(printf "[ %s ] %s %s" "$(yellow env-append)" "$(bright-red "$var")" "$val" )"
    local cur=$(eval "echo \$$var")
    local concat="$cur:$val"
    xport "$var" "$concat"
}

# ------ short-circuit on emptiness.
doublebar () {
    if [ -n "$1" ]; then echo "$1"; else echo "$2"; fi
}
ternary () {
    if [ -n "$1" ]; then echo "$2"; else echo "$3"; fi
}

multiline-cmd-init () {
    __multiline=
}

multiline-cmd-build () {
    if [ "${__multiline+fish}" != fish ]; then
        error "must call multiline-cmd-init before multiline-cmd-build"
    fi
    __multiline="$__multiline $@"
}

multiline-cmd-go () {
    cmd $__multiline
}

mcg () {
    multiline-cmd-go "$@"
}

mci () {
    multiline-cmd-init "$@"
}

mcb () {
    multiline-cmd-build "$@"
}

