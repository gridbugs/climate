#!/usr/bin/env bash

PROGRAM_NAME="foo"

STATUS_DONE=100
STATUS_ERROR_WORD_OUT_OF_BOUNDS=101
STATUS_ERROR_WORD_INDEX_PAST_CURSOR=102

status_is_done() {
    test "$1" -eq "$STATUS_DONE"
}
status_is_error() {
    test "$1" -gt "$STATUS_DONE"
}

comp_words_count() {
    echo "${#COMP_WORDS[@]}"
}
comp_words_get_nth() {
    local i=$1
    if [ "$i" -ge "$(comp_words_count)" ]; then
	return $STATUS_ERROR_WORD_OUT_OF_BOUNDS
    fi
    echo "${COMP_WORDS[$i]}"
}
__COMP_WORDS_CURRENT_INDEX=0
comp_words_traverse_init() {
    __COMP_WORDS_CURRENT_INDEX=0
}
comp_words_traverse_get_current() {
    comp_words_get_nth "$__COMP_WORDS_CURRENT_INDEX"
}
comp_words_traverse_advance() {
    __COMP_WORDS_CURRENT_INDEX=$((__COMP_WORDS_CURRENT_INDEX + 1))
}
comp_words_traverse_is_at_cursor() {
    test "$__COMP_WORDS_CURRENT_INDEX" -eq "$COMP_CWORD"
}
comp_words_traverse_is_past_cursor() {
    test "$__COMP_WORDS_CURRENT_INDEX" -gt "$COMP_CWORD"
}
comp_words_traverse_advance_if_equals_sign() {
    if [ "$(comp_words_traverse_get_current)" == "=" ]; then
	comp_words_traverse_advance
    fi
}


# Takes the word under the cursor (just the portion up to the cursor)
# and a space separated list of completion strings.
add_reply_fixed() {
    local suggestions
    mapfile -t suggestions < <(compgen -W "$2" -- "$1")
    for suggestion in "${suggestions[@]}"; do
	COMPREPLY+=("$suggestion ")
    done
}

# Takes the word under the cursor (just the portion up to the cursor) and a
# space separated list of completion strings, where a completion string ending
# with a "=" indicates that it expects a parameter. Each completion string is
# added to the commands completion suggestions with no space following completion
# strings which expect a parameter, and a space following strings which do not.
add_reply_no_space_if_ends_with_equals_sign() {
    local CURRENT_WORD_UP_TO_CURSOR=$2
    local suggestions
    mapfile -t suggestions < <(compgen -W "$2" -- "$1")
    for suggestion in "${suggestions[@]}"; do
        case $suggestion in
            *=)
                COMPREPLY+=("$suggestion")
                ;;
            *)
                COMPREPLY+=("$suggestion ")
                ;;
        esac
    done
}

# Takes the word under the cursor (just the portion up to the cursor)
# and completes with files in the current directory with some nicer
# formatting (e.g. directories have a slash added to the end of their
# names).
add_reply_files() {
    local suggestions
    mapfile -t suggestions < <(compgen -A file -- "$1")
    for suggestion in "${suggestions[@]}"; do
	COMPREPLY+=("$suggestion ")
    done
}

# Wraps its arguments in named arguments for passing to a reentrant query
wrap_command_line_for_reentrant_query() {
    local REENTRANT_AUTOCOMPLETION_COMMAND_LINE_NAME="__reentrant-autocompletion-command-line"
    for word in "$@"; do
	printf " --%s=\"$word\"" $REENTRANT_AUTOCOMPLETION_COMMAND_LINE_NAME
    done
}

complete_foo_--root() {
    local current_word_up_to_cursor=$2
    comp_words_traverse_advance_if_equals_sign
    if comp_words_traverse_is_past_cursor; then
	return $STATUS_ERROR_WORD_INDEX_PAST_CURSOR
    fi
    if comp_words_traverse_is_at_cursor; then
	#add_reply_files "$current_word_up_to_cursor"
	add_reply_fixed "$current_word_up_to_cursor" "foo bar"
	return $STATUS_DONE
    else
	:
    fi
}


complete_foo_bisect_start() {
    local current_word_up_to_cursor=$2
    while true; do
	if comp_words_traverse_is_past_cursor; then
	    return $STATUS_ERROR_WORD_INDEX_PAST_CURSOR
	fi
	if comp_words_traverse_is_at_cursor; then
	    add_reply_files "$current_word_up_to_cursor"
	    return $STATUS_DONE
	else
	    local current_word status
	    current_word=$(comp_words_traverse_get_current)
	    status=$?
	    if [ "$status" -ne 0 ]; then
		return $status
	    fi
	    comp_words_traverse_advance
	    case $current_word in
		*)
		    ;;
	    esac
	fi
    done
}
complete_foo_bisect_stop() {
    local current_word_up_to_cursor=$2
    while true; do
	if comp_words_traverse_is_past_cursor; then
	    return $STATUS_ERROR_WORD_INDEX_PAST_CURSOR
	fi
	if comp_words_traverse_is_at_cursor; then
	    add_reply_files "$current_word_up_to_cursor"
	    return $STATUS_DONE
	else
	    local current_word status
	    current_word=$(comp_words_traverse_get_current)
	    status=$?
	    if [ "$status" -ne 0 ]; then
		return $status
	    fi
	    comp_words_traverse_advance
	    case $current_word in
		*)
		    ;;
	    esac
	fi
    done
}

complete_foo_bisect() {
    local current_word_up_to_cursor=$2
    while true; do
	if comp_words_traverse_is_past_cursor; then
	    return $STATUS_ERROR_WORD_INDEX_PAST_CURSOR
	fi
	if comp_words_traverse_is_at_cursor; then
	    add_reply_fixed "$current_word_up_to_cursor" "start stop"
	    return $STATUS_DONE
	else
	    local current_word status
	    current_word=$(comp_words_traverse_get_current)
	    status=$?
	    if [ "$status" -ne 0 ]; then
		return $status
	    fi
	    comp_words_traverse_advance
	    case $current_word in
		start)
		    complete_foo_bisect_start "$1" "$2" "$3"
		    return $?
		    ;;
		stop)
		    complete_foo_bisect_stop "$1" "$2" "$3"
		    return $?
		    ;;
		*)
		    ;;
	    esac
	fi
    done
}

complete_foo_log() {
    add_reply_files "$2"
}

complete_foo() {
    local current_word_up_to_cursor=$2
    while true; do
	if comp_words_traverse_is_past_cursor; then
	    return $STATUS_ERROR_WORD_INDEX_PAST_CURSOR
	fi
	if comp_words_traverse_is_at_cursor; then
	    add_reply_no_space_if_ends_with_equals_sign "$current_word_up_to_cursor" "config checkout commit log bisect --help --root="
	    return $STATUS_DONE
	else
	    local current_word status
	    current_word=$(comp_words_traverse_get_current)
	    status=$?
	    if [ "$status" -ne 0 ]; then
		return $status
	    fi
	    comp_words_traverse_advance
	    case $current_word in
		bisect)
		    complete_foo_bisect "$1" "$2" "$3"
		    return $?
		    ;;
		log)
		    complete_foo_log "$1" "$2" "$3"
		    return $?
		    ;;
		--root)
		    complete_foo_--root "$1" "$2" "$3"
		    status=$?
		    if [ "$status" -ne 0 ]; then
			return $status
		    fi
		    ;;
	    esac
	fi
    done
}

complete_foo_entry() {
    # Set nospace so that completion doesn't add a space after
    # completing a word. In cases where the space is needed, it will
    # be added manually.
    compopt -o nospace
    comp_words_traverse_init
    if [ "$COMP_CWORD" == "0" ]; then
	# The shell only runs this completion script when the first
	# word of the command line is known, since the first word is
	# used to determine which completion script to run.
	:
    elif [ "$(comp_words_count)" -lt 2 ]; then
	# This should never happen as the arg stack should contain at
	# least the program name and the start of the first argument
	# (which will be the empty string if the user types the program
	# name followed by a space and then hits tab).
	:
    elif [ "$(comp_words_traverse_get_current)" != "$PROGRAM_NAME" ]; then
	# This should never happen as the first word in the arg list
	# should always be the program name.
	:
    else
	comp_words_traverse_advance
	complete_foo "$1" "$2" "$3"
	if status_is_error $?; then
	    :
	fi
    fi
}

complete -F complete_foo_entry $PROGRAM_NAME
