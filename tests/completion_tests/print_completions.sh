#!/usr/bin/env bash

# Usage: ./print_completions.sh COMPLETION_SCRIPT COMPLETION_FUNCTION COMMAND_LINE CURSOR_LINE
# Prints the completion suggestions if a user was to press TAB on a
# given command line with the cursor at the position indicated by
# CURSOR_LINE. CURSOR_LINE is expected to be a string where the first
# non-space character is at the position of the cursor. This is so
# that test scripts can be formatted with COMMAND_LINE and CURSOR_LINE
# on separate lines to make it visually clear which character of the
# command line is under the cursor.
# COMPLETION_SCRIPT is the name of a script that defines bash
# completion rules, and COMPLETION_FUNCTION is the name of the
# function defined in that script registered with `complete`.

set -u

# Fake compopt function as the real compopt would complain about being called
# outside of the context of a completion function otherwise.
compopt() { true; }

# Print the completion suggestions of a given completion function on
# some input and a cursor position.
#
# $1 is the name of the completion function to test
#
# $2 is a command line that will be passed to the completion function
#
# $3 is a string indicating the cursor position with the first
# non-space character. This lets tests be formatted with the cursor
# string directly beneath the command line, so the non-space character
# is lined up with the character in the command line where the cursor
# is located.
completion_test() {
    local completion_function="$1"
    local COMP_LINE="$2"
    local cursor_line="$3"
    read -r -a COMP_WORDS <<< "$COMP_LINE"
    if [ ${#COMP_WORDS[@]} == 1 ]; then
	COMP_WORDS+=("")
    fi
    local first_word="${COMP_WORDS[0]}"
    COMP_CWORD=0
    local length=${#cursor_line}
    local curr_word=""
    local prev_word=""
    local prev_char=" "
    for ((i = 0; i < length; i++)); do
	local cursor_char="${cursor_line:i:1}"
	if [ "$cursor_char" != " " ]; then
	    break
	fi
	local char="${COMP_LINE:i:1}"
	if [ "$char" != " " ]; then
	    curr_word="$curr_word$char"
	elif [ "$char" == " " ] && [ "$prev_char" != " " ]; then
	    COMP_CWORD=$((COMP_CWORD + 1))
	    prev_word=$curr_word
	    curr_word=""
	fi
	prev_char=$char
    done
    COMPREPLY=()
    $completion_function "$first_word" "$curr_word" "$prev_word"
    printf "%s\n" "${COMPREPLY[@]}" | grep -v "^\.cram" | sort
}

main () {
    local completion_script="$1"
    local completion_function="$2"
    local COMP_LINE="$3"
    local cursor_line="$4"
    . "$completion_script"
    completion_test "$completion_function" "$COMP_LINE" "$cursor_line"
}

main "$1" "$2" "$3" "$4"
