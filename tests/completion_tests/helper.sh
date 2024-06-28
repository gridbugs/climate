#!/usr/bin/env bash

set -u

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
    completion_function="$1"
    COMP_LINE="$2"
    cursor_line="$3"
    read -r -a COMP_WORDS <<< "$COMP_LINE"
}
