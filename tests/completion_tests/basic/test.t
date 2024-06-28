  $ pwd
  $ tree
  $ tree ..
  $ echo $SHELL
  $ $SHELL --version
  $ . ../helper.sh
  $ ./main.exe > completion.sh
  $ . ./completion.sh
  $ x() { completion_test _basic_complete "$1" "$2"; }

  $ x "basic " \
  >   "      ^" 
  --bar
  --baz
  --foo
  --help
  -h

  $ x "basic --" \
  >   "        ^"
  --bar
  --baz
  --foo
  --help

  $ x "basic --b" \
  >   "         ^"
  --bar
  --baz

  $ x "basic --ba" \
  >   "           ^"
  --bar
  --baz

  $ x "basic --bar" \
  >   "           ^" 
  --bar

  $ x "basic --bar " \
  >   "            ^"
  --bar
  --baz
  --foo
  --help
  -h

  $ x "basic --foo --bar " \
  >   "                  ^"
  --bar
  --baz
  --foo
  --help
  -h
  $ x "basic --foo --bar" \
  >   "         ^       "
  --foo

  $ x "basic --bar --foo" \
  >   "         ^       " 
  --bar
  --baz
