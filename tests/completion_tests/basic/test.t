  $ ./basic.exe > completion.sh
  $ x() { ../print_completions.sh ./completion.sh _basic_complete "$1" "$2"; }

  $ x "basic " \
  >   "      ^"
  --bar
  --baz
  --foo
  --help
  --manpage
  -h

  $ x "basic --" \
  >   "        ^"
  --bar
  --baz
  --foo
  --help
  --manpage

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
  --manpage
  -h

  $ x "basic --foo --bar " \
  >   "                  ^"
  --bar
  --baz
  --foo
  --help
  --manpage
  -h

  $ x "basic --foo --bar" \
  >   "         ^       "
  --foo

  $ x "basic --bar --foo" \
  >   "         ^       "
  --bar
  --baz
