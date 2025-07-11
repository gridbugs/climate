  $ ./fake_git.exe completions \
  > --program-name=fake-git \
  > --global-symbol-prefix=_fake_git_ \
  > --no-comments \
  > --no-whitespace \
  > --minify-global-names \
  > --minify-local-variables \
  > --optimize-case-statements \
  > --no-command-hash-in-function-names > completion.sh
  $ x() { ../print_completions.sh ./completion.sh _fake_git_complete "$1" "$2"; }

Make a fake .git directory with some branches.
  $ mkdir -p .git/branches
  $ touch .git/branches/foo
  $ touch .git/branches/bar

  $ x "fake-git " \
  >   "         ^"
  checkout
  log
  bisect
  commit

  $ x "fake-git checkout " \
  >   "                  ^"
  bar
  foo

  $ x "fake-git checkout foo " \
  >   "                      ^" | sort
  .git
  completion.sh
  fake_git.exe

  $ x "fake-git checkout foo a.txt " \
  >   "                            ^" | sort
  .git
  completion.sh
  fake_git.exe

  $ x "fake-git checkout foo a.txt " \
  >   "                  ^         "
  bar
  foo

  $ x "fake-git checkout foo a.txt " \
  >   "                      ^     " | sort
  .git
  completion.sh
  fake_git.exe

  $ x "fake-git log " \
  >   "             ^"
  bar
  foo

  $ x "fake-git log -" \
  >   "              ^"
  --help
  --manpage
  --pretty
  -h
  -p

  $ x "fake-git log --" \
  >   "               ^"
  --help
  --manpage
  --pretty

  $ x "fake-git log --pretty " \
  >   "                      ^"
  full
  fuller
  short
  oneline

  $ x "fake-git log --pretty f" \
  >   "                       ^"
  full
  fuller

  $ x "fake-git log --pretty full " \
  >   "                           ^"
  bar
  foo


  $ x "fake-git log --pretty full foo " \
  >   "                               ^"
  --help
  --manpage
  --pretty
  -h
  -p

Test that positional arguments and subcommands are both listed.
  $ x "fake-git bisect " \
  >   "                ^"
  start
  reset
  good
  bad

  $ x "fake-git bisect -" \
  >   "                 ^"
  --help
  --manpage
  -h

  $ x "fake-git bisect start " \
  >   "                      ^"
  --help
  --manpage
  -h

Test the behaviour of arguments with no hints.
  $ x "fake-git commit --message " \
  >   "                          ^" | sort
  .git
  completion.sh
  fake_git.exe

Test completion on files whose names contain spaces.
  $ mkdir dir
  $ touch "dir/a b c d" "dir/e f g h" "dir/a b c"

  $ x "fake-git commit dir/a" \
  >   "                     ^"
  dir/a b c d
  dir/a b c

  $ x "fake-git commit dir/" \
  >   "                    ^"
  dir/a b c d
  dir/a b c
  dir/e f g h
