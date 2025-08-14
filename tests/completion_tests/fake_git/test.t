  $ ./fake_git.exe completions --program-name=fake-git --global-symbol-prefix=_fake_git_ > completion.sh
  $ x() { ../print_completions.sh ./completion.sh _fake_git_complete "$1" "$2"; }

Make a fake .git directory with some branches.
  $ mkdir -p .git/branches
  $ touch .git/branches/foo
  $ touch .git/branches/bar

  $ x "fake-git " \
  >   "         ^"
  bisect
  checkout
  commit
  help
  log

  $ x "fake-git checkout " \
  >   "                  ^"
  bar
  foo

  $ x "fake-git checkout foo " \
  >   "                      ^"
  .git
  completion.sh
  fake_git.exe

  $ x "fake-git checkout foo a.txt " \
  >   "                            ^"
  .git
  completion.sh
  fake_git.exe

  $ x "fake-git checkout foo a.txt " \
  >   "                  ^         "
  bar
  foo

  $ x "fake-git checkout foo a.txt " \
  >   "                      ^     "
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
  --pretty
  -h
  -p

  $ x "fake-git log --" \
  >   "               ^"
  --help
  --pretty

  $ x "fake-git log --pretty " \
  >   "                      ^"
  full
  fuller
  oneline
  short

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
  --pretty
  -h
  -p

Test that positional arguments and subcommands are both listed.
  $ x "fake-git bisect " \
  >   "                ^"
  bad
  good
  help
  reset
  start

  $ x "fake-git bisect -" \
  >   "                 ^"
  --help
  -h

  $ x "fake-git bisect start " \
  >   "                      ^"
  --help
  -h

Test the behaviour of arguments with no hints.
  $ x "fake-git commit --message " \
  >   "                          ^"
  .git
  completion.sh
  fake_git.exe

Test completion on files whose names contain spaces.
  $ mkdir dir
  $ touch "dir/a b c d" "dir/e f g h" "dir/a b c"

  $ x "fake-git commit dir/a" \
  >   "                     ^"
  dir/a b c
  dir/a b c d

  $ x "fake-git commit dir/" \
  >   "                    ^"
  dir/a b c
  dir/a b c d
  dir/e f g h

Test the completion of commands under help commands.
  $ x "fake-git h" \
  >   "           ^"
  help

  $ x "fake-git help " \
  >   "              ^"
  bisect
  checkout
  commit
  help
  log

  $ x "fake-git help commit -" \
  >   "                      ^"
  --help
  -h

  $ x "fake-git help bisect s" \
  >   "                      ^"
  start

  $ x "fake-git bisect help s" \
  >   "                      ^"
  start

  $ x "fake-git bisect help start -" \
  >   "                            ^"
  --help
  -h
