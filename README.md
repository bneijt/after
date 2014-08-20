After
=====

![Travis CI build status](https://api.travis-ci.org/bneijt/after.svg)

Simple program that will exit when another program exits (by polling /proc).

It makes it easy to retroactively schedule a command after another command
has started (without using `Ctrl-Z`, `fg; new command`)


Usage

    sleep 1m &
    after sleep; echo Done sleeping

with `--pid` you can also use the pid:

    sleep 20 & BG=$!
    after --pid $BG; echo Done sleeping
