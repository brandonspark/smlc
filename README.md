# smlc

I couldn't be bothered to think of a better name.

This is a wrapper over the SML/NJ REPL, designed to mimic a "compiler extension"
for the SML/NJ compiler, that implements "C-like syntax". Currently, it can do
most of the things through the first 6 weeks of 150, minus datatypes.

It also has options for a few 150-related jokes. This was for my 150 interview.
Please consider my candidacy.

I'll bet you've never used Unix pipes in SML before.

# How do I use this?

To replicate the effect that I had, you need to run `sml sources.cm finish.sml
<flags> > /dev/null`. Obviously, this gives away the trick immediately, so the
actual way to do this is that you should alias (in your shell) the command `sml`
to instead run `sh top/mock.sh`, which adds in the extra arguments to where they
need to go.

The reason why you need to pipe into `/dev/null` is so that the actual output
from compiling `sources.cm` and `finish.sml` does not show up. This maintains
the illusion that the only thing happening is booting up the SML/NJ REPL.

`sources.cm` compiles the lexer and parser that are used to interpret and
translate the modified syntax. `finish.sml` runs the actual command-line
nonsense, as well as some systems-level stuff that completes the illusion.

You can see the various commands added via running `sml -h` (where `sml` is
aliased to the included Bash file). Alternatively, `sml --special-help`
describes some funny options.

Have fun!
