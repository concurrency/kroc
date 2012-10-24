
============================================================
EXAMPLES: notes on kroc compilation and occam main processes
============================================================


Compilation Units
-----------------

occam-pi *compilation units* consist of a list of declarations: constants,
types, protocols, processes ... but *not* variables or channels (at this
level, these only occur inside PROC declarations).

occam-pi files containing compilations units should have ".occ" as suffix
to their name.

Compilation units (e.g. "stuff.occ") may be sparately compiled with the
command:

  % kroc -c stuff.occ

Let's leave this for now - separate compilation and libraries are addressed
elsewhere.

*Main* compilation units, which can be compiled and linked into executables,
are compilation units whose last item is a *main* process (see below).  Main
compilation units (e.g. "bar.occ") are compiled and linked with the command:

  % kroc bar.occ

If there are no errors, this produces an executable file ("bar") which can
be run with the command:

  % ./bar

[Note: compiling and running under the Transterpreter is rather different!
Please check out <http://transterpreter.org/docs/get_started>.]


Main processes
--------------

This directory contains example *main* occam-pi compilation units.  The last
process listed in each is a *main* process.  Typically, these have the form:

  PROC foo (CHAN BYTE keyboard?, screen!, error!)

All three parameters do not have to be present.  Where used, they should
be included in the above order.  The first one is mapped to (UNIX) `stdin',
the second to `stdout' and the third to `stderr'.  No other parameters are
allowed.

[Note: currently for the Tranterpreter, all three of the above parameters
*do* have to be present - just ignore the compiler warnings if you don't
use them!]

[Note: for consistency (currently) between the Tranterpreter and kroc
run-times, the examples here always have all three parameters present.
Those using kroc may remove the ones not used.]

kroc runs its keyboard channel in `raw' mode without echoing - i.e.
individual keystrokes are supplied to the occam process immediately
(not buffered up until a carriage-return is typed).

kroc runs its screen channel in `line-buffered' mode - i.e. characters
are not normally delivered to the screen until a new-line is output.
To force immediate delivery, output the FLUSH byte (255) to the screen.

kroc runs its error channel in `raw' mode - i.e. characters are delivered
straight away, without any buffering.

If we need echoing of keyboard input, the process must be programmed to do
that itself, by explicitly outputting the character to the screen channel.

[Note: all the above is also true for the Transterpreter run-time.]

Note that the identifier names `foo', `keyboard', `screen' and `error'
in the above main PROC declaration are user-chosen.  Just as legal is:

  PROC bar (CHAN BYTE stdin?, stdout!, stderr!)

Choose names that are meaningful for you.

See the README.txt file for the contents of this directory.

Take your own copies of all these files.  Once in your directory space,
you may compile any of these by applying `kroc' - e.g.

  % kroc bar.occ

which produces an executable called sort_inside.  Run this - i.e.

  % ./bar

and you are in business!

[Note: compiling and running under the Transterpreter is rather different!
Please check out <http://transterpreter.org/docs/get_started>.]


Peter Welch.
(23-11-2011)
