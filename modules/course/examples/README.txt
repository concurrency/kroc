
========================
What's in this directory
========================

These files are all main compilation units:

  hello_raw_world.occ

    This just outputs the expected message to stdout.  It uses no
    libraries and some very primitive occam.

  hello_seq_world.occ

    This also outputs the expected message.  However, it declares
    the message as a string constant and uses a simple SEQ-loop
    (which corresponds, loosely, to a for-loop in C/Java) to output
    the characters in sequence.  It also uses no libraries.

  hello_world.occ

    This uses a library procedure ("out.string") to output the string.
    The library is introduced by the first line in the file:

      #INCLUDE "course.module"

    Documentation (HTML) on all the modules provided by kroc is included
    in the release - and is on-line at:

      http://occam-pi.org/occamdoc/frames.html

  double_echo.occ

    This is a simple process to echo characters from stdin to stdout.
    To make it slightly interesting, each character is echoed twice.
    Note the use of the FLUSH character (which is defined in the
    #INCLUDEd file consts.inc - see the kroc/course/libsrc directory).
    The process terminates after echoing the character 'Z'.

  casting.occ

    This demonstrates the rules for casting between types.

  test_utils.occ

    This is a process that shows how to use all the procedures and
    functions provided by the utils.occ library (whose source code
    and documentation are in the kroc/course/libsrc directory).

  test_bell.occ

    This is a process showing the effect (if any) of sending the BELL
    character to the screen and error channels.  It also illustrates
    the need to FLUSH incomplete lines on the screen channel, but not
    for the error channel.  Unfortunately, some versions of the
    Transterpreter operate the error channel in the same way as the
    screen and, so, the error channel *may* need flushing.
    Use this program to check!

  demo.occ

    This process will be explained in the course.  It demonstrates
    the `legoland' components defined in the demo_cycles.occ and
    demo_nets libraries (sources in kroc/course/libsrc).

  sort_pump.occ

    This process will be explained in the course.  It demonstrates
    the functionality of a parallel `sort pump', viewed as a black box.

  sort_inside.occ

    This process will be explained in the course.  It demonstrates
    the internal workings of the parallel sort pump.  This version
    is a little artificial so that, for easy viewing, the flow of data
    through the pump is constrained by the way the cell reports are
    processed: the cells are lock-stepped by a report from *every*
    cell being demanded every cycle.

  sort_inside_2.occ

    This is a more realistic version of the above.  Cell reports are
    processed individually whenever made, so the cells can cycle
    independently (not in lock-step).  A delay process (that holds
    up each passing number by half a second) is placed at the end
    of the sort pump, as part of the test-rig.  If you can type
    characters faster than 2 per second, you will see the sort pump
    fill up.  In a real application, data will be being supplied
    faster than the pump cells can cycle, so all will be busy all
    the time - which is the point of the exercise!

  commstime.occ

    This is a classic benchmark to measure the basic context-switch
    overhead (under favourable conditions - everything easilly fits
    into primary cache).

  bar.occ

    This is an animated example of fair ALTing.


Peter Welch.
(23-11-2011)
