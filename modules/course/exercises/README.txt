
===================================
EXERCISES: what's in this directory
===================================

These are starter files for some of the exercises in the basic course.
You will need your own copies of all the `.occ' files in this directory.

  early-exercises.doc/pdf

    These define exercises 1 and 2, for which q1.occ and q2.occ below
    are starter files.

  q1.occ

    Simple plug-and-play components: two generators, a simple multiplexor
    (first from one stream, then the other), a filter and printer.

  q2.occ

    Use the `Legoland' processes to undo the integrate (i.e. differentiate).
  
  extra-exercises.doc/pdf

    This gives just one extra exercise that can be taken early.  See the
    e1.occ starter file below.
  
  e1.occ

    This is the starter file for the extra exercise above.  A simple
    component to merge two sorted ascending number streams has to be
    implemented.  This is then used to build mergers for three, four
    and *any* number of input streams - the latter needing recursion
    with the concurrency.

  more-exercises.doc/pdf

    These define exercises 3 and 4, for which q3.occ and q4.occ below
    are starter files.
  
  q3.occ

    Modify existing functionality by adding a parallel controller - not by
    modifying the existing component.

  q4.occ

    Create a test rig for the squares pipeline that shows all stages of
    processing.  Modify to allow user restting of its running components.

  q5.occ

    A type ahead buffer.

  q6.occ

    Names and numbers - an exercise using a variant PROTOCOL.  Solutions
    range from serial (with rich data structures) to highly parallel (with
    trivial data structures).  This uses the files `nn_data.txt',
    `nn_small.txt' and `nn_tiny.txt' for data.  It should generate results
    like those in `small_results.txt' or `small_results_sorted.txt'.

  q7.occ

    Animate the classic dining philosophers' college.

  santa-exercise.doc/pdf

    These present the classic Santa-Claus concurrency problem and
    invites various solutions in occam-pi.  There is no starter file!

  cylons.doc/pdf

    This is a simple exercise in robotics.  Your task is to program
    a better brain ('brain.1') for the green robots.  They must keep
    moving, cover as much ground as possible (don't just wander
    around in circles like the red robots ('brain.0') and avoid
    obstacles (walls and other robots).  There is some neat graphics,
    all programmed in occam-pi, but which need not be understood
    to complete the given exercise.  The exercise can be expanded
    in many ways.  Hundreds of autonomous robots can be exercised
    using KRoC compiled code - tens of robots with the Transterpreter.

  cylons.occ

    This is the starter file for the robots (Cylons) exercise.

  bugs.occ

    This is a process with some serious syntactic and semantic damage.
    Your task is to get it to work as its designers intended!


Peter Welch.
