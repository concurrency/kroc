.\" groff -ms o2cl.ms > o2cl.ps
.\" groff -mtty-char -P '-cbu' -ms -Tascii o2cl.ms >o2cl.txt

.ND
.nr PI 1m
.de BS
.in +0.18i
.nr LL -0.30i
.LP
.sp 0.1v
..
.de BE
.nr LL +0.30i
.in 0
.sp 0.3v
.LP
..
.ds dg "\v'-0.3v'\s-1\(dg\s+1\v'0.3v'
.tr _\(ru
.nr LL 15.6c
.nr PL +0.80i
.pl +0.80i
.nr HY 0
.hy 0
.po +0.10i
.LP
.sp 0.5
.ft B
.nr PS 24
.ps 24
.nr VS 14
.vs 14
.LP
.sv 0.6c
.ce
An \fHoccam-\fP\(*p Checklist \s-4(\fHoccam2.1\fP subset)\s0
.nr PS 12
.ps 12
.nr VS 14
.vs 14
.sp
.ce 2
.I
Copyright \(co 2006 P.H.Welch \f(CR<P.H.Welch@kent.ac.uk>\fP
P.H.Welch, Computing Laboratory, University of Kent at Canterbury, CT2 7NF.
.R
.LP
.sp 0.3v
.NH 1
Introduction
.NH 2
Top-Down View
.LP
An \fIoccam\fP program is a \s-1\&\f(CO<process>\fR\s+1.
.LP
A \s-1\&\f(CO<process>\fR\s+1 is a (possibly null) sequence of \s-1\&\f(CO<declarations>\fR\s+1
followed by  an \s-1\&\f(CO<executable>\fR\s+1.
Each \s-1\&\f(CO<declaration>\fR\s+1 starts on a separate line
and at the same level of indentation.
The \s-1\&\f(CO<executable>\fR\s+1 also starts at this same level of
indentation.  The scope of any \s-1\&\f(CO<declaration>\fR\s+1 covers only those
\s-1\&\f(CO<declarations>\fR\s+1 and the \s-1\&\f(CO<executable>\fR\s+1 that follow it.
.LP
An \s-1\&\f(CO<executable>\fR\s+1 is either a \s-1\&\f(CO<primitive.process>\fR\s+1
or a \s-1\&\f(CO<structured.process>\fR\s+1.
.NH 2
Bottom-Up View
.LP
\fIoccam\fR processes \s-1\&\f(CRINT\fR\s+1s (16, 32 or 64 bits, depending on the word-length of
the target processor), \s-1\&\f(CRINT16\fR\s+1s (16 bits, regardless of the word-length of
the target processor), \s-1\&\f(CRINT32\fR\s+1s (32 bits, also regardless ...),
\s-1\&\f(CRINT64\fR\s+1s (64 bits, also regardless ...), \s-1\&\f(CRBYTE\fR\s+1s, (8 bit unsigned
numbers \(em i.e. 0..255), \s-1\&\f(CRBOOL\fR\s+1s and
ANSI/IEEE-754 standard floating-point
numbers in two precisions \(em \s-1\&\f(CRREAL32\fR\s+1 and \s-1\&\f(CRREAL64\fR\s+1.  Variables,
channels and expressions are all typed and strong checking rules are
enforced that do not allow inconsistent mixes.  Character literals
(in single quotes) are treated as \s-1\&\f(CRBYTE\fR\s+1s and character strings (in double
quotes) as fixed length \s-1\&\f(CRBYTE\fR\s+1 arrays.  The following operators are defined for
expressions:
.DS
.ta 25n
\s-1\&\f(CR+, \-, *, /, \e\fR\s+1	(operands \s-1\&\f(CRINT\f(COxx\fR\s+1 or \s-1\&\f(CRBYTE\fR\s+1, result \s-1\&\f(CRINT\f(COxx\fR\s+1 or \s-1\&\f(CRBYTE\fR\s+1)
\s-1\&\f(CRPLUS, MINUS, TIMES\fR\s+1	(operands \s-1\&\f(CRINT\f(COxx\fR\s+1 or \s-1\&\f(CRBYTE\fR\s+1, result \s-1\&\f(CRINT\f(COxx\fR\s+1 or \s-1\&\f(CRBYTE\fR\s+1)
\s-1\&\f(CRAFTER\fR\s+1	(operands \s-1\&\f(CRINT\f(COxx\fR\s+1, result \s-1\&\f(CRBOOL\fR\s+1)
\s-1\&\f(CR+, \-, *, /, \e\fR\s+1	(operands \s-1\&\f(CRREAL\f(COxx\fR\s+1, result \s-1\&\f(CRREAL\f(COxx\fR\s+1)
\s-1\&\f(CR<, <=, >, >=\fR\s+1	(operands \s-1\&\f(CRINT\f(COxx\fR\s+1, \s-1\&\f(CRREAL\f(COxx\fR\s+1 or \s-1\&\f(CRBYTE\fR\s+1, result \s-1\&\f(CRBOOL\fR\s+1)
\s-1\&\f(CRAND, OR, NOT\fR\s+1	(operands \s-1\&\f(CRBOOL\fR\s+1, result \s-1\&\f(CRBOOL\fR\s+1)
\s-1\&\f(CR=, <>\fR\s+1	(operands same type, result \s-1\&\f(CRBOOL\fR\s+1)
\s-1\&\f(CR/\e, \e/, \(ap, ><\fR\s+1	(operands \s-1\&\f(CRINT\f(COxx\fR\s+1 or \s-1\&\f(CRBYTE\fR\s+1, result \s-1\&\f(CRINT\f(COxx\fR\s+1 or \s-1\&\f(CRBYTE\fR\s+1)
\s-1\&\f(CR<<, >>\fR\s+1	(operands \s-1\&\f(CRINT\f(COxx\fR\s+1 or \s-1\&\f(CRBYTE\fR\s+1, result \s-1\&\f(CRINT\f(COxx\fR\s+1 or \s-1\&\f(CRBYTE\fR\s+1,
	\ where 0 <= right operand < 64)
.DE
\fBWarning\fP: there is no precedence ordering for operators nor any
`left-to-right' evaluation rule \(em use brackets!
.LP
\fBNote:\fP infix operators with \s-1\&\f(CRINT\f(COxx\fR\s+1 or \s-1\&\f(CRREAL\f(COxx\fR\s+1 operands must
have the same precision (i.e. 16, 32 or 64 bits), as well as the same type,
on each side.
They return values of that same precision and type.
.LP
\fBNote:\fP to convert between types/precisions, just use the target type/precision
name as a prefix operator on the source type/precision value \(em see section 11.5.
.LP
\fBNote:\fP \s-1\&\f(CRPLUS\fR\s+1, \s-1\&\f(CRMINUS\fR\s+1, \s-1\&\f(CRTIMES\fR\s+1 and \s-1\&\f(CRAFTER\fR\s+1
are best applied to time values \(em see sections 2.4, 3.4, 3.5 and 4.3.
Time cycles through the complete range of \s-1\&\f(CRINT\fR\s+1s.  If we
\s-1\&\f(CRPLUS\fR\s+1 one to the most positive \s-1\&\f(CRINT\fR\s+1, we
get the most negative \s-1\&\f(CRINT\fR\s+1 \(em not an overflow error.
.NH 1
Declarations
.NH 2
Value Abbreviation (constants)
.LP
.DS
.ta 50m
\s-1\&\f(CRVAL INT max IS 50:
VAL INT double.max IS 2*max:	\(mi\(mi constant "folding"
VAL BYTE first.letter IS \(fmA\(fm:	\(mi\(mi character literals are BYTEs
VAL []BYTE hello IS "Hello World*c*n":	\(mi\(mi string literals are BYTE arrays
VAL INT control IS #1F:	\(mi\(mi a hexadecimal literal
VAL BYTE bell.char IS BYTE (control /\e\ (INT \(fmG\(fm)):	\(mi\(mi note type conversions
VAL [8]INT masks IS [#01, #02, #04, #08, #10, #20, #40, #80]:	\(mi\(mi the 8 was optional\fR\s+1
.DE
.LP
\fBNote:\fP these are a special form of the more general \fIoccam\fP concept
of abbreviation \(em see section 8.
.NH 2
Variable Declaration
.LP
.DS
\s-1\&\f(CRINT a, b:
[max]INT c:
[double.max][max]BYTE d:\fR\s+1
.DE
\fRThe array \s-1\&\f(CRc\fR\s+1 has \s-1\&\f(CRmax\fR\s+1 elements indexed \s-1\&\f(CRc[0]\fR\s+1 to
\s-1\&\f(CRc[max\-1]\fR\s+1 inclusive.  The declared size of an array must be a
constant.  The variables \s-1\&\f(CRa\fR\s+1 and \s-1\&\f(CRb\fR\s+1 and elements of \s-1\&\f(CRc\fR\s+1 are
\s-1\&\f(CRINT\fR\s+1 quantities.  The array \s-1\&\f(CRd\fR\s+1 has \s-1\&\f(CRdouble.max\fR\s+1 elements
indexed \s-1\&\f(CRd[0]\fR\s+1 to \s-1\&\f(CRd[99]\fR\s+1.  The elements of \s-1\&\f(CRd\fR\s+1
are \s-1\&\f(CR[max]BYTE\fR\s+1 arrays.
The byte \s-1\&\f(CRd[a][b]\fR\s+1 is the \s-1\&\f(CRb\fR\s+1th. element of the
\s-1\&\f(CRa\fR\s+1th. element of \s-1\&\f(CRd\fR\s+1.
.NH 2
Channel Declaration
.DS
\s-1\&\f(CRCHAN BYTE p:
[200]CHAN INT q:\fR\s+1
.DE
.R	\" don't ask me why this is necessary
Here, \s-1\&\f(CRp\fR\s+1 is a channel carrying \s-1\&\f(CRBYTE\fR\s+1 values
and \s-1\&\f(CRq\fR\s+1 is an array of 200 \s-1\&\f(CRINT\fR\s+1 channels
indexed \s-1\&\f(CRq[0]\fR\s+1 to \s-1\&\f(CRq[199]\fR\s+1 inclusive.
.NH 2
Timer Declaration
.LP
.DS
\s-1\&\f(CRTIMER tim:\fR\s+1
.DE
To access the clock, \s-1\&\f(CRINT\fR\s+1 values need to be input from
a \s-1\&\f(CRTIMER\fR\s+1 (see Section 3.4).
Any number of \s-1\&\f(CRTIMER\fR\s+1s may be declared \(em they
all go at the same speed (one tick per microsecond is the default),
but they are not obliged to show the same time.
.NH 2
Process Abstraction
.LP
.DS
.ta 13n
\s-1\&\f(CRPROC foo (VAL []BYTE s, VAL BOOL mode, INT result,
          CHAN INT in?, out!, CHAN BYTE pause?)
  \&\f(CO<process>
\&\f(CR:\fR\s+1
.DE
\s-1\&\f(CRVAL\fR\s+1 \s-1\&\f(CO<type>\fR\s+1 \s-1\&\f(CO<formal>\fR\s+1 parameters are similar to value parameters in
\fIPascal\fP \(em except that they are treated as constants within the
\s-1\&\f(CO<process>\fR\s+1 body and may not be altered.
\s-1\&\f(CO<type>\fR\s+1 \s-1\&\f(CO<formal>\fR\s+1 parameters are similar to reference
(i.e. \s-1\&\f(CRvar\fR\s+1)
parameters in \fIPascal\fP.
.bp
.LP
\fBNote:\fP \fIoccam\fP parameter mechanisms are formally defined in terms of
the \fIoccam\fP abbreviation concept \(em section 8.  Aliasing of item names
cannot be introduced through parameters (as can happen in \fIJava\fP,
\fIPascal\fP, \fIC\fP, \fIFORTRAN\fP, \fIAda\fP, \fIModula-2\fP, ...).
.LP
The \s-1\&\f(CO<process>\fR\s+1 body of the \s-1\&\f(CRPROC\fR\s+1 is as
defined in section 1.1 above \(em i.e. it can have its own (private)
declarations. 
This body must be indented \fIexactly two spaces\fP from
the \s-1\&\f(CRPROC\fR\s+1 line.
Global names may be accessed directly from the body \(em but please,
of course, avoid global variables and channels.  Parameter channels
must be used exclusively for input or exclusively for output within the body
(their direction is defined by the \s-1\&\f(CR?\fR\s+1 or \s-1\&\f(CR!\fR\s+1
symbol after their name in the parameter list).
In \s-1\&\f(CRfoo\fR\s+1, the parameter \s-1\&\f(CRs\fR\s+1 may be passed
either an a \s-1\&\f(CRBYTE\fR\s+1 array \fIreference\fP
(like \s-1\&\f(CRd[a]\fR\s+1)
or a \s-1\&\f(CRBYTE\fR\s+1 array \fIvalue\fP
(like \s-1\&\f(CRhello\fR\s+1);
its parameter \s-1\&\f(CRresult\fR\s+1 may only be passed
an \s-1\&\f(CRINT\fR\s+1 \fIreference\fP (like the variable
\s-1\&\f(CRa\fR\s+1, in which the result of any assignments
made by \s-1\&\f(CRfoo\fR\s+1 to \s-1\&\f(CRresult\fR\s+1 will be left).
\s-1\&\f(CRPROC\fR\s+1s may be used to create autonomous processes
(running in parallel with each other) \fIor\fR for traditional procedure calls.
.\".LP
.\"\fBWarning\fP: \s-1\&\f(CRPROC\fR\s+1s may not have recursive definitions \(em sorry!
.NH 1
Primitive Processes
.NH 2
Assignment Process
.LP
.DS
\s-1\&\f(CRa := c[2] + b\fR\s+1
.DE
.LP
\fIoccam\fR expressions have no side-effects!  Because of the anti-aliasing laws
(see section 8), 
different names in the same scoping context always refer to
different items \(em no nasty surprises!
This gives us \fIreferential transparency\fP, a posh name for 
\fIwhat-you-see-is-what-you-get\fP.
Such good behaviour is not displayed by \fIany\fP of the traditional
languages for procedural programming (\fIJava\fP, \fIPascal\fP,
\fIC\fP, \fIFORTRAN\fP, \fIAda\fP, \fIModula-2\fP, ...)
and is a major cause of hard-to-find and serious error.
.NH 2
Input Process
.LP
.DS
\s-1\&\f(CRin ? a\fR\s+1
.DE
\fRThis process is suspended until a message arrives on channel \s-1\&\f(CRin\fR\s+1.  It then inputs
the contents into the variable \s-1\&\f(CRa\fR\s+1 (whose type, of course,
must match that of the channel).
.NH 2
Output Process
.LP
.DS
\s-1\&\f(CRout ! a + (2 * b)\fR\s+1
.DE
\fRThe expression is evaluated and output down channel \s-1\&\f(CRout\fR\s+1
(whose type, of course, must match that of the expression).
This process is then suspended until the message is accepted by the
receiving process (at the other end of the channel).
.LP
Every communication between parallel processes uses this
synchronisation mechanism \(em sometimes called a `rendezvous'.
.NH 2
Timer Process
.LP
.DS
\s-1\&\f(CRtim ? t\fR\s+1
.DE
The result of
this process is that the value of the timer \s-1\&\f(CRtim\fR\s+1 is assigned
to the variable \s-1\&\f(CRt\fR\s+1 (which must be an \s-1\&\f(CRINT\fR\s+1).
.LP
Although the syntax for reading them is similar, timers are different from
channels.
Channel input is \fIsynchronised\fP \(em we have to wait for the message
to arrive!  Data must not get lost during channel communication!!
.LP
On the other hand,
timers behave like \fIun-synchronised\fP `channels' \(em when we read
the time, we just want the latest value.  We don't want to have to
wait for the clock to tick!  Data gets lost \(em all those
time-values we didn't read \(em but that's OK for this kind of device.
We don't want the clock to stop just because we don't look at it!!
.NH 2
Timeout Process
.LP
.DS
\s-1\&\f(CRtim ? AFTER t\fR\s+1
.DE
\fRThis process is suspended until the value on the \s-1\&\f(CRtim\fR\s+1 timer
(always available and increasing)
gets \s-1\&\f(CRAFTER\fR\s+1 the value of \s-1\&\f(CRt\fR\s+1.
Note that \s-1\&\f(CRt\fR\s+1 may be any \s-1\&\f(CRINT\fR\s+1 expression, but
don't forget to use \s-1\&\f(CRPLUS\fR\s+1 and \s-1\&\f(CRMINUS\fR\s+1
when working with time values (rather than \s-1\&\f(CR\+\fR\s+1 and \s-1\&\f(CR\-\fR\s+1).
.NH 2
Null Process
.LP
.DS
\s-1\&\f(CRSKIP\fR\s+1
.DE
\fRThis process immediately terminates.  It's needed
because sometimes the syntax requires a process and we have nothing
to do.  Of course, this process generates no object code!
.NH 2
Suspend Process
.LP
.DS
\s-1\&\f(CRSTOP\fR\s+1
.DE
\fRThis process is suspended and never resumed \(em needed for semantic
completeness and panic situations.
.NH 1
Structured Processes
.NH 2
Sequential Processes
.LP
.DS
\s-1\&\f(CRSEQ
  \&\f(CO<process.0>
  <process.1>
  <process.2>\fR\s+1
.DE
\fRThe \s-1\&\f(CO<processes>\fR\s+1 are as described in
1.1 (i.e. may have private declarations)
and must be indented exactly two spaces further than the
\s-1\&\f(CRSEQ\fR\s+1 keyword.  The extent of the \s-1\&\f(CRSEQ\fR\s+1 construct is finished when
the indentation level returns to the same as (or less than) that of the
\s-1\&\f(CRSEQ\fR\s+1.
.LP
The construct means that \s-1\&\f(CO<process.0>\fR\s+1, \s-1\&\f(CO<process.1>\fR\s+1 and
\s-1\&\f(CO<process.2>\fR\s+1 are
to be executed in that order.  Any channel used within a \s-1\&\f(CRSEQ\fR\s+1 construct
must be used exclusively for input or exclusively for output.
.NH 2
Parallel Processes
.LP
.DS
\s-1\&\f(CRPAR
  \&\f(CO<process.0>
  <process.1>
  <process.2>\fR\s+1
.DE
\fRThe indentation and extent rules are as for the \s-1\&\f(CRSEQ\fR\s+1 above.
The construct means that \s-1\&\f(CO<process.0>\fR\s+1,
\s-1\&\f(CO<process.1>\fR\s+1 and \s-1\&\f(CO<process.2>\fR\s+1
are to be executed in parallel.  
This construct does not terminate until all its component processes have
terminated.
The order in which the \s-1\&\f(CO<processes>\fR\s+1 are listed is irrelevant.
.bp
.LP
Parallel processes may not assign or input
to shared data (but they may inspect shared data).  Parallel processes may not
share input channels.  Parallel processes may not share output channels.
The effect of these rules is that parallel processes can only influence each
other by communicating along dedicated point-to-point channels.
So long as there are no \s-1\&\f(CRALT\fR\s+1s (see section 4.3) present,
their semantics \(em i.e. the services they provide to, and demand from,
their environment \(em 
are completely deterministic, regardless of internal scheduling patterns or
relative processor speed (when running on multiple processors).
.NH 2
Non-Deterministic Process
.LP
.DS
\s-1\&\f(CRALT
  \&\f(CO<guard.0>
    <process.0>
  <guard.1>
    <process.1>
  <guard.2>
    <process.2>\fR\s+1
.DE
\fREach \s-1\&\f(CO<guard>\fR\s+1 is indented two spaces and its associated
\s-1\&\f(CO<process>\fR\s+1 is indented a further two spaces.  We shall refer to
such a pair as a \s-1\&\f(CO<guarded.process>\fR\s+1.  The \s-1\&\f(CRALT\fR\s+1 construct is
suspended until one or more of the \s-1\&\f(CO<guards>\fR\s+1 becomes `ready' (see
below).  One of the `ready' \s-1\&\f(CO<guards>\fR\s+1 (chosen \fIarbitrarily\fP) is then
executed followed by its associated \s-1\&\f(CO<process>\fR\s+1.  The order in which
these \s-1\&\f(CO<guarded.processes>\fR\s+1 are listed is irrelevant.
.LP
A \s-1\&\f(CO<guard>\fR\s+1 is either a \s-1\&\f(CO<simple.guard>\fR\s+1 or a \s-1\&\f(CO<pre.conditioned.guard>\fR\s+1.
.LP
A \s-1\&\f(CO<simple.guard>\fR\s+1 is most commonly an \s-1\&\f(CO<input.guard>\fR\s+1:
.DS
  \s-1\&\f(CRin ? x\fR\s+1
.DE
This becomes `ready' if a message is pending on the channel.  Its execution
is to input the message.
.LP
Alternatively, a \s-1\&\f(CO<simple.guard>\fR\s+1 may be a \s-1\&\f(CO<time.out.guard>\fR\s+1:
.DS
  \s-1\&\f(CRtim ? AFTER absolute.time.out\fR\s+1
.DE
This becomes `ready' when the value from the \s-1\&\f(CRtim\fR\s+1 timer
(always available and increasing) becomes \s-1\&\f(CRAFTER\fR\s+1 the \s-1\&\f(CRabsolute.time.out\fR\s+1
value.  Its execution is null.
.LP
Thirdly, a \s-1\&\f(CO<simple.guard>\fR\s+1 may be a \s-1\&\f(CO<null.guard>\fR\s+1:
.DS
  \s-1\&\f(CRSKIP\fR\s+1
.DE
This is always `ready' and has a null execution.
.LP
Finally, a \s-1\&\f(CO<pre.conditioned.guard>\fR\s+1 has the form:
.DS
  \s-1\&\f(CO<pre.condition> \&\f(CR& \&\f(CO<simple.guard>\fR\s+1
.DE
where the \s-1\&\f(CO<pre.condition>\fR\s+1 is any \s-1\&\f(CRBOOL\fR\s+1 expression.  If the
\s-1\&\f(CO<pre.condition>\fR\s+1 is \s-1\&\f(CRFALSE\fR\s+1, the \s-1\&\f(CO<simple.guard>\fR\s+1 and its associated
\s-1\&\f(CO<process>\fR\s+1 are not candidates for execution \(em even if the
\s-1\&\f(CO<simple.guard>\fR\s+1 becomes `ready'.  (Note that, because of the rules
forbidding shared data between parallel processes, the value of any
\s-1\&\f(CO<pre.condition>\fR\s+1 cannot alter whilst awaiting a \s-1\&\f(CO<simple.guard>\fR\s+1.)
.LP
\fBNote:\fP \fIexcept within\fP a \s-1\&\f(CRPRI\fR\s+1 \s-1\&\f(CRALT\fR\s+1
(section 6), \s-1\&\f(CRSKIP\fR\s+1 guards are compelled (by language rules)
to have a pre-condition dependent on run-time values
\(em they make no sense otherwise.
.NH 2
Sequential Choice
.LP
.DS
\s-1\&\f(CRIF
  \&\f(CO<condition.0>
    <process.0>
  <condition.1>
    <process.1>
  <condition.2>
    <process.2>\fR\s+1
.DE
\fRThe indentation and extent rules are as for the \s-1\&\f(CRALT\fR\s+1 construct above.
We shall refer to a \s-1\&\f(CO<condition>\fR\s+1 and its associated \s-1\&\f(CO<process>\fR\s+1
as a \s-1\&\f(CO<conditional.process>\fR\s+1.
This corresponds to a traditional:
.DS
\s-1\&\f(CRif (\&\f(CO<condition.0>\&\f(CR) {\&\f(CO<process.0>\&\f(CR} else
if (\&\f(CO<condition.1>\&\f(CR) {\&\f(CO<process.1>\&\f(CR} else
if (\&\f(CO<condition.2>\&\f(CR) {\&\f(CO<process.2>\&\f(CR} else
STOP\fR\s+1
.DE
of, say, \fIJava\fP or \fIC\fP.
Note the \s-1\&\f(CRSTOP\fR\s+1 if no \s-1\&\f(CO<condition>\fR\s+1 holds!
.NH 2
Sequential Loop
.LP
.DS
\s-1\&\f(CRWHILE \&\f(CO<condition>
  <process>\fR\s+1
.DE
\fRThis is just like a while-loop in \fIJava\fP.
The \s-1\&\f(CO<process>\fR\s+1 must be indented two spaces from
the \s-1\&\f(CRWHILE\fR\s+1.  The body of the loop is defined by
the indentation.
.LP
There are no \s-1\&\f(CRGOTO\fR\s+1s in \fIoccam\fP nor any abnormal loop
breaking mechanisms (such as \s-1\&\f(CRbreak\fR\s+1 or
\s-1\&\f(CRreturn\fR\s+1).
Thus, we always know that, when the loop does terminate, the\s-1\&\f(CO<condition>\fR\s+1
must be \s-1\&\f(CRFALSE\fR\s+1.
This is not the case for \fIJava\fP, \fIPascal\fP, \fIC\fP, \fIFORTRAN\fP, \fIAda\fP, \fIModula-2\fP, ...
.NH 2
Process Instantiation
.LP
.DS
\s-1\&\f(CRfoo (hello, FALSE, a, q[0]?, q[199]!, p?)\fR\s+1
.DE
\fRThis is like an \fIown-class\fP method invocation in \fIJava\fP.
Value parameters must be passed \fIexpressions\fP of the correct type.
Reference parameters must be passed \fIvariables\fP of the correct type.
Channel parameters must be passed the correct ends of channels
(\s-1\&\f(CR?\fR\s+1 or \s-1\&\f(CR!\fR\s+1) with the correct protocol
(section 7).
We may also have timer and port (section 10) parameters.
.NH 1
Replicated Processes
.LP
The \s-1\&\f(CRSEQ\fR\s+1, \s-1\&\f(CRPAR\fR\s+1, \s-1\&\f(CRALT\fR\s+1, and \s-1\&\f(CRIF\fR\s+1 constructs may be
replicated.  Suppose \s-1\&\f(COXXX\fR\s+1 is one of these four
keywords.  Then:
.DS
\s-1\&\f(COXXX \&\f(CRi = start FOR n
  \&\f(CO<thing.which.may.use.i>\fR\s+1
.DE
is short-hand for:
.DS
\s-1\&\f(COXXX
  <thing.with.i.replaced.by.start>
  <thing.with.i.replaced.by.(start + 1)>
  \&...
  <thing.with.i.replaced.by.(start + (n \- 1))>\fR\s+1
.DE
For \s-1\&\f(CRSEQ\fR\s+1 and \s-1\&\f(CRPAR\fR\s+1, the \s-1\&\f(CO<thing.which.may.use.i>\fR\s+1 is a \s-1\&\f(CO<process>\fR\s+1.
For \s-1\&\f(CRALT\fR\s+1, it is a \s-1\&\f(CO<guarded.process>\fR\s+1.  For \s-1\&\f(CRIF\fR\s+1, it is a
\s-1\&\f(CO<conditional.process>\fR\s+1.
The replicator control value, \s-1\&\f(CRi\fR\s+1, is an \s-1\&\f(CRINT\fR\s+1
declared by this construct.
It has scope for, and may not be altered by, the \s-1\&\f(CO<thing.which.may.use.i>\fR\s+1.
For a replicated \s-1\&\f(CRPAR\fR\s+1,
the replication factor, \s-1\&\f(CRn\fR\s+1, should preferably be a constant.
However, \fHoccam-\fP\(*p allows this to be run-time defined, although that
does reduce (currently) some of the compile-time safety checks.
.LP
The replicated \s-1\&\f(CRSEQ\fR\s+1 corresponds to a traditional \s-1\&\f(CRfor\fR\s+1-loop.  The
replicated \s-1\&\f(CRIF\fR\s+1 is an efficient construct for performing a bounded
length search.  The
\s-1\&\f(CO<conditional.process>\fR\s+1 of an \s-1\&\f(CRIF\fR\s+1 construct can itself
be an \s-1\&\f(CRIF\fR\s+1 construct.
The \s-1\&\f(CO<conditional.processes>\fR\s+1 of an inner \s-1\&\f(CRIF\fR\s+1 are treated as through they
were at the same level as those of the outer \s-1\&\f(CRIF\fR\s+1.
Thus, a search through the array \s-1\&\f(CRc\fR\s+1 for a particular item, say \s-1\&\f(CR42\fR\s+1,
may be coded:
.DS
\s-1\&\f(CRIF
  IF i = 0 FOR SIZE c
    c[i] = 42
      found.it, its.index := TRUE, i
  TRUE
    found.it := FALSE\fR\s+1
.DE
where \s-1\&\f(CRSIZE\fR\s+1 \s-1\&\f(CRc\fR\s+1 is the number
of elements in \s-1\&\f(CRc\fR\s+1.
This could be coded using a \s-1\&\f(CRWHILE\fR\s+1 or replicated \s-1\&\f(CRSEQ\fR\s+1 construct \(em
but it would be much less clear and efficient.
.LP
Similarly to \s-1\&\f(CRIF\fR\s+1 constructs, the \s-1\&\f(CO<guarded.process>\fR\s+1 within an \s-1\&\f(CRALT\fR\s+1 construct can
itself be an \s-1\&\f(CRALT\fR\s+1 construct \(em the \s-1\&\f(CO<guarded.processes>\fR\s+1 of the inner
\s-1\&\f(CRALT\fR\s+1 being treated as though they were at the same level as those
of the outer \s-1\&\f(CRALT\fR\s+1.  The use of replicated \s-1\&\f(CRALT\fR\s+1 and
\s-1\&\f(CRPAR\fR\s+1 constructs has no analogy within traditional programming languages.
.LP
The expression \s-1\&\f(CR[c FROM start FOR n]\fR\s+1 represents a `slice' of the
array \s-1\&\f(CRc\fR\s+1
from element \s-1\&\f(CRc[start]\fR\s+1 to \s-1\&\f(CRc[start + (n \- 1)]\fR\s+1 inclusive.
\s-1\&\f(CR[c FOR n]\fR\s+1 represents a `slice' covering the first \s-1\&\f(CRn\fR\s+1 elements.
\s-1\&\f(CR[c FROM start]\fR\s+1 represents a `slice' from element \s-1\&\f(CRstart\fR\s+1 to the end.
Array slices may be assigned to one another,
or input or output through channels.
The sizes and types of slices on either side of the assignment or channel must be equal.
Such operations are more efficient than a single assignment, input or
output controlled by a corresponding replicated \s-1\&\f(CRSEQ\fR\s+1.
Array parameters may also be passed array slices.
.NH 1
Prioritised Processes
.LP
We may write \s-1\&\f(CRPRI\fR\s+1 \s-1\&\f(CRALT\fR\s+1 instead of \s-1\&\f(CRALT\fR\s+1.  The only difference is that
if more than one of the \s-1\&\f(CO<guards>\fR\s+1 is `ready', the choice of which one to
execute is not \fIarbitrary\fP but is the \fIfirst\fP in the order listed.
.LP
We may write \s-1\&\f(CRPRI\fR\s+1 \s-1\&\f(CRPAR\fR\s+1 instead of \s-1\&\f(CRPAR\fR\s+1.  This imposes a `priority'
on the \s-1\&\f(CO<processes>\fR\s+1 to be run in parallel according to the order in
which the \s-1\&\f(CO<processes>\fR\s+1 are listed.
Lower priority \s-1\&\f(CO<processes>\fR\s+1 are never
executed if there is a higher priority one able to run.
The current transputer implementation restricts the number of components
of a \s-1\&\f(CRPRI\fR\s+1 \s-1\&\f(CRPAR\fR\s+1 to just two \(em `high' priority and `low' priority.
.LP
.NH 1
Protocols
.LP
All channel declarations state the message structure (called the \s-1\&\f(CRPROTOCOL\fR\s+1) carried by that channel.
All messages sent over a channel must conform to that protocol.
All protocols used so far have been `simple' ones \(em i.e. \fIoccam\fP data-types.
However, we frequently want messages with richer structures \(em e.g.
a mixture of data-types,
differing data-types (depending upon the run-time state)
or differing amounts of the same data-type (depending upon the run-time state).
.NH 2
Simple Protocols
.LP
Any \fIoccam\fP data-type may be used for a channel protocol \(em even arrays:
.DS
\s-1\&\f(CRCHAN [max]REAL64 chunk:\fR\s+1
.DE
Suppose \s-1\&\f(CRping\fR\s+1 is a \s-1\&\f(CR[max]REAL64\fR\s+1 array.
Then:
.DS
\s-1\&\f(CRchunk ! ping\fR\s+1
.DE
outputs all 50 elements of \s-1\&\f(CRping\fR\s+1 down the channel
\s-1\&\f(CRchunk\fR\s+1.
At the other end:
.DS
\s-1\&\f(CRchunk ? pong\fR\s+1
.DE
inputs all 50 elements into \s-1\&\f(CRpong\fR\s+1
(which must, of course, be a \s-1\&\f(CR[max]REAL64\fR\s+1 variable).
.LP
\fBNote:\fP this single transfer of the whole array is much more efficient (in
terms of code space and execution speed) than the replicated transfer of
its elements, one at a time, down a \s-1\&\f(CRREAL64\fR\s+1 channel.
.NH 2
Sequential Protocols
.LP
Suppose we need to send three values of differing types \(em say an integer, a real and a boolean
\(em from one process to another.  First, define a suitable message structure:
.DS
\s-1\&\f(CRPROTOCOL PACKET IS INT; REAL64; BOOL:\fR\s+1
.DE
where \s-1\&\f(CRPACKET\fR\s+1 is our own choice of name.  Then, we may declare:
.DS
\s-1\&\f(CRCHAN PACKET carry:\fR\s+1
.DE
When the channel \s-1\&\f(CRcarry\fR\s+1 is used, we must output a message that
matches the structure of the \s-1\&\f(CRPACKET\fR\s+1 \(em for example:
.DS
\s-1\&\f(CRcarry ! n; a*(b + c); flag\fR\s+1
.DE
where \s-1\&\f(CRn\fR\s+1 must be an \s-1\&\f(CRINT\fR\s+1, 
\s-1\&\f(CRa\fR\s+1, \s-1\&\f(CRb\fR\s+1 and \s-1\&\f(CRc\fR\s+1 must be \s-1\&\f(CRREAL64\fR\s+1s and
\s-1\&\f(CRflag\fR\s+1 must be a \s-1\&\f(CRBOOL\fR\s+1.
As always, this output process is suspended until the whole message has
been input at the other end of the channel.
.LP
The receiver must provide variables that also match the
\s-1\&\f(CRPACKET\fR\s+1 protocol \(em for example:
.DS
\s-1\&\f(CRcarry ? i; x; mode\fR\s+1
.DE
where \s-1\&\f(CRi\fR\s+1 must be an \s-1\&\f(CRINT\fR\s+1, \s-1\&\f(CRx\fR\s+1 must be a \s-1\&\f(CRREAL64\fR\s+1 and \s-1\&\f(CRmode\fR\s+1 must be
a \s-1\&\f(CRBOOL\fR\s+1.
.LP
There is an analogy in the way that a sequential protocol packages messages to the
way that a `record' structure packages data-types.
However, it is a little bit
different since the ordering of message fields is significant \(em the
data is delivered in the sequence defined by the protocol declaration.
We may safely rely on this ordering, for example, to send information in the
first part of the
message that defines where later parts are to be stored:
.DS
\s-1\&\f(CRcarry ? i; x[i]; mode[i]\fR\s+1
.DE
where \s-1\&\f(CRx\fR\s+1 and \s-1\&\f(CRmode\fR\s+1 must now be (respectively) \s-1\&\f(CRREAL64\fR\s+1 and \s-1\&\f(CRBOOL\fR\s+1 arrays.
.NH 2
Variant Protocols
.LP
If we want to send different message structures at different times down the same channel, we
use a variant (or \s-1\&\f(CRCASE\fR\s+1) protocol.
In the protocol declaration, we list the various message structures
we wish to send, each preceded by a unique `tag' name of our choice.
For example:
.DS
\s-1\&\f(CRPROTOCOL SERVICE
  CASE
    enquire; INT
    update; INT; REAL32
    reset
    terminate
:\fR\s+1
.DE
The tags \s-1\&\f(CRenquire\fR\s+1, \s-1\&\f(CRupdate\fR\s+1,
\s-1\&\f(CRreset\fR\s+1 and \s-1\&\f(CRterminate\fR\s+1 are new and
distinct constants of a new and private type.
They are introduced by the above declaration.
They can only be used in communications over channels carrying this
protocol \(em for example:
.DS
\s-1\&\f(CRCHAN SERVICE to.server:
CHAN REAL32 from.server:\fR\s+1
.DE
Notice that two of the tags (\s-1\&\f(CRreset\fR\s+1
and \s-1\&\f(CRterminate\fR\s+1) are followed by no further message.
The tag is the only information conveyed.
.LP
Outputting down variant channels is straightforward \(em for example:
.DS
\s-1\&\f(CRSEQ
  to.server ! reset
  \&...
  to.server ! update; 42; 3.142 (REAL32)
  \&...
  to.server ! enquire; 42
  from.server ? x
  \&...
  to.server ! terminate\fR\s+1
.DE
where \s-1\&\f(CRx\fR\s+1 is a \s-1\&\f(CRREAL32\fR\s+1 variable.
The inputting side uses a \s-1\&\f(CRCASE\fR\s+1 syntax to distinguish between
the variant message structures according to the tag \(em for example:
.DS
\s-1\&\f(CRWHILE running
  to.server ? CASE
    enquire; i
      from.server ! B[i]
    update; i; B[i]
      SKIP
    reset
      \&...  clear B back to initial values
    terminate
      running := FALSE\fR\s+1
.DE
where \s-1\&\f(CRrunning\fR\s+1 is a \s-1\&\f(CRBOOL\fR\s+1, \s-1\&\f(CRi\fR\s+1 is an \s-1\&\f(CRINT\fR\s+1 and \s-1\&\f(CRB\fR\s+1 is an array of \s-1\&\f(CRREAL32\fR\s+1s.
.LP
\fBNote:\fP if the system is in a state where some of the protocol variants
will not be sent, the inputting process is not obliged to cater for them
(i.e. we may omit the relevant tagged input lines and associated processes).
If that leaves only one variant to be serviced, a short-hand form
of the input syntax may be used \(em for example:
.DS
\s-1\&\f(CR  to.server ? CASE update; i; B[i]\fR\s+1
.DE
\fBNote:\fP if a variant arrives that is not allowed for by the inputting process,
this is a run-time error and the process \s-1\&\f(CRSTOP\fR\s+1s.
This is caused, of course, by a system design error.
.NH 2
Counted Array Protocols
.LP
A common form of message is a list of data items whose length is determined
at run-time.
Such a structure is called a \fIcounted array\fP and is described by the
following syntax:
.DS
\s-1\&\f(CO<count.type>\f(CR::[]\f(CO<any.type>\fR\s+1
.DE
where \s-1\&\f(CO<count.type>\fR\s+1 is either a \s-1\&\f(CRBYTE\fR\s+1
or an \s-1\&\f(CRINT\fR\s+1 and \s-1\&\f(CO<any.type>\fR\s+1 is any \fIoccam\fP
data type.
It defines a message with two components \(em a count and an array (whose size
is that count).
The count may be zero, but must not be negative.
.LP
A counted array may be a protocol on its own or a field in a sequential
or variant protocol.
For example:
.DS
\s-1\&\f(CRPROTOCOL STRING IS BYTE::[]BYTE:
PROTOCOL FLEXI.CHUNK IS INT::[]REAL64:\fR\s+1
.DE
Then, we may declare:
.DS
\s-1\&\f(CRCHAN STRING screen:
CHAN FLEXI.CHUNK flexible:\fR\s+1
.DE
Such \s-1\&\f(CRSTRING\fR\s+1 channels allow strings of up to 255 characters
(including the null string) to be transmitted.
For example:
.DS
\s-1\&\f(CRVAL []BYTE epitaph IS "Goodbye Cruel World ...":
VAL BYTE size IS BYTE (SIZE epitaph):
screen ! size::epitaph\fR\s+1
.DE
This outputs the entire \s-1\&\f(CRepitaph\fR\s+1.
As always, this output process does not complete until the message is input
by the process at the other end of the channel.
.LP
We do not have to output the whole of an array \(em for example:
.DS
\s-1\&\f(CRflexible ! n::ping\fR\s+1
.DE
outputs only the first \s-1\&\f(CRn\fR\s+1 elements of the \s-1\&\f(CRREAL64\fR\s+1
array \s-1\&\f(CRping\fR\s+1, where \s-1\&\f(CRn\fR\s+1 is an \s-1\&\f(CRINT\fR\s+1 value.
If \s-1\&\f(CRping\fR\s+1 has less than \s-1\&\f(CRn\fR\s+1 elements, this is a run-time error.
.LP
At the receiving end:
.DS
\s-1\&\f(CRscreen ? length::text\fR\s+1
.DE
inputs the \s-1\&\f(CRBYTE\fR\s+1 count of the \s-1\&\f(CRSTRING\fR\s+1
message into \s-1\&\f(CRlength\fR\s+1 (which must be a \s-1\&\f(CRBYTE\fR\s+1
variable) and the array part into the first \s-1\&\f(CRlength\fR\s+1
elements of \s-1\&\f(CRtext\fR\s+1 (which must be a sufficiently long
\s-1\&\f(CRBYTE\fR\s+1 array).
.LP
When we need to communicate to and from sections of an array
\fIother than\fR the initial one, we must use array slices (see the end
of section 5).
Consider:
.DS
\s-1\&\f(CRflexible ? m::[pong FROM start FOR slice]\fR\s+1
.DE
where \s-1\&\f(CRm\fR\s+1 is an \s-1\&\f(CRINT\fR\s+1 variable,
\s-1\&\f(CRstart\fR\s+1 and \s-1\&\f(CRslice\fR\s+1 are \s-1\&\f(CRINT\fR\s+1
values and \s-1\&\f(CRpong\fR\s+1 is a \s-1\&\f(CRREAL64\fR\s+1 array with
at least \s-1\&\f(CR(start\fR\s+1 \s-1\&\f(CR+\fR\s+1 \s-1\&\f(CRslice)\fR\s+1
elements.
This process inputs the \s-1\&\f(CRFLEXI.CHUNK\fR\s+1 count value into
\s-1\&\f(CRm\fR\s+1.
The array part of the message goes into the \s-1\&\f(CRm\fR\s+1 consecutive
elements of \s-1\&\f(CRpong\fR\s+1 starting from index \s-1\&\f(CRstart\fR\s+1.
There will be a run-time error unless
\s-1\&\f(CR0\fR\s+1 \(<= \s-1\&\f(CRm\fR\s+1 \(<= \s-1\&\f(CRslice\fR\s+1.
.bp
.NH 1
Abbreviations
.NH 2
Reference Abbreviations
.LP
Any \fIoccam\fP item (i.e. a piece of data, a channel, a timer or a
port) may be passed as an \fIactual\fP parameter to a (matching)
\fIformal\fP parameter of a \s-1\&\f(CRPROC\fR\s+1.  During the execution of the
\s-1\&\f(CRPROC\fR\s+1 body, the item acquires the formal parameter name as
an `alias' or `abbreviation'.
.LP
Aliasing (i.e. allowing one item
to be referred to by different names) is uncontrolled in
traditional programming languages and leads to semantic
complexities that are generally underestimated,
easily overlooked and cause serious error.
In \fIoccam\fP, when a new name is introduced
for an existing item, we are only allowed to use the new name \(em the
old name is banned!  We have, therefore, the assurance that
different names in the same scope refer to different items \(em
regardless of the context in which they appear.  This may seem
a minor issue, but it is highly significant in practice.
.LP
We shall return to parameter mechanisms shortly.  First, \fIoccam\fP
has a direct way of introducing a new name for an existing
item:
.DS
\s-1\&\f(CO<item.specifier> <new.name> \f(CRIS \f(CO<old.name>\f(CR:\fR\s+1
.DE
where \s-1\&\f(CO<item.specifier>\fR\s+1 indicates a data-type, channel-type,
timer or port-type.
The \s-1\&\f(CO<old.name>\fR\s+1 may be a variable, array
element or array slice.  It names a particular item conforming to the
\s-1\&\f(CO<item.specifier>\fR\s+1.
The \s-1\&\f(CO<old.name>\fR\s+1 may not be used whilst the
\s-1\&\f(CO<new.name>\fR\s+1 is in scope.
Any variables used in \s-1\&\f(CO<old.name>\fR\s+1 to determine individual
array elements or slices may be used in the scope of
\s-1\&\f(CO<new.name>\fR\s+1, but may not be changed by it.
.LP
The \s-1\&\f(CO<new.name>\fR\s+1 is said to be an `abbreviation' of
the \s-1\&\f(CO<old.name>\fR\s+1.
Such an abbreviation may occur anywhere a declaration is allowed.
As with declarations,
its scope covers only the process that immediately follows it.
Of course, that process may have its own declarations and abbreviations.
For example:
.DS
\s-1\&\f(CRINT result IS n:
[]REAL64 row IS X[i]:
CHAN FLEXI.CHUNK out! IS flexible!:
\f(CO\&<process>\fR\s+1
.DE
where \s-1\&\f(CRn\fR\s+1 must be an \s-1\&\f(CRINT\fR\s+1 variable, \s-1\&\f(CRX\fR\s+1 must be a \s-1\&\f(CRREAL64\fR\s+1 matrix,
\s-1\&\f(CRi\fR\s+1 must be an \s-1\&\f(CRINT\fR\s+1 value (within the range of the first dimension
of \s-1\&\f(CRX\fR\s+1) and \s-1\&\f(CRflexible\fR\s+1 must be a channel
carrying the \s-1\&\f(CRFLEXI.CHUNK\fR\s+1 protocol.
.LP
Within \s-1\&\f(CO\&<process>\fR\s+1,
the `old' names \s-1\&\f(CRn\fR\s+1, \s-1\&\f(CRX[i]\fR\s+1 and \s-1\&\f(CRflexible\fR\s+1 may not be used \(em we must use
the `new' names \s-1\&\f(CRresult\fR\s+1, \s-1\&\f(CRrow\fR\s+1 and \s-1\&\f(CRout\fR\s+1 instead.
The value of \s-1\&\f(CRi\fR\s+1 may be used but not changed!
More generally, if \s-1\&\f(CRi\fR\s+1 were an expression, all
its constituent variables would have their values frozen.
Disjoint parts of the matrix \s-1\&\f(CRX\fR\s+1
may be referenced (e.g. \s-1\&\f(CRX[j]\fR\s+1),
but only if we can guarantee their
disjointness \(em e.g. \s-1\&\f(CRi\fR\s+1 and \s-1\&\f(CRj\fR\s+1 are constant values, known to be
different at compile time.
.LP
We may, of course, abbreviate abbreviations:
.DS
\s-1\&\f(CR[16]REAL64 section IS [row FROM start FOR 16]:\fR\s+1
.DE
where the value of \s-1\&\f(CRstart\fR\s+1 is now frozen within the scope of
\s-1\&\f(CRsection\fR\s+1.  Note that \s-1\&\f(CRsection[0]\fR\s+1 corresponds to \s-1\&\f(CRrow[start]\fR\s+1 which
corresponds to \s-1\&\f(CRX[i][start]\fR\s+1, but that the latter two names may not
now be used.
.NH 2
Value Abbreviations
.LP
Any data \fIvalue\fP (i.e. an expression made from variables, literal constants,
array elements and array slices) may be passed to a (matching) formal
\s-1\&\f(CRVAL\fR\s+1 parameter of a \s-1\&\f(CRPROC\fR\s+1.
During the execution of the
\s-1\&\f(CRPROC\fR\s+1 body, the value acquires the formal parameter name.
We may not assign or input to this name \(em it represents a \fIvalue\fP,
not a \fIvariable\fP, and we are not allowed to change it!
.LP
Again, \fIoccam\fP has a direct way of introducing names for data values:
.DS
\s-1\&\f(CRVAL \f(CO<data.type> <new.name> \f(CRIS \f(CO<expression>\f(CR:\fR\s+1
.DE
We may not assign or input to \s-1\&\f(CO<new.name>\fR\s+1.
Any variables mentioned within the \s-1\&\f(CO<expression>\fR\s+1 can
continue to be used within the scope of \s-1\&\f(CO<new.name>\fR\s+1, but may
not be changed by that process.
The \s-1\&\f(CO<expression>\fR\s+1 must yield a value of \s-1\&\f(CO<data.type>\fR\s+1.
.LP
Section 2.1 uses this mechanism to name values known at compile-time (i.e.
constants).
However, values computed at run-time may also be given names \(em for example:
.DS
\s-1\&\f(CRVAL REAL32 hypoteneuse IS SQRT ((a*a) + (b*b)):
VAL []REAL64 row IS X[i]:
VAL n IS SIZE row:
\&\f(CO<process>\fR\s+1
.DE
where \s-1\&\f(CRa\fR\s+1 and \s-1\&\f(CRb\fR\s+1 must be \s-1\&\f(CRREAL32\fR\s+1 values, \s-1\&\f(CRX\fR\s+1 must be a \s-1\&\f(CRREAL64\fR\s+1
matrix and \s-1\&\f(CRi\fR\s+1 must be an \s-1\&\f(CRINT\fR\s+1 value.
Within \s-1\&\f(CO\&<process>\fR\s+1,
the `new' names \s-1\&\f(CRhypoteneuse\fR\s+1, \s-1\&\f(CRrow\fR\s+1 and \s-1\&\f(CRn\fR\s+1 represent values that
cannot be altered.  The names \s-1\&\f(CRa\fR\s+1, \s-1\&\f(CRb\fR\s+1, \s-1\&\f(CRi\fR\s+1 and \s-1\&\f(CRX[i]\fR\s+1 may be freely
used, but again their values cannot be altered.
.LP
\fBNote:\fP value abbreviations \fIdo\fP allow multiple names for the same
piece of data to be introduced (e.g. when the \s-1\&\f(CO<expression>\fR\s+1 is just a variable)
and used in the same scope \(em but only after
first `freezing' that piece of data to a constant value.  Different names
for the same constant do not lead to the semantic complexities
of different names for the same variable.
.NH 2
Parameters and Abbreviations
.LP
\fIoccam\fR \s-1\&\f(CRPROC\fR\s+1 calls are formally defined by in-line expansion
into the text of the \s-1\&\f(CRPROC\fR\s+1 body.  Formal parameters are linked
to the actual ones by a sequence of abbreviations.
For example, the call:
.DS
\s-1\&\f(CRfoo (hello, FALSE, a, q[0]?, q[199]!, p?)\fR\s+1
.DE
(see section 2.5 and 4.6) transforms into:
.DS
\s-1\&\f(CRVAL []BYTE s IS hello:
VAL BOOL mode IS FALSE:
INT result IS a:
CHAN INT in? IS q[0]?:
CHAN INT out! IS q[199]!:
CHAN BYTE pause? IS p?:
\f(CO<process>\fR\s+1
.DE
where \s-1\&\f(CO<process>\fR\s+1 is the text of the body of \s-1\&\f(CRfoo\fR\s+1.
.LP
The point of this relationship is that it enables us to derive
anti-aliasing laws regarding the use of parameters directly from
those for abbreviations.  These laws are strictly enforced.
.bp
.LP
For example, if the actual parameter \s-1\&\f(CRa\fR\s+1 was in scope at the point of
definition of \s-1\&\f(CRfoo\fR\s+1 and if it was mentioned in its body, the above
call would lead to a violation of the aliasing rules for reference
abbreviation and would be rejected by the compiler.
.LP
Similarly, the calls:
.DS
\s-1\&\f(CRfoo (hello, FALSE, a, q[0]?, q[199]!, panic[a]?)\fR\s+1
.DE
where \s-1\&\f(CRpanic\fR\s+1 is an array of \s-1\&\f(CRBYTE\fR\s+1 channels,
and:
.DS
\s-1\&\f(CRfoo (hello, FALSE, a, q[42]?, q[42]!, p?)\fR\s+1
.DE
would always be suppressed.
.LP
\fBNote:\fP free names in \s-1\&\f(CRPROC\fR\s+1 bodies are bound to the global
definitions visible at the point
of the \s-1\&\f(CRPROC\fR\s+1 definition \(em not at the \s-1\&\f(CRPROC\fR\s+1
call.
The above transformation is, therefore, subject to the condition that any such
free names do not have re-declarations in scope at the point of the call.
If those names are so re-declared, the re-declarations must be eliminated
(by choosing different names that cause no similar clashes) before the
transformation becomes valid.
This follows precisely the rule for consistent substitution from the
\(*l-calculus.
[This rule is relevant only for completing the formal definition of the \s-1\&\f(CRPROC\fR\s+1 call
through in-line substitution of the \s-1\&\f(CRPROC\fR\s+1 body
(i.e. \(*b-reduction).
In practice, of course, \fIoccam\fP is not usually implemented like this \(em
i.e. we do not have to avoid such re-declarations.]
.NH 2
And So ...
.LP
The anti-aliasing laws greatly simplify the semantics and
implementation of parameter passing.  For reference parameters,
\fIcopy-in/copy-out\fR and \fIcall-by-address\fR mechanisms are
indistinguishable \(em the compiler may apply whichever is most
efficient (e.g. the former for `small' items and the latter for
`large' ones).  For value parameters, \fIcopy-in\fR and
\fIcall-by-address\fR are also the same \(em so it is perfectly secure to pass
arrays by address (since the \s-1\&\f(CRPROC\fR\s+1 body is forbidden to
update them).
.LP
To illustrate the simplicity that results from \fIoccam\fP's
careful control of aliasing, consider:
.DS
\s-1\&\f(CRSEQ
  n := n \+ a
  n := n \- a\fR\s+1
.DE
where \s-1\&\f(CRn\fR\s+1 and \s-1\&\f(CRa\fR\s+1 are \s-1\&\f(CRINT\fR\s+1 variables.  Assuming we are not dealing
with values that cause arithmetic overflow, everything we know
about algebra and the properties of variables, assignment and
sequencing tells us that the above code changes nothing.  With
\fIoccam\fP, this simple interpretation is the correct one and the
code may be safely replaced with a \s-1\&\f(CRSKIP\fR\s+1.
.LP
With languages that take a less rigorous approach towards aliasing
(such as \fIJava\fP, \fIPascal\fP, \fIC\fP, \fIFORTRAN\fP, \fIAda\fP, \fIModula-2\fP, ...),
the semantics are not so straightforward.  We have
to look into the context of the code and check the bindings of
the variables.  If both \s-1\&\f(CRn\fR\s+1 and \s-1\&\f(CRa\fR\s+1 refer to the same data item (e.g.
when \s-1\&\f(CRn\fR\s+1 and \s-1\&\f(CRa\fR\s+1 are formal reference parameters to which the
same actual parameter has been supplied), the above code
changes that data item to zero!  The problem is that the \s-1\&\f(CRa\fR\s+1
`variable' is no longer behaving in the way we expect a
variable to behave \(em i.e. remain the same unless we change it.
If \s-1\&\f(CRn\fR\s+1 is an alias name for \s-1\&\f(CRa\fR\s+1, the value of \s-1\&\f(CRa\fR\s+1 is different in each of
the above lines!!
.LP
Such semantic complexities are easily missed when reasoning
about algorithms in traditional languages.  With \fIoccam\fP,
different names in the same scope refer to different items whatever
the context and no such singularities of meaning are possible.
What you see is what you get \(em end of story!
.bp
.NH 1
Retyping
.LP
It is sometimes helpful to view a piece of data as though it had
a different type structure from its `natural' one.  \fIoccam\fR allows
this with a slight variation on its abbreviation concept.  For
example, we may have reference retyping:
.DS
\s-1\&\f(CO<data.type> <new.name> \f(CRRETYPES \f(CO<old.name>\f(CR:\fR\s+1
.DE
All the anti-aliasing rules for reference abbreviation apply.  The
only difference is that the type of \s-1\&\f(CO<old.name>\fR\s+1 need not be the
same as \s-1\&\f(CO<data.type>\fR\s+1, but its representation must occupy the
same number of bits.  Using \s-1\&\f(CO<new.name>\fR\s+1, we manipulate the
bit-string value of \s-1\&\f(CO<old.name>\fR\s+1 as though it belonged to the new
\s-1\&\f(CO<data.type>\fR\s+1.  No type-conversion takes place 
\(em the interpretation of the value in the new type is
implementation-dependent upon the representation formats for
the old and new types.
.LP
For example, if \s-1\&\f(CRn\fR\s+1 is an \s-1\&\f(CRINT32\fR\s+1 variable, then:
.DS
\s-1\&\f(CR[4]BYTE b RETYPES n:
b[0], b[1], b[2], b[3] := b[3], b[2], b[1], b[0]\fR\s+1
.DE
reverses the ordering of the byte representation of \s-1\&\f(CRn\fR\s+1 (which may
be useful in reformatting data produced by a `little-endian'
machine for use in a `big-endian' one).
.LP
Consider also:
.DS
\s-1\&\f(CRREAL64 x RETYPES [buffer FROM speed.index FOR 8]:
x := speed\fR\s+1
.DE
where \s-1\&\f(CRbuffer\fR\s+1 is a \s-1\&\f(CRBYTE\fR\s+1 array with at 
least \s-1\&\f(CR(speed.index\fR\s+1 \s-1\&\f(CR+\fR\s+1 \s-1\&\f(CR8)\fR\s+1 
elements and \s-1\&\f(CRspeed\fR\s+1 is a \s-1\&\f(CRREAL64\fR\s+1.  This 
packs the \s-1\&\f(CRspeed\fR\s+1 value into an 8-byte slice of the array 
\s-1\&\f(CRbuffer\fR\s+1 (prior, perhaps, to output down an unstructured
byte-stream).
.LP
We also have value retyping:
.DS
\s-1\&\f(CRVAL \f(CO<data.type> <new.name> \f(CRRETYPES \f(CO<expression>\f(CR:\fR\s+1\fR\s+1
.DE
Again, the same rules for value abbreviation apply to value
retyping.  Again, the type of \s-1\&\f(CO<expression>\fR\s+1 need not be the same as
\s-1\&\f(CO<data.type>\fR\s+1, but it must have the same sized representation.
.LP
For example:
.DS
\s-1\&\f(CRVAL REAL64 x RETYPES [buffer FROM speed.index FOR 8]:
speed := x\fR\s+1
.DE
retrieves the \s-1\&\f(CRREAL64\fR\s+1 value (packed as an \s-1\&\f(CR[8]BYTE\fR\s+1 slice of
\s-1\&\f(CRbuffer\fR\s+1) into a \s-1\&\f(CRREAL64\fR\s+1 variable.  Notice that the original
packing could also have been done with a value retyping:
.DS
\s-1\&\f(CRVAL [8]BYTE s RETYPES speed:
[buffer FROM speed.index FOR 8] := s\fR\s+1
.DE
and, indeed, the unpacking could have been done by reference:
.DS
\s-1\&\f(CR[8]BYTE s RETYPES speed:
s := [buffer FROM speed.index FOR 8]\fR\s+1
.DE
Retypings are implementation-dependent in their meaning and
should be used with care and restraint!  Generally, their scope should be
very localised \(em they only extend over one line in the above
examples.  
.bp
.LP
Occasionally, value retypings are used to define (global)
constants \(em i.e they have a very long scope.  For example:
.DS
\s-1\&\f(CRVAL REAL64 pi RETYPES #400921FB54442D18 (INT64):
VAL REAL64 infinity RETYPES #7FF0000000000000 (INT64):
VAL REAL64 NaN RETYPES #7FF0000200000000 (INT64):\fR\s+1
.DE
(where the \s-1\&\f(CR#\fR\s+1 symbol introduces a hexadecimal literal)
defines \(*p, \v'0.1v'\s+4\(if\s-4\v'-0.1v'
and \fINot-a-Number\fR with full 64-bit precision according
to the IEEE/ANSI-754 standard format.
Expressing \(*p as a \s-1\&\f(CRREAL64\fR\s+1 literal:
.DS
\s-1\&\f(CRVAL REAL64 pi IS 3.141592653589793238462643383280 (REAL64):\fR\s+1
.DE
relies on conversion routines in the compiler that are accurate to the
last bit.
The other two values, \s-1\&\f(CRinfinity\fR\s+1 and \s-1\&\f(CRNaN\fR\s+1,
cannot be so expressed at all!
.NH 1
Ports
.LP
External devices that provide/receive information via memory-mapped
registers are interfaced to \fIoccam\fP processes through the notion of \fIports\fP.
.LP
Such a register must not be modelled by a variable, since it
does not behave as a variable \(em consecutive reads will often
yield different values.  It must not be modelled by a channel,
since read/write access is not synchronised with a matching
write/read.
.LP
In fact, ports are asynchronous channels.  They have a channel-like
syntax for declaration and use:
.DS
\s-1\&\f(CRPORT INT16 control.register:
PLACE control.register AT #80000100:
.sp 0.7v
PORT INT16 data.register:
PLACE data.register AT #80000104:\fR\s+1
.DE
Ports may be declared with any \fIoccam\fP data-type.
The \s-1\&\f(CRPLACE\fR\s+1ment line defines the base address
(usually expressed as a hexadecimal \s-1\&\f(CRINT\fR\s+1 literal) of the
data-structure representing the port.
.LP
\fBWarning:\fR although these base addresses must be complie-time values,
current \fIoccam\fP compilers make no attempt to check
that user-placed structures do not conflict either with each other or
with the compiled code, process workspace or any `special' locations
peculiar to the target architecture!  We are on our own here!!
.LP
The following code busily polls the \s-1\&\f(CRcontrol.register\fR\s+1 until its third bit
is set, whereupon it outputs a \s-1\&\f(CRvalue\fR\s+1 (which must be an \s-1\&\f(CRINT16\fR\s+1)
to the \s-1\&\f(CRdata.register\fR\s+1:
.DS
\s-1\&\f(CRVAL INT bit.3 IS 4:  -- bit mask
INT16 control:
SEQ
  control.register ? control
  WHILE (control /\e\ bit.3) = 0
    control.register ? control
  data.register ! value\fR\s+1
.DE
\fBNote:\fR `busy'-polling is not generally a good idea \(em the loop
should at least have a decent `sleep' process in its body.  Much
better than any such \fIpolling\fR of a control register is to \fIwait\fR for
an `interrupt' signal telling us to proceed \(em see section 11.6
.bp
.LP
Input and output on ports are asynchronous operations \(em i.e. they
never get blocked and they always terminate.
They are usually implemented as normal \fIloads\fP and \fIstores\fP on memory.
Reading from a \s-1\&\f(CRTIMER\fR\s+1 is logically similar to reading
from a \s-1\&\f(CRPORT\fR\s+1 \s-1\&\f(CRINT\fR\s+1.
Note that, as happens with timers (but unlike channels), ports introduce
non-determinism into the semantics.
.LP
Ports should only be used to interface \fIoccam\fP systems to
non-\fIoccam\fP devices.
This means that a port is used by an \fIoccam\fP system only at one end!
Ports may not be used to provide asynchronous communication between
parallel processes \fIwithin\fP the system!!
.NH 1
Odds and Ends
.LP
This section summarises the facilities in the language not so far mentioned.
They provide important abstractions that help ease the expression of
algorithms, but provide no `new' concepts that would be unfamiliar to
users of traditional languages (although, of course, \fIoccam\fP enforces
a very clean and secure binding for them).
They were not introduced earlier because their details would have caused
an unnecessary distraction.
.NH 2
Parallel Assignment
.LP
We have seen examples of parallel assignment in code
fragments in sections 6 and 9.  The full syntax is:
.DS
\s-1\&\f(CO<list.of.variables> \f(CR:= \f(CO<list.of.expressions>\fR\s+1
.DE
where a \s-1\&\f(CO<variable>\fR\s+1 is anything assignable (i.e. a variable, array
element or array slice) and an \s-1\&\f(CO<expression>\fR\s+1 is anything
that represents a data value.
The lists are comma-separated, have equal length and the types
of corresponding elements must match.
.LP
The semantics of parallel assignment has two consecutive phases:
.DS
\s-1\&\f(CRSEQ
  ...  evaluate the \f(CO<list.of.expressions>\f(CR
  ...  assign to the \f(CO<list.of.variables>\fR\s+1
.DE
Each phase operates in parallel.  Since \fIoccam\fP expressions cause no
side-effects, no restrictions are imposed on the
\s-1\&\f(CO<list.of.expressions>\fR\s+1.
On the other hand, the \s-1\&\f(CO<list.of.variables>\fR\s+1
must be distinct and independent.  Thus:
.DS
\s-1\&\f(CRi, i :=  3, 4\fR\s+1
.DE
is not allowed and neither is:
.DS
\s-1\&\f(CRi, a[i] :=  3, 4\fR\s+1
.DE
On the other hand, swapping the values of two variables:
.DS
\s-1\&\f(CRi, j :=  j, i\fR\s+1
.DE
is always (thanks to the anti-aliasing laws) legal \(em
as is any permutation of a list of variables (e.g. see section 9).
.NH 2
Functions and Value Processes
.LP
A \s-1\&\f(CO<value.process>\fR\s+1 is an \fIoccam\fP process that yields a (list of)
data values.  Because it is only used as (part of) an expression,
it must cause no side-effects and be fully deterministic.  Thus,
it may not change global data-structures, communicate over
global channels nor declare its own ports or timers.  However,
it may declare its own data variables and execute (serial)
algorithms using them.  Parallel code is forbidden \(em although
this is somewhat over-restrictive (only \s-1\&\f(CRALT\fR\s+1s should be disallowed!).
.LP
The syntax for a \s-1\&\f(CO<value.process>\fR\s+1 is:
.DS
\s-1\&\f(CR\f(CO<local.declarations.and.abbreviations>
\f(CRVALOF
  \f(CO<process>
  \f(CRRESULT \f(CO<list.of.expressions>\fR\s+1
.DE
The \s-1\&\f(CO<process>\fR\s+1 is any \fIoccam\fP process (subject to the above
restrictions).  The \s-1\&\f(CO<list.of.expressions>\fR\s+1 is as in section 11.1.
.LP
For example:
.DS
\s-1\&\f(CRaverage := (VAL INT n IS SIZE X:
           REAL32 sum:
           VALOF
             SEQ
               sum := 0.0 (REAL32)
               SEQ i = 0 FOR n
                 sum := sum + x [i]
             RESULT IS sum/n)\fR\s+1
.DE
A \s-1\&\f(CO<value.process>\fR\s+1 may be used in an expresssion anywhere its
resulting \s-1\&\f(CO<list.of.expressions>\fR\s+1 would be legal.  However, its real
practical role is to set up the mechanism for an \fIoccam\fP function.
.LP
A \s-1\&\f(CRFUNCTION\fR\s+1, like a \s-1\&\f(CO<value.process>\fR\s+1, yields a list of data
values, occurs only in an expression, causes no side-effects and
is deterministic:
.DS
\s-1\&\f(CO<list.of.types> \f(CRFUNCTION \f(CO<name> \f(CR(\f(CO<parameters>\f(CR)
  \f(CO<value.process>\f(CR
:\fR\s+1
.DE
.\"The resulting \s-1\&\f(CO<list.of.expressions>\fR\s+1 from the \s-1\&\f(CO<value.process>\fR\s+1
\fI<to be continued>\fR
.NH 2
Case Process
.LP
\fI<coming soon>\fP
.NH 2
More Local Declarations
.LP
\fI<coming soon>\fP
.NH 2
Special Operators
.LP
\fI<coming soon>\fP
.NH 2
Placing Channels and Variables
.LP
\fI<coming soon>\fP
.NH 2
Placing Processes
.LP
\fI<coming soon>\fP
.LP
