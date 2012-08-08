.RP
.TL
Programmer's Crib Sheet
.br
for the
.br
IRAF Program Interface
.AU
Douglas Tody
.AI
.K2 "" "" "*"
September 1983
.AB
IRAF applications programs, as well as most IRAF system programs, are
written in the IRAF SPP language with calls to the library routines
comprising the IRAF \fIprogram interface\fR.  The program interface
includes routines for file i/o, formatted i/o, command language (CL) i/o,
database i/o, image i/o, graphics i/o, magnetic tape i/o, and memory i/o.
Routines are also provided for manipulating virtual file names, for date
and time, process control, exception handling, bitwise boolean operations,
string operations, character comparisons, vector operations (array
processing), sorting, pattern matching, and random number generation.
.PP
Only the machine independent part of the program interface is described
here; the machine dependent OS interface is not intended for use in
applications programs and is therefore not described here.  An extensive
collection of mathematical library routines is available but is described
elsewhere.  As its name implies, the "crib sheet" does not explain how
to use the program interface routines; little more than the calling
sequence for each routine is given.
.AE
