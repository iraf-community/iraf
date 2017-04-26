.RP
.TL
A User's Guide to Fortran Programming in IRAF
.br
The IMFORT Interface
.AU
Doug Tody
.AI
.K2 "" "" "*"
September 1986

.AB
The IMFORT interface is a Fortran programming environment suitable for general
Fortran programming, with special emphasis on batch image processing.
IMFORT is intended for use primarily by the scientist/user who occasionally
needs to write a program for their own personal use, but who does not program
often enough to make it worthwhile learning a larger, more complex but fully
featured programming environment.  IMFORT is therefore a small interface which
is easy to learn and use, and which relies heavily upon host system (non-IRAF)
facilities which the user is assumed to already be familiar with.
Facilities are provided for accessing command line arguments, reading and
writing IRAF images, and returning output to the CL.  Provisions are made
for editing, compiling, linking, and debugging programs without need to leave
the IRAF environment, making use of familiar host system editing and debugging
tools wherever possible.
.AE

.NH
Introduction
.PP
The IMFORT interface is a library of Fortran callable subroutines which can
be called from a host system Fortran program to perform such operations as
fetching the arguments given on the command line when the task was invoked,
or accessing the header or pixel information in an IRAF image (bulk data frame).
Since the result is a host program rather than an IRAF program, only limited
access to the facilities provided by the runtime IRAF system is possible,
but on the other hand one has full access to the facilities provided by the
host system.  Programs which use IMFORT may be run as ordinary host system
programs outside of IRAF, or may be interfaced to the IRAF command language
(CL) as CL callable tasks.  Within the IRAF environment these user written,
non-IRAF tasks behave much like ordinary IRAF tasks, allowing background
execution, use of i/o redirection and pipes, evaluation of expressions on
the command line, programmed execution in scripts, and so on.

.NH 2
Who Should Use IMFORT
.PP
The most significant feature of the IMFORT interface is that it is designed
for use by \fIhost\fR Fortran programs.  The scientist/user will often already
be using such programs when IRAF becomes available.  IMFORT allows these
pre-existing programs to be modified to work within the IRAF environment
with a minimum of effort and with minimum changes to the existing program.
The only alternative is to rework these programs as \fIIRAF\fR programs,
but few existing Fortran programs could (or should) survive such a transition
without being completely rewritten.  If the program in question is useful
enough such a rewrite might be warranted, but in most cases this will not
be practical, hence something like the IMFORT interface is clearly needed
to keep these old programs alive until they are no longer needed.
.PP
The second goal of the IMFORT interface is to provide a way for the user to
add their own programs to IRAF without having to invest a lot of time learning
the full blown IRAF programming environment.  IMFORT makes it possible for
the user to begin writing useful programs within hours of their first exposure
to the system.  It is possible that the IMFORT interface will provide all the
capability that some users will ever need, especially when supplemented by other
(non-IRAF) Fortran callable libraries available on the local host machine.
Programs developed in this way are bound to have portability and other
problems, but it should be up to the developer and user of the software to
decide whether these problems are worth worrying about.  IMFORT is simply
a \fItool\fR, to be used as one sees fit; there is no attempt to dictate to
the user how they should write their programs.
.PP
The alternative to IMFORT, if applications programming within IRAF is the goal,
is the IRAF SPP/VOS programming environment.  The SPP/VOS programming
environment is a fully featured scientific programming environment which
carefully addresses all the software engineering issues avoided by IMFORT.
The VOS is a large and complex environment and therefore takes longer to learn
than IMFORT, but it provides all the facilities needed by large applications
hence is \fIeasier\fR to use than simpler interfaces like IMFORT, if one is
faced with the already difficult task of coding a large program or package.
Furthermore, the SPP/VOS environment fully addresses the problems of
portability and device independence, critical issues for applications which
must be supported and used simultaneously on a range of machines over a
period of years, during which time the software is likely to be continually
evolving.  An overview of the SPP/VOS programming environment is given in
\fIThe IRAF Data Reduction and Analysis System\fR, February 1986, by the author.
.PP
In summary, IMFORT is intended for use to interface old Fortran programs to
IRAF with a minimum of effort, and as an entry level programming environment
which new users can learn to use in a few hours.  Experienced users,
professional programmers, and developers of large applications will find that
they can accomplish more with less effort once they have learned to use the
more fully featured SPP/VOS programming environment.

.bp
.NH
Getting Started
.PP
Although programs which use IMFORT can and often will be invoked from the
host system command interpreter, it is likely that such programs will also
be used interactively in combination with the tasks provided by the standard
IRAF system.  For example, the IRAF graphics and image display facilities
are likely to be used to examine the results of an image operation performed
by a user written Fortran/IMFORT program.  Indeed, the standard IRAF tasks
are likely to be used for testing new IMFORT programs as well as reducing data
with old ones, so we shall assume that software development will take place
from within the IRAF environment.  Since IRAF provides full access to the
facilities of the host system at all times, there is little reason not to
work from within the IRAF environment.
.PP
As a first step, let's see what is required to enter, compile, link, and
execute a small Fortran program which does nothing more than print the
message \fLhello, world!\fR on the terminal.  We shall assume that the
reader has read the \fICL User's Guide\fR and is already familiar with
basic CL command entry, the OS escape facility, the editor interface and so on.
The first step is to call up the editor to enter the program into a file:
.DS
\fLcl> edit hello.f\fR
.DE
Note that the filename extension is ".f", which is what IRAF uses for
Fortran files.  The extension will be mapped into the local host system
equivalent when IRAF communicates with the host system, but when working
in the IRAF environment the IRAF name should be used.
.LP
Once in the editor, enter the following program text:
.DS
\fLprogram hello
write (*,*) 'hello, world!'
stop
end\fR
.DE
The next step is to compile and link the \fLhello\fR program.  This is done
by the command \fIfc\fR (\fIf\fRortran-\fIc\fRompile), which produces an
object file \fLhello.o\fR and an executable program file \fLhello.e\fR.
Note that the \fIfc\fR task is defined in the default \fIuser\fR package in
your \fLLOGIN.CL\fR file, hence a \fImkiraf\fR may be required to regenerate
the \fLLOGIN.CL\fR file if the file is old or has been modified.
.DS
\fLcl> fc hello.f\fR
.DE
Since the \fLhello\fR program is a host Fortran program, it can be executed
immediately with an OS escape, e.g., \fL!hello.e\fR on UNIX, or
\fL!run hello\fR on VMS.  A better approach if the task has command line
arguments is to use the IRAF \fIforeign task\fR facility to define the
program as a new IRAF task, as we shall see in the next section.

.NH 2
Example 1:  Plotting a function
.PP
As a slightly more complicated example, let's construct a program to compute
and plot a function using command line arguments to input the function
parameters, with output consisting of a simple ASCII table sampling the
computed function.  Our example computes the Planck function, which gives the
emissivity of a blackbody as a function of wavelength and temperature.
The sample program is shown in Figure 1.  Source code for this and all other
examples in this paper may be found in the IRAF directory \fLimfort$tasks\fR.

.DS
\fL
.ps 8
.vs 9p
c PLANCK -- Compute the Planck blackbody radiation distribution for a
c given temperature and wavelength region.
c
c       usage:  planck temperature lambda1 lambda2
c
c The temperature is specified in degrees Kelvin and the wavelength
c region in microns (1u=10000A).  100 [x,y] data points defining the
c curve are output.
c ----------------------------------------------------------------------

        program planck

        character*80    errmsg
        integer         nargs, ier, i
        real            w1, w2, dw, cm, t
        real            xv(100), yv(100)

c --- Get the temperature in degrees kelvin.
        call clargr (1, t, ier)
        if (ier .ne. 0) then
            write (*, '('' temperature (degrees kelvin): '',$)')
            read (*,*) t
        endif

c --- Get the wavelength region to be computed.
        call clnarg (nargs)
        if (nargs .ge. 3) then
            call clargr (2, w1, ier)
            if (ier .ne. 0) goto 91
            call clargr (3, w2, ier)
            if (ier .ne. 0) goto 91
        else
            write (*, '('' start wavelength (microns): '',$)')
            read (*,*) w1
            write (*, '('' end wavelength (microns): '',$)')
            read (*,*) w2
        endif

c --- Compute the blackbody curve.
        dw = (w2 - w1) / 99.0
        do 10 i = 1, 100
            xv(i) = ((i-1) * dw) + w1
            cm = xv(i) * 1.0E-4
            yv(i) = (3.74185E-5 * (cm ** -5)) /
     *          (2.71828 ** (1.43883 / (cm * t)) - 1.0)
 10     continue

c --- Print the curve as a table.
        do 20 i = 1, 100
            write (*, '(f7.4, g12.4)') xv(i), yv(i)
 20     continue

        stop

c --- Error exit. 
 91     call imemsg (ier, errmsg)
        write (*, '('' Error: '', a80)') errmsg
        stop
        end\fR
.DE
.vs
.ps
.sp
.ce
Figure 1.  Sample program to compute the Planck function\(dd
.FS
\(ddThe trailing \fL$\fR carriage control code used in the format strings in
the \fLWRITE\fR statements in this and the other sample Fortran programs
is nonstandard Fortran and may not be available on all host machines.
Its function is to defeat the carriage-return linefeed so that the user's
response may be entered on the same line as the prompt.
.FE

.PP
This example serves to demonstrate the use of the IMFORT \fIclarg\fR procedures
to fetch the command line arguments, and the use of i/o redirection to capture
the output to generate the plot.  The command line to an IMFORT program consists
of a sequence of arguments delimited by spaces or tabs.  The subroutine
\fIclnarg\fR returns the number of arguments present on the command line when
the task was called.  The \fIclargr\fR, \fIclargi\fR, etc. procedures fetch
and decode the values of the individual arguments.  Virtually all IMFORT
procedures include an integer output variable \fIier\fR in their argument list;
a zero status indicates success, anything else indicates failure and the actual
error code identifies the cause of the problem.  The \fIimemsg\fR procedure
may be called to convert IMFORT error codes into error message strings, as in
the example.
.PP
Once the program has been entered and compiled and linked with \fIfc\fR,
we must declare the program as a foreign task to the CL.  If this is not
done the program can still be run via an OS escape, but none of the advanced
CL features will be available, e.g., background execution, command line
expression evaluation, i/o redirection, and so on.  The technique used to
declare a foreign task is machine dependent since it depends upon the syntax
of the host command interpreter.  For example, to declare the new CL foreign
task \fIplanck\fR on a UNIX system, we enter the following command:
.DS
\fLcl> task $planck = $planck.e\fR
.DE
The same thing can be achieved on a VMS system with the following
declaration (it can be simplified by moving the VMS foreign task declaration
to your \fLLOGIN.COM\fR file):
.DS
\fLcl> task $planck = "$planck:==\\\\$\fIdisk\fP:[\fIdir\fP...]planck.exe!planck"\fR
.DE
The \fL$\fR characters are required to tell the CL that the new task does
not have a parameter file, and is a foreign task rather than a regular
IRAF task.  The \fL!\fR in the VMS example is used to delimit multiple DCL
commands; the command shown defines the DCL foreign task \fIplanck\fR and
then executes it.  The use of the \fItask\fR statement to declare foreign
tasks is discussed in detail in \(sc3.3.
.PP
We have written the program in such a way that the arguments will be queried
for if not given on the command line, so if we enter only the name of the
command, an interaction such as the following will occur:
.DS
\fLcl> planck
temperature (degrees kelvin): 3000
start wavelength (microns): .1
end wavelength (microns): 4\fR
.DE
Note that if the output of the \fIplanck\fR task is redirected this input
mechanism will \fInot\fR work, since the queries will be redirected along
with the output.  Hence if we use a pipe to capture the output, as in the
following example, the arguments must be given on the command line.
.DS
\fLcl> planck 3000 0.1 4.0 | graph
.DE
This command will compute and plot the emissivity for a 3000 degree kelvin
blackbody from 0.1 to 4.0 microns (1000 to 40000 angstroms).
.PP
An interesting alternative way to implement the above program would be to
output the function curve as a line in an image, rather than as a table of
numbers.  For example, a two dimensional image could be generated wherein
each line corresponds to a different temperature.  \fIGraph\fR or \fIimplot\fR
could then be used to plot curves or overplot families of curves; this would
be more efficient than the technique employed in our sample program.
Image access via IMFORT is illustrated in our next example.

.NH 2
Example 2:  Compute the range of pixel values in an image
.PP
The program shown in Figure 2 opens the named image, examines each line in
the image to determine the minimum and maximum pixel values, keeping a running
tally until the entire image has been examined (there is no provision for
detecting and ignoring bad pixels in the image).  The newly computed minimum
and maximum pixel values are then updated in the image header as well as
printed on the standard output.
.DS
\fL
.ps 8
.vs 9p
c MINMAX -- Compute the minimum and maximum pixel values in an image.
c The new values are printed as well as updated in the image header.
c
c       usage:  minmax image
c ----------------------------------------------------------------------

        program minmax

        character*80    image, errmsg
        real            pix(4096), dmin, dmax, vmin, vmax
        integer         im, axlen(7), naxis, dtype, ier, j

c --- Get image name.
        call clargc (1, image, ier)
        if (ier .ne. 0) then
            write (*, '('' enter image name: '',$)')
            read (*,*) image
        endif

c --- Open the image for readwrite access (we need to update the header).
        call imopen (image, 3, im, ier)
        if (ier .ne. 0) goto 91
        call imgsiz (im, axlen, naxis, dtype, ier)
        if (ier .ne. 0) goto 91

c --- Read through the image and compute the limiting pixel values.
        do 10 j = 1, axlen(2)
            call imgl2r (im, pix, j, ier)
            if (ier .ne. 0) goto 91
            call alimr (pix, axlen(1), vmin, vmax)
            if (j .eq. 1) then
                dmin = vmin
                dmax = vmax
            else
                dmin = min (dmin, vmin)
                dmax = max (dmax, vmax)
            endif
 10     continue

c --- Update the image header.
        call impkwr (im, 'datamin', dmin, ier)
        if (ier .ne. 0) goto 91
        call impkwr (im, 'datamax', dmax, ier)
        if (ier .ne. 0) goto 91

c --- Clean up.
        call imclos (im, ier)
        if (ier .ne. 0) goto 91
        write (*, '(a20, 2 g12.5)') image, dmin, dmax
        stop

c --- Error exit.
 91     call imemsg (ier, errmsg)
        write (*, '('' Error: '', a80)') errmsg
        stop
        end\fR
.DE
.vs
.ps
.sp
.ce
Figure 2.  Compute the min and max pixel values in an image

.PP
The program as written can only deal with images of one or two dimensions,
of pixel type short (16 bit integer) or real (32 bit floating), with a line
length not to exceed 4096 pixels per line.  We could easily change the program
to deal with images of up to three dimensions, but the IMFORT interface does
not provide dynamic memory allocation facilities so there is always going
to be an upper limit on the line length if we use the simple get line i/o
procedure \fIimgl2r\fR, as shown.  The use of fixed size buffers simplifies
the program, however, and is not expected to be a serious problem in most
IMFORT applications.
.PP
The \fIalimr\fR subroutine in the previous example is from the IRAF VOPS
(vector operators) package.  The function of \fIalimr\fR is to compute
the limiting (min and max) pixel values in a vector of type real, the
function type being indicated by the \fIlim\fR, and the pixel datatype by
the \fIr\fR.  The VOPS package provides many other such vector operators,
and is discussed further in \(sc 4.4.

.NH 2
Example 3:  Copy an image
.PP
Our final example (Figure 3) shows how to create a new image as a copy of
some existing image.  This can be used as a template to create any binary
image operator, i.e., any program which computes some transformation upon
an existing image, writing a new image as output.
.PP
By now the functioning of this procedure should be self evident.
The only thing here which is at all subtle is the subroutine \fIimopnc\fR,
used to open (create) a new copy of an existing image.  The open new copy
operation creates a new image the same size and datatype as the old
image, and copies the image header of the old image to the new image.
Any user keywords in the header of the old image will be automatically
passed to the new image, without requiring that the calling program
have explicit knowledge of the contents of the image header.
.PP
Note that the program is written to work only on pixels of type real,
hence will be inefficient if used to copy images of type short-integer.
A more efficient approach for a general image copy operator would be
to add a conditional test on the variable \fLdtype\fR, executing a
different copy-loop for each datatype, to avoid having to convert from
integer to real and back again when copying a short-integer image.
The short-integer equivalents of \fIimgl3r\fR (get line, 3 dim image,
type real) and \fIimpl3r\fR (put line, 3 dim image, type real) are
called \fIimgl3s\fR and \fIimpl3s\fR.
.PP
The program as written will work for images of up to three dimensions,
even though it is written to deal with only the three dimensional case.
This works because the length of the "unused" axes in an image is
set to one when the image is created.  A program passed an image of
higher dimension than it is written for will also work, but will not
process all of the data.  IMFORT does not support image sections,
so only the first few lines of the image will be accessible to such
a program.

.PP
Additional useful examples of Fortran programs using IMFORT are given in
\fLimfort$tasks\fR.  These include utility programs to make test images,
print the contents of an image header, print the values of the pixels in
a subraster, and so on.  You may wish to copy the source for these to your
own workspace for use as is, or for use as templates to construct similar
programs.

.DS
\fL
.ps 8
.vs 9p
c IMCOPY -- Copy an image.  Works for images of up to three dimensions
c with a pixel type of short or real and up to 4096 pixels per line.
c
c       usage:  imcopy oldimage newimage
c ---------------------------------------------------------------------

        program imcopy

        real            pix(4096)
        character*80    oimage, nimage, errmsg
        integer         ncols, nlines, nbands, j, k, oim, nim
        integer         ier, axlen(7), naxis, dtype, nargs

c --- Get command line arguments.
        call clnarg (nargs)
        if (nargs .eq. 2) then
            call clargc (1, oimage, ier)
            if (ier .ne. 0) goto 91
            call clargc (2, nimage, ier)
            if (ier .ne. 0) goto 91
        else
            write (*, '('' input image: '',$)')
            read (*,*) oimage
            write (*, '('' output image: '',$)')
            read (*,*) nimage
        endif

c --- Open the input image and create a new-copy output image.
        call imopen (oimage, 1, oim, ier)
        if (ier .ne. 0) goto 91
        call imopnc (nimage, oim, nim, ier)
        if (ier .ne. 0) goto 91

c --- Determine the size and pixel type of the image being copied.
        call imgsiz (oim, axlen, naxis, dtype, ier)
        if (ier .ne. 0) goto 91

        ncols  = axlen(1)
        nlines = axlen(2)
        nbands = axlen(3)

c --- Copy the image.
	do 15 k = 1, nbands
	    do 10 j = 1, nlines
		call imgl3r (oim, pix, j, k, ier)
		if (ier .ne. 0) goto 91
		call impl3r (nim, pix, j, k, ier)
		if (ier .ne. 0) goto 91
 10         continue
 15     continue

c --- Clean up.
        call imclos (oim, ier)
        if (ier .ne. 0) goto 91
        call imclos (nim, ier)
        if (ier .ne. 0) goto 91

        stop

c --- Error actions.
 91     call imemsg (ier, errmsg)
        write (*, '('' Error: '', a80)') errmsg
        stop
        end\fR
.DE
.vs
.ps
.sp
.ce
Figure 3.  Image copy program

.bp
.NH
The IMFORT Programming Environment
.PP
IRAF provides a small programming environment for the development of host
Fortran programs using the IMFORT interface.  This environment consists
of the general CL tools, e.g., the editor, the \fIpage\fR and \fIlprint\fR
tasks, etc., plus a few special tools, namely, the \fIfc\fR compile/link
utility and the foreign task facility.  In this section we discuss these
special tools and facilities.  Information is also provided for linking
to the IMFORT libraries if program development is to take place at the host
system level.
.PP
The classic third generation program development cycle (ignoring such minor
details as designing the software) is edit \(em compile/link \(em debug.
The edit phase uses the CL \fIedit\fR task, an interface to the host system
editor of choice.  The compile/link phase is performed by the \fIfc\fR utility.
The debug phase is optional and is generally only necessary for large programs.
The host system debug tool is used; while IRAF does not provide a special
interface to the host debug tool, one can easily be constructed using the
foreign task facility if desired.
.PP
Programs which use the IMFORT interface are inevitably host system dependent
to some degree, since they are host programs.  In the interests of providing
the user with concrete examples, the discussion in this section must therefore
delve into the specifics of certain host operating systems.  We have chosen
to use UNIX and VMS in the examples, since most IRAF implementations run on
one or the other of these operating systems.  The ties between the IMFORT
programming environment and the host system are quite simple, however, so it
should not be difficult to see how to modify the examples for a different host.

.NH 2
The FC Compile/Link Utility
.PP
The \fIfc\fR utility provides a consistent, machine independent interface to
the host system compiler and linker which is convenient and easy to use.
In addition, \fIfc\fR provides a means for linking host programs with the
IRAF libraries without having to type a lot, and without having to build
host command scripts.  All of the IRAF libraries are accessible via \fIfc\fR,
not just IMFORT (\fLlib$libimfort.a\fR) and the IRAF system libraries used
by IMFORT, but all the other IRAF libraries as well, e.g., the math libraries.
.PP
The default action of \fIfc\fR is to compile and link the files listed on the
command line, i.e., source files in various languages, object modules, and
libraries.  Any source files are first turned into object modules, then the
objects are linked in the order given, searching any libraries in the order
in which they are encountered on the command line (the IMFORT libraries are
searched automatically, after any libraries listed on the command line).
By default, the root name of the new executable will be the same as that of
the first file listed on the command line; a different name may be assigned
with the \fI-o\fR switch if desired.
.LP
The syntax of the \fIfc\fR command is as follows:
.DS
\fLfc [\fIswitches\fP] \fIfile\fL [\fIfile ...\fL] [-o \fIexefile\fL]\fR
.DE
The most interesting switches are as follows:
.in 0.5i
.IP \fB-c\fR
Compile but do not link.
.IP \fB-l\fIlibrary\fR
.br
Link to the named IRAF library.  On a UNIX host this switch may also be
used to reference the UNIX libraries.  The \fI-llibrary\fR reference
should be given in the file list at the point at which you want the
library to be searched.  The \fI-l\fR causes \fIfc\fR to look in a set
of standard places for the named library; user libraries should be
referenced directly by the filename of the library.
.IP \fB-o\fR\ \fIexefile\fR
.br
Override the default name for the executable file produced by the linker.
.IP \fB-x\fR
Compile and link for debugging.
.in

.LP
Since the \fIfc\fR command line can contain many different types of objects,
a filename extension is required to identify the object type.  The IRAF
filename extensions \fImust\fR be used; these are listed in the table below.

.TS
box center;
cb s
ci | ci
c | l.
IRAF Filename Extensions
_
extn	usage
=
\.a	object library
\.c	C source file
\.e	executable
\.f	Fortran source file
\.o	object module
\.s	Assembler source file
\.x	SPP source file
.TE

.PP
The \fIfc\fR utility is easy to learn and use.  Here are a few examples
illustrating the most common usage of the utility.  To compile and link the
Fortran program \fLprog.f\fR, producing the executable program \fLprog.e\fR:
.DS
\fLcl> fc prog.f\fR
.DE
To compile the file \fLutil.f\fR to produce the object \fLutil.o\fR,
without linking anything:
.DS
\fLcl> fc -c util.f\fR
.DE
To link \fLprog.o\fR and \fLutil.o\fR, producing the executable program
\fLprog.e\fR:
.DS
\fLcl> fc prog.o util.o\fR
.DE
To do the same thing, producing an executable named \fLfoo.e\fR instead
of \fLprog.e\fR:
.DS
\fLcl> fc prog.o util.o -o foo.e\fR
.DE
To compile and link \fLprog.f\fR for debugging:
.DS
\fLcl> fc -x prog.f\fR
.DE
To link \fLprog.o\fR with the IRAF library \fLlib$libdeboor.a\fR (the DeBoor
spline package), producing the executable \fLprog.e\fR as output:
.DS
\fLcl> fc prog.o -ldeboor\fR
.DE
To do the same thing, spooling the output in the file \fLspool\fR and
running the whole thing in the background:
.DS
\fLcl> fc prog.o -ldeboor >& spool &\fR
.DE
To link instead with the library \fLlibfoo.a\fR, in the current directory
(note that in this case the library is a module and not a switch):
.DS
\fLcl> fc prog.o libfoo.a\fR
.DE
.LP
Just about any combination of switches and modules that makes sense will
work.  The order of libraries in the argument list is important, as they
will be searched in the order in which they are listed on the command line.
.PP
The \fIfc\fR utility is actually just a front-end to the standard IRAF
compiler \fIxc\fR, as we shall see in \(sc3.3.  See the manual page for
\fIxc\fR for additional information.

.NH 2
Host Level Linking to the IMFORT Libraries
.PP
In some cases it may be desirable to use host system facilities to compile
and link programs which use the IMFORT interface.  The procedure for doing
this is host dependent and is completely up to the user, who no doubt will
already have a preferred technique worked out.  All one needs to know in
this situation are the names of the libraries to be linked, and the order
in which they are to be linked.  The libraries are as follows, using the
IRAF filenames for the libraries.  All the libraries listed are referenced
internally by the IMFORT code hence are required.

.TS
center;
l l.
lib$libimfort.a	IMFORT itself
lib$libsys.a	Contains certain pure code modules used by IMFORT
lib$libvops.a	The VOPS vector operators library
hlib$libos.a	The IRAF kernel (i/o primitives)
.TE
.LP
The host pathnames of these libraries will probably be evident, given the
host pathname of the IRAF root directory (\fIlib\fR is a subdirectory of
the IRAF root directory).  If in doubt, the \fIosfn\fR intrinsic function may
be used while in the CL to print the host pathname of the desired library.
For example,
.DS
\fLcl> = osfn ("lib$libimfort.a")\fR
.DE
will cause the CL to print the host pathname of the main IMFORT library.

.NH 2
Calling Host Programs from the CL
.PP
Since Fortran programs which use IMFORT are host programs rather than IRAF
programs, the CL \fIforeign task\fR interface is used to connect the programs
to the CL as CL callable tasks.  The foreign task interface may also be used
to provide custom CL task interfaces to other host system utilities, e.g.,
the debugger or the librarian.
.PP
The function of the \fItask\fR statement in the CL is to make a new task
known to the CL.  The CL must know the name of the new task, the name of
the package to which it is to be added, whether or not the new task has a
parameter file, the type of task being defined, and the name of the file in
which the task resides.  At present new tasks are always added to the
"current" package.  The possible types of tasks are normal IRAF executable
tasks, CL script tasks, and foreign tasks.  Our interest here is only in the
forms of the task statement used to declare foreign tasks.  There are two
such forms at present.  The simplest is the following:
.DS
\fLtask $\fItaskname\fR [, \fL$\fItaskname\fR...]\fL = $foreign\fR
.DE
This form is used when the command to be sent to the host system to run
the task is identical to the name by which the task is known to the CL.
Note that any number of new tasks may be declared at one time with this
form of the task statement.  The \fL$\fR prefixing each \fItaskname\fR
tells the CL that the task does not have a parameter file.  The \fL$foreign\fR
tells the CL that the new tasks are foreign tasks and that the host command
is the same as \fItaskname\fR.  For example, most systems have a system
utility \fImail\fR which is used to read or send electronic mail.
To declare the \fImail\fR task as an IRAF foreign task, we could enter
the following declaration, and then just call the \fImail\fR task from
within the CL like any other IRAF task.
.DS
\fLtask $mail = $foreign\fR
.DE
The more general form of the foreign task statement is shown below.
The host command string must be quoted if it contains blanks or any other
special characters; \fL$\fR is a reserved character and must be escaped
to be included in the command sent to the host system.
.DS
\fLtask $\fItaskname\fL = $\fIhost_command_string\fR
.DE
In this form of the task statement, the command to be sent to the host system
to execute the new IRAF task may be any string.  For example, on a VMS host,
we might want to define the \fImail\fR task so that outgoing messages are
always composed in the editor.  This could be set up by adding the \fL/EDIT\fR
switch to the command sent to VMS:
.DS
\fLtask $mail = $mail/edit\fR
.DE
Foreign task statements which reference user-written Fortran programs often
refer to the program by its filename.  For the task to work regardless of the
current directory, either the full pathname of the executable file must be
given, or some provision must be made at the host command interpreter level
to ensure that the task can be found.
.PP
When a foreign task is called from the CL, the CL builds up the command string
to be sent to the host command interpreter by converting each command line
argument to a string and appending it to \fIhost_command_string\fR preceded
by a space.  This is the principal difference between the foreign task
interface and the low level OS escape facility: in the case of a foreign task,
the command line is fully parsed, permitting general expression evaluation,
i/o redirection, background execution, minimum match abbreviations, and so on.
.PP
In most cases this simple method of composing the command to be sent to the
host system is sufficient.  There are occasional cases, however, where it is
desirable to \fIembed\fR the command line arguments somewhere in the string
to be sent to the host system.  A special \fIargument substitution\fR notation
is provided for this purpose.  In this form of the task statement,
\fIhost_command_string\fR contains special symbols which are replaced by the
CL command line arguments to form the final host command string.
These special symbols are defined in the table below.

.TS
center;
c l.
$0	replaced by \fItaskname\fR
$1, $2, ..., $9	replaced by the indicated argument string
$\(**	replaced by the entire argument list
$(N)	use host equivalent of filename argument N (1-9 or \(**)
.TE

.PP
An example of this form of the task statement is the \fIfc\fR task discussed
in \(sc3.1.  As we noted earlier, \fIfc\fR is merely a front-end to the more
general IRAF HSI command/link utility \fIxc\fR.  In fact, \fIfc\fR is
implemented as a foreign task defined in the default \fIuser\fR package in
the \fLLOGIN.CL\fR file.  The task declaration used to define \fIfc\fR is
shown below.  The task statement shown is for UNIX; the VMS version is
identical except that the \fL-O\fR switch must be quoted else DCL will convert
it to lower case.  In general, foreign task statements are necessarily
machine dependent, since their function is to send a command to the host system.
.DS
\fLtask $fc = "$xc -h -O $\(** -limfort -lsys -lvops -los"\fR
.DE
The argument substitution facility is particularly useful when the host
command template consists of several statements to be executed by the
host command interpreter in sequence each time the CL foreign task is called.
In this case, a delimiter character of some sort is required to delimit
the host command interpreter statements.  Once again, this is host system
dependent, since the delimiter character to be used is defined by the syntax
of the host command interpreter.  On UNIX systems the command delimiter
character is semicolon (`\fB;\fR').  VMS DCL does not allow multiple
statements to be given on a single command line, but the IRAF interface
to DCL does, using the exclamation character (`\fB!\fR'), which is the
comment character in DCL.
.PP
The \fL$()\fR form of argument substitution is useful for foreign tasks
with one or more filename arguments.  The indicated argument or arguments
are taken to be IRAF virtual filenames, and are mapped into their host
filename equivalents to build up the host command string.  For example,
assume that we have an IMFORT task \fIphead\fR, the function of which is
to print the header of an image in FITS format on the standard output
(there really is such a program - look in \fLimfort$tasks/phead.f\fR).
We might declare the task as follows (assuming that \fIphead\fR means
something to the host system):
.DS
\fLtask $phead = "$phead $(*)"\fR
.DE
We could then call the new task from within the CL to list the header
of, for example, the standard test image \fLdev$pix\fR, and page the output:
.DS
\fLcl> phead dev$pix | page\fR
.DE
Or we could direct the output to the line printer:
.DS
\fLcl> phead dev$pix | lpr\fR
.DE
Filename translation is available for all forms of argument substitution
symbols, e.g., \fL$(1)\fR, \fL$(2)\fR, \fL$(\(**)\fR, and so on; merely
add the parenthesis.
.PP
It is suggested that new foreign task statements, if not typed in
interactively, be added to the \fIuser\fR package in your \fLLOGIN.CL\fR file,
so that the definitions are not discarded when you log out of the CL or exit
a package.  If you want to make the new tasks available to other IRAF users
they can be added to the \fIlocal\fR package by adding the task statements
to the file \fLlocal$tasks/local.cl\fR.  If this becomes unwieldy the
next step is to define a new package and add it to the system; this is not
difficult to do, but it is beyond the scope of this manual to explain how
to do so.

.NH 3
Example 1 Revisited
.PP
Now that we are familiar with the details of the foreign task statement,
it might be useful to review the examples of foreign task statements given
in \(sc2.1, which introduced the \fIplanck\fR task.  The UNIX example given
was as follows:
.DS
\fLcl> task $planck = $planck.e\fR
.DE
This is fine, but only provided the \fIplanck\fR task is called from the
directory containing the executable.  To enable the executable to be called
from any directory we can use a UNIX pathname instead, e.g.,
.DS
\fLcl> task $planck = $/usr/jones/iraf/tasks/planck.e\fR
.DE
Alternatively, one could place all such tasks in a certain directory, and
either define the pathname of the directory as a shell environment variable
to be referenced in the task statement, or include the task's directory in
the shell search path.  There are many other possibilities, of course, but
it would be inappropriate to enumerate them here.
.LP
The VMS example given earlier was the following:
.DS
\fLcl> task $planck = "$planck:==\\\\$\fIdisk\fP:[\fIdir\fP...]planck.exe!planck"\fR
.DE
The command string at the right actually consists of two separate DCL commands
separated by the VMS/IRAF DCL command delimiter `\fB!\fR'.  If we invent a
pathname for the executable, we can write down the the first command:
.DS
\fL$ planck :== $usr\\\\$2:[jones.iraf.tasks]planck.exe\fR
.DE
This is a DCL command which defines the new DCL foreign task \fIplanck\fR.
We could shorten the CL foreign task statement by moving the DCL declaration
to our DCL \fLLOGIN.COM\fR file; this has the additional benefit of allowing
the task to be called directly from DCL, but is not as self-contained.
If this were done the CL task statement could be shortened to the following.
.DS
\fLcl> task $planck = $foreign\fR
.DE
The same thing could be accomplished in Berkeley UNIX by defining a cshell
\fIalias\fR for the task in the user's \fL.cshrc\fR file.

.NH 2
Debugging IMFORT Programs
.PP
Programs written and called from within the IRAF environment can be debugged
using the host system debug facility without any inconvenience.  The details
of how to use the debugger are highly dependent upon the host system since
the debugger is a host facility, but a few examples should help the reader
understand what is involved.
.PP
Berkeley UNIX provides two debug tools, the assembly language debugger
\fIadb\fR and the source language debugger \fIdbx\fR.  Both are implemented
as UNIX tasks and are called from within the IRAF environment as tasks,
with the name of the program to be debugged as a command line argument
(this example assumes that \fIadb\fR is a defined foreign task):
.DS
\fLcl> adb planck.e\fR
.DE
The program is then run with a debugger command, passing any command line
arguments to the program as part of the debugger run-program command.
Programs do not have to be compiled in any special way to be debugged
with \fIadb\fR; programs should be compiled with \fIfc -x\fR to be debugged
with \fIdbx\fR.
.PP
In VMS, the debugger is not a separate task but rather a shareable image
which is linked directly into the program to be debugged.  To debug a program,
the program must first be linked with \fIfc -x\fR.  The program is then run
by simply calling it in the usual way from the CL, with any arguments given
on the command line.  When the program runs it comes up initially in the
debugger, and a debugger command (\fIgo\fR) is required to execute the user
program.  Note that if the program is run directly with \fLrun/debug\fR
there is no provision for passing an argument list to the task.

.NH 2
Calling IMFORT from Languages other than Fortran
.PP
Although our discussion and examples have concentrated exclusively on the
use of the IMFORT library in host Fortran programs, the library is in fact
language independent, i.e., it uses only low level, language independent
system facilities and can therefore be called from any language available
on the host system.  The method by which Fortran subroutines and functions
are called from another language, e.g., C or assembler, is highly machine
dependent and it would be inappropriate for us to go into the details here.
Note that \fIfc\fR may be used to compile and link C or assembler programs
as well as Fortran programs.

.NH 2
Avoiding Library Name Collisions
.PP
Any program which uses IMFORT is being linked against the main IRAF system
libraries, which together contain some thousands of external procedure names.
Only a few hundred of these are likely to be linked into a host program,
but there is always the chance that a user program module will have the
same external name as one of the modules in the IRAF libraries.
If such a library collision should occur, at best one would get an error
message from the linker, and at worst one would end up with a program
which fails mysteriously at run time.
.PP
At present there is no utility which can search a user program for externals
and cross check these against the list of externals in the IRAF system
libraries.  A database of external names is however available in the file
\fLlib$names\fR; this contains a sorted list of all the Fortran callable
external names defined by procedures in the \fIimfort\fR, \fIex\fR, \fIsys\fR,
\fIvops\fR, and \fIos\fR libraries (the \fIex\fR library is however not
searched when linking IMFORT programs).
.PP
The \fImatch\fR task may be used to check individual user external names
against the name list, or a host utility may be used for the same purpose.
For example, to determine if the module \fIsubnam\fR is present in any
of the IRAF system libraries:
.DS
\fLcl> match subnam lib$names\fR
.DE
The names database is also useful for finding the names of all the procedures
sharing a particular package prefix.  For example,
.DS
\fLcl> match "^cl" lib$names | table\fR
.DE
will find all the procedures whose names begin with the prefix "cl" and
print them as a table (the \fIlists\fR package must be loaded first).

.bp
.NH
The IMFORT Library
.PP
In this section we survey the procedures provided by the IMFORT interface,
grouped according to the function they perform.  There are currently four main
groups: the command line access procedures, the image access procedures,
the vector operators (VOPS), and a small binary file i/o package.  With the
exception of the VOPS procedures, all of the IMFORT routines were written
especially for IMFORT and are not called in standard IRAF programs.
The VOPS procedures are standard IRAF procedures, but are included in the
IMFORT interface because they are coded at a sufficiently low level that they
can be linked into any program, and they tend to be useful in image processing
applications such as IMFORT is designed for.
.PP
The ANSI Fortran-77 standard requires that all names in Fortran programs have
six or fewer characters.  To eliminate guesswork, the names of all the IMFORT
procedures are exactly six characters long and the names adhere to a
\fBnaming convention\fR.  The first one or two characters in each name
identify the package or group to which the procedure belongs, e.g.,
\fIcl\fR for the command line access package, \fIim\fR for the image
access package, and so on.  The package prefix is followed by the function name,
and lastly a datatype code identifying the datatype upon which the procedure
operates, in cases where multiple versions of the procedure are available for
a range of datatypes.
.DS
\fIpackage_prefix // function_code // type_suffix\fR
.DE
The type suffix codes have already been introduced in the examples.  They are
the same as are used throughout IRAF.  The full set is \fB[bcsilrdx]\fR, as
illustrated in the following table (not all are used in the IMFORT procedures).

.TS
center box;
cb s s s
ci | ci | ci | ci
c | l | c | l.
Standard IRAF Datatypes
_
suffix	name	code	typical fortran equivalent
=
b	bool	1	\fLLOGICAL\fR
c	char	2	\fLINTEGER\(**2\fR (non-ANSI)
s	short	3	\fLINTEGER\(**2\fR (non-ANSI)
i	int	4	\fLINTEGER\fR
l	long	5	\fLINTEGER\(**4\fR (non-ANSI)
r	real	6	\fLREAL\fR
d	double	7	\fLDOUBLE PRECISION\fR
x	complex	8	\fLCOMPLEX\fR
.TE
.PP
The actual mapping of IRAF datatypes into host system datatypes is machine
dependent, i.e., \fIshort\fR may not map into INTEGER\(**2 on all machines.
This should not matter since the datatype in which data is physically stored
internally is hidden from user programs by the IMFORT interface.
.PP
In cases where multiple versions of a procedure are available for operands
of different datatypes, a special nomenclature is used to refer to the class
as a whole.  For example,
.DS
\fLclarg[cird] (argno, [cird]val, ier)\fR 
.DE
denotes the set of four procedures \fIclargc, clargi, clargr\fR, and
\fIclargd\fR.  The datatype of the output operand (\fIcval, ival\fR, etc.)
must match the type specified by the procedure name.
.PP
With the exception of the low level binary file i/o procedures (BFIO),
all IMFORT procedures are implemented as subroutines rather than functions,
for reasons of consistency and to avoid problems with mistyping of undeclared
functions by the Fortran compiler.

.NH 2
Command Line Access
.PP
The command line access procedures are used to decode the arguments present
on the command line when the IMFORT program was invoked.  This works both
when the program is called from the IRAF CL, and when the program is called
from the host system command interpreter.  The command line access procedures
are summarized in Figure 4, below.

.TS
center;
n.
\fLclnarg (\&nargs)\fR
\fLclrawc (\&outstr, ier)\fR
\fLclarg[cird] (\&argno, [cird]val, ier)\fR
.TE
.sp
.ce
Figure 4.  Command Line Access Procedures

.PP
The \fIclnarg\fR procedure returns the number of command line arguments;
zero is returned if an error occurs or if there were no command line arguments.
The \fIclargc\fR, \fIclargi\fR, etc., procedures are used to fetch and decode
the individual arguments; \fIclargc\fR returns a character string, \fIclargi\fR
returns an integer, and so on.  A nonzero \fIier\fR status indicates either
that the command line did not contain the indexed argument, or that the
argument could not be decoded in the manner specified.  Character string
arguments must be quoted on the command line if they contain any blanks or
tabs, otherwise quoting is not necessary.  The rarely used \fIclrawc\fR
procedure returns the entire raw command line as a string.

.NH 2
Image Access
.PP
The image access procedures form the bulk of the IMFORT interface.  There are
three main categories of image access procedures, namely, the general image
management procedures (open, close, create, get size, etc.), the header access
procedures (used to get and put the values of header keywords), and the pixel
i/o procedures, used to read and write image data.
.PP
IMFORT currently supports images of up to three dimensions,
of type short-integer or real.
There is no builtin limit on the size of an image, although
the size of image a particular program can deal with is normally limited by
the size of a statically allocated buffer in the user program.  IMFORT does
not map IRAF virtual filenames, hence host dependent names must be used when
running a program which uses IMFORT.
.PP
IMFORT currently supports only the OIF image format, and images must be
of type short-integer or real.  Since normal IRAF programs support images of
up to seven disk datatypes with a dimensionality of up to seven, as well as
completely different image formats than that expected by IMFORT (e.g., STF),
if you are not careful IRAF can create images which IMFORT programs cannot
read (don't omit the error checking!).  In normal use, however,
types short-integer and real are by far the most common and images with
more than two dimensions are rare, so these are not expected to be serious
limitations.

.NH 3
General Image Access Procedures
.PP
The general image access and management procedures are listed in Figure 5.
An image must be opened with \fIimopen\fR or \fIimopnc\fR before header access
or pixel i/o can occur.  The image open procedures return an
\fIimage descriptor\fR (an integer magic number) which uniquely identifies
the image in all subsequent accesses until the image is closed.
When the operation is completed, an image must be closed with \fIimclos\fR to
flush any buffered output, update the image header, and free any resources
associated with the image descriptor.  The maximum number of images which
can be open at any one time is limited by the maximum number of open file
descriptors permitted by the host operating system.
.PP
New images are created with \fIimopnc\fR and \fIimcrea\fR.  The \fIimopnc\fR
procedure creates a new copy of an existing image, copying the header of
the old image to the new image but not the data.  The new copy image must
be the same size and datatype as the old image.  For complete control over
the attributes of a new image the \fIimcrea\fR procedure must be used.
The \fIimopnc\fR operation is equivalent to an \fIimopen\fR followed by an
\fIimgsiz\fR to determine the size and datatype of the old image, followed by
an \fIimcrea\fR to create the new image, followed by an \fIimhcpy\fR to copy
the header of the old image to the new image and then two \fIimclos\fR calls
to close both images.
.PP
Note that \fIimgsiz always returns seven elements in the output array axlen\fR,
regardless of the actual dimensionality of the image; this is so that current
programs will continue to work in the future if IMFORT is extended to support
images of dimensionality higher than three.  Images may be deleted with
\fIimdele\fR, or renamed with \fIimrnam\fR; the latter may also be used to
move an image to a different directory.  The \fIimflsh\fR procedure is used
to flush any buffered output pixel data to an image opened for writing.

.TS
center;
n.
\fLimopen (\&image, acmode, im, ier)      \fRacmode: 1=RO,3=RW
\fLimopnc (\&nimage, oim, nim, ier)       \fRacmode: always RW
\fLimclos (\&im, ier)\fR

\fLimcrea (\&image, axlen, naxis, dtype, ier)\fR
\fLimdele (\&image, ier)\fR
\fLimrnam (\&oldnam, newnam, ier)\fR

\fLimflsh (\&im, ier)\fR
\fLimgsiz (\&im, axlen, naxis, dtype, ier)\fR
\fLimhcpy (\&oim, nim, ier)\fR
\fLimpixf (\&im, pixfd, pixfil, pixoff, szline, ier)\fR
.TE
.sp
.ce
Figure 5.  General Image Access Procedures

.PP
The \fIimpixf\fR procedure may be used to obtain the physical attributes
of the pixel file, i.e., the pixel file name, the one-indexed \fIchar\fR
offset to the first pixel, and the physical line length of an image as stored
in the pixel file (the image lines may be aligned on device block boundaries).
These parameters may be used to bypass the IMFORT pixel i/o procedures to
directly access the pixels if desired (aside from the blocking of lines to
fill device blocks, the pixels are stored as in a Fortran array).
The BFIO file descriptor of the open pixel file is also returned, allowing
direct access to the pixel file via BFIO if desired.  If lower level (e.g.,
host system) i/o facilities are to be used, \fIbfclos\fR or \fIimclos\fR
should be called to close the pixel file before reopening it with the foreign
i/o system.
.PP
Direct access to the pixel file is not recommended since it makes a program
dependent upon the details of how the pixels are stored on disk; such a
program may not work with future versions of the IMFORT interface, nor with
implementations of the IMFORT interface for different (non-OIF) physical image
storage formats.  Direct access may be warranted when performing a minimum
modification hack of an old program to make it work in the IRAF environment,
or in applications with unusually demanding performance requirements,
where the (usually negligible) overhead of the BFIO buffer is unacceptable.
Note that in many applications, the reduction in disk accesses provided by
the large BFIO buffer outweighs the additional cpu cycles required for memory
to memory copies into and out of the buffer.

.NH 3
Image Header Keyword Access
.PP
The image header contains a small number of standard fields plus an arbitrary
number of user or application defined fields.  Each image has its own header
and IMFORT does not in itself make any association between the header parameters
of different images.  The header access procedures are summarized in Figure 6.
Note that the \fIimgsiz\fR procedure described in the previous section is the
most convenient way to obtain the size and datatype of an open image, although
the same thing can be achieved by a series of calls to obtain the values of
the individual keywords, using the procedures described in this section.

.TS
center;
n.
\fLimacck (\&im, keyw, ier)\fR
\fLimaddk (\&im, keyw, dtype, comm, ier)\fR
\fLimdelk (\&im, keyw, ier)\fR
\fLimtypk (\&im, keyw, dtype, comm, ier)\fR

\fLimakw[bcdir] (\&im, keyw, [bcdir]val, comm, ier)\fR
\fLimgkw[bcdir] (\&im, keyw, [bcdir]val, ier)\fR
\fLimpkw[bcdir] (\&im, keyw, [bcdir]val, ier)\fR

\fLimokwl (\&im, patstr, sortit, kwl, ier)\fR
\fLimgnkw (\&kwl, outstr, ier)\fR
\fLimckwl (\&kwl, ier)\fR
.TE
.sp
.ce
Figure 6.  Image Header Access Procedures

.PP
Both the standard and user defined header parameters may be accessed via the
procedures introduced in this section.  The \fIimacck\fR procedure tests for
the existence of the named keyword, returning a zero \fIier\fR if the keyword
exists.  New keywords may be added to the image header with \fIimaddk\fR,
and old keywords may be deleted with \fIimdelk\fR.  The datatype of a keyword
may be determined with \fIimtypk\fR.  The attributes of a keyword are its
name, datatype, value, and an optional comment string describing the
significance of the parameter.  The comment string is normally invisible
except when the header is listed, but may be set when a new keyword is added
to the header, or fetched with \fIimtypk\fR.
.PP
The most commonly used procedures are likely to be the \fIimgkw\fR and
\fIimpkw\fR families of procedures, used to get and put the values of named
keywords; these procedures require that the keyword already be present in
the header.  The \fIimakw\fR procedures should be used instead of the
\fIimpkw\fR procedures if it is desired that a keyword be automatically added
to the header if not found, before setting the new value.  Automatic datatype
conversion is performed if the requested datatype does not match the actual
datatype of the keyword.
.PP
The \fIkeyword list\fR package is the only way to obtain information from
the header without knowing in advance the names of the header keywords.
The \fIimokwl\fR procedure opens a keyword list consisting of all header
keywords matching the given pattern, returning a \fIlist descriptor\fR to
be used as input to the other procedures in the package.  Successive
keyword \fInames\fR are returned in calls to \fIimgnkw\fR; a nonzero
\fIier\fR is returned when the end of the list is reached.  The keyword
name is typically used as input to other procedures such as \fIimtypk\fR
or one of the \fIimgkw\fR procedures to obtain further information about
the keyword.  A keyword list should be closed with \fIimckwl\fR when it is
no longer needed to free system resources associated with the list descriptor.

.TS
center box;
cb s s
ci | ci | ci
l | c | l.
Standard Image Header User Keywords
_
name	datatype	description
=
naxis	int	number of axes (dimensionality)
naxis[1:3]	int	length of each axis, pixels
pixtype	int	pixel datatype
datamin	real	minimum pixel value
datamax	real	maximum pixel value
ctime	int	image creation time
mtime	int	image modification time
limtime	int	time min/max last updated
title	string	image title string (for plots etc.)
.TE

.PP
The keyword list pattern string follows the usual IRAF conventions; some
useful patterns are "\(**", which matches the entire header, and "i_", which
matches only the standard header keywords (the standard header keywords are
really named "i_naxis", "i_pixtype", etc., although the "i_" may be omitted
in most cases).  A pattern which does not include any pattern matching
metacharacters is taken to be a prefix string, matching all keywords whose
names start with the pattern string.
.PP
An image must be opened with read-write access for header updates to have
any effect.  An attempt to update a header without write permission will
not produce an error status return until \fIimclos\fR is called to update
the header on disk (and close the image).

.NH 3
Image Pixel Access
.PP
The IMFORT image pixel i/o procedures are used to get and put entire image
lines to N-dimensional images, or to get and put N-dimensional subrasters
to N-dimensional images.  In all cases the caller supplies a buffer into
which the pixels are to be put, or from which the pixels are to be taken.
The pixel i/o procedures are summarized in Figure 7.
.PP
As shown in the figure, there are four main classes of pixel i/o procedures,
the get-line, put-line, get-section, and put-section procedures.  The get-line
and put-line procedures are special cases of the get/put section procedures,
provided for programming convenience in the usual line by line sequential
image operator (they are also slightly more efficient than the subraster
procedures for line by line i/o).  It is illegal to reference out of bounds
and \fIi1\fR must be less than or equal to \fIi2\fR (IMFORT will not flip
lines); the remaining subscripts may be swapped if desired.  Access may be
completely random if desired, but sequential access (in storage order) implies
fewer buffer faults and is more efficient.

.KS
.TS
center;
n.
\fLim[gp]l1[rs] (\&im, buf, ier)\fR
\fLim[gp]l2[rs] (\&im, buf, lineno, ier)\fR
\fLim[gp]l3[rs] (\&im, buf, lineno, bandno, ier)\fR
\fLim[gp]s1[rs] (\&im, buf, i1, i2, ier)\fR
\fLim[gp]s2[rs] (\&im, buf, i1, i2, j1, j2, ier)\fR
\fLim[gp]s3[rs] (\&im, buf, i1, i2, j1, j2, k1, k2, ier)\fR
.TE
.sp
.ce
Figure 7.  Image Pixel I/O Procedures
.KE

.PP
Type short and type real versions of each i/o procedure are provided.
The type real procedures may be used to access images of either type short
or type real, with automatic datatype conversion being provided if the disk
and program datatypes do not match.  The type short-integer i/o procedures
may only be used with type short images.
.PP
The user who is familiar with the type of image i/o interface which maps
the pixel array into virtual memory may wonder why IMFORT uses the more old
fashioned buffered technique.  There are two major reasons why this approach
was chosen.  Firstly, the virtual memory mapping technique, in common use on
VMS systems, is \fInot portable\fR.  On a host which does not support the
mapping of file segments into paged memory, the entire image must be copied
into paged memory when the image is opened, then copied again when the image
operation takes place, then copied once again from memory to disk when the
image is closed.  Needless to say this is very inefficient, particularly for
large images, and some of our applications deal with images 2048 or even 6000
pixels square.
.PP
Even on a machine that supports mapping of file segments into memory, mapped
access will probably not be efficient for sequential access to large images,
since it causes the system to page heavily; data pages which will never be
used again fill up the system page caches, displacing text pages that must
then be paged back in.  This happens on even the best systems, and on a system
that does not implement virtual memory efficiently, performance may suffer
greatly.
.PP
A less obvious reason is that mapping the image directly into memory violates
the principle of \fIdata independence\fR, i.e., a program which uses this
type of interface has a builtin dependence on the particular physical image
storage format in use when the program was developed.  This rules out even
such simple interface features as automatic datatype conversion, and prevents
the expansion of the interface in the future, e.g., to provide such attractive
features as an image section capability (as in the real IRAF image interface),
network access to images stored on a remote node, support for pixel storage
schemes other than line storage mode (e.g., isotropic mappings or sparse image
storage), and so on.  
.PP
The majority of image operations are either sequential whole-image operations
or operations upon subrasters, and are just as easily programmed with a
buffered interface as with a memory mapped interface.  The very serious
drawbacks of the memory mapped interface dictate that it not be used except
in special applications that must randomly access individual pixels in an
image too large to be read in as a subraster.

.NH 2
Error Handling
.PP
The IMFORT error handling mechanism is extremely simple.  All procedures in
which an error condition can occur return a nonzero \fIier\fR error code
if an error occurs.  The value of \fIier\fR identifies which of many possible
errors actually occurred.  These error codes may be converted into error
message strings with the following procedure:
.DS
\fLimemsg (\&ier, errmsg)\fR
.DE
It is suggested that every main program contain an error handling section at
the end of the program which calls \fIimemsg\fR and halts program execution
with an informative error message, as in the examples in \(sc2.
This is especially helpful when debugging new programs.

.NH 2
Vector Operators
.PP
The vector operators (VOPS) package is a subroutine library implementing
a large number of primitive operations upon one dimensional vectors of any
datatype.  Some of the operations implemented by the VOPS routines are
non-trivial to implement, in which case the justification for a library
subroutine is clear.  Even in the simplest cases, however, the use of a
VOPS procedure is advantageous because it provides scope for optimizing
all programs which use the VOPS operator, without having to modify the
calling programs.  For example, if the host machine has vector hardware
or special machine instructions (e.g., the block move and bitfield instructions
of the VAX), the VOPS operator can be optimized in a machine dependent way
to take advantage of the special capabilities of the hardware, without
compromising the portability of the applications software using the procedure.
.PP
The VOPS procedures adhere to the naming convention described in \(sc4.
The package prefix is \fIa\fR, the function code is always three characters,
and the remaining one or two characters define the datatype or types upon
which the procedure operates.  For example, \fIaaddr\fR performs a vector
add upon type real operands.  If the character \fIk\fR is added to the
three character function name, one of the operands will be a scalar.
For example, \fIaaddkr\fR adds a scalar to a vector, with both the scalar
and the vector being of type real.
.PP
Most vector operators operate upon operands of a single datatype: one notable
exception is the \fIacht\fR (change datatype) operator, used to convert a
vector from one datatype to another.  For example, \fIachtbi\fR will unpack
each byte in a byte array into an integer in the output array, providing a
capability that cannot be implemented in portable Fortran.  Any datatype
suffix characters may be substituted for the \fIbi\fR, to convert a vector
from any datatype to any other datatype.
.PP
In general, there are are three main classes of vector operators, the
\fIunary\fR operators, the \fIbinary\fR operators, and the \fIprojection\fR
operators.  The unary operators perform some operation upon a single input
vector, producing an output vector as the result.  The binary operators
perform some operation upon two input vectors, producing an output vector
as the result.  The projection operators compute some function of a single
input vector, producing a scalar function value (rather than a vector) as
the result.  Unary operators typically have three arguments, binary
operators four, and projection operators two arguments and one output
function value.  For example, \fIaabsi\fR is the unary absolute value
vector operator, type integer (here, \fIa\fR is the input vector, \fIb\fR
is the output vector, and \fInpix\fR is the number of vector elements):
.DS
\fLaabsi (a, b, npix)\fR
.DE
A typical example of a binary operator is the vector add operator, \fIaaddr\fR.
Here, \fIa\fR and \fIb\fR are the input vectors, and \fIc\fR is the output
vector:
.DS
\fLaaddr (a, b, c, npix)\fR
.DE
In all cases except where the output vector contains fewer elements than one
of the input vectors, the output vector may be the same as one of the input
vectors.  A full range of datatypes are provided for each vector operator,
except that there are no boolean vector operators (integer is used instead),
and \fIchar\fR and \fIcomplex\fR are only partially implemented, since they
are not sensible datatypes for many vector operations.  In any case, the VOPS
\fIchar\fR is the SPP char and should be avoided in Fortran programs.
.PP
Once these rules are understood, the calling sequence of a particular VOPS
operator can usually be predicted with little effort.  The more complex
operators, of course, may have special arguments, and some study is typically
required to determine their exact function and how they are used.  A list of
the VOPS operators currently provided is given below (the datatype suffix
characters must be added to the names shown to form the full procedure names).

.TS
center;
n.
aabs -\& Absolute value of a vector
aadd -\& Add two vectors
aaddk -\& Add a vector and a scalar
aand -\& Bitwise boolean AND of two vectors
aandk -\& Bitwise boolean AND of a vector and a scalar
aavg -\& Compute the mean and standard deviation of a vector
abav -\& Block average a vector
abeq -\& Vector equals vector
abeqk -\& Vector equals scalar
abge -\& Vector greater than or equal to vector
abgek -\& Vector greater than or equal to scalar
abgt -\& Vector greater than vector
abgtk -\& Vector greater than scalar
able -\& Vector less than or equal to vector
ablek -\& Vector less than or equal to scalar
ablt -\& Vector less than vector
abltk -\& Vector less than scalar
abne -\& Vector not equal to vector
abnek -\& Vector not equal to scalar
abor -\& Bitwise boolean OR of two vectors
abork -\& Bitwise boolean OR of a vector and a scalar
absu -\& Block sum a vector
acht -\& Change datatype of a vector
acjgx -\& Complex conjugate of a complex vector
aclr -\& Clear (zero) a vector
acnv -\& Convolve two vectors
acnvr -\& Convolve a vector with a real kernel
adiv -\& Divide two vectors
adivk -\& Divide a vector by a scalar
adot -\& Dot product of two vectors
advz -\& Vector divide with divide by zero detection
aexp -\& Vector to a real vector exponent
aexpk -\& Vector to a real scalar exponent
afftr -\& Forward real discrete fourier transform
afftx -\& Forward complex discrete fourier transform
aglt -\& General piecewise linear transformation
ahgm -\& Accumulate the histogram of a series of vectors
ahiv -\& Compute the high (maximum) value of a vector
aiftr -\& Inverse real discrete fourier transform
aiftx -\& Inverse complex discrete fourier transform
aimg -\& Imaginary part of a complex vector
alim -\& Compute the limits (minimum and maximum values) of a vector
alln -\& Natural logarithm of a vector
alog -\& Logarithm of a vector
alov -\& Compute the low (minimum) value of a vector
altr -\& Linear transformation of a vector
alui -\& Vector lookup and interpolate (linear)
alut -\& Vector transform via lookup table
amag -\& Magnitude of two vectors (sqrt of sum of squares)
amap -\& Linear mapping of a vector with clipping
amax -\& Vector maximum of two vectors
amaxk -\& Vector maximum of a vector and a scalar
amed -\& Median value of a vector
amed3 -\& Vector median of three vectors
amed4 -\& Vector median of four vectors
amed5 -\& Vector median of five vectors
amgs -\& Magnitude squared of two vectors (sum of squares)
amin -\& Vector minimum of two vectors
amink -\& Vector minimum of a vector and a scalar
amod -\& Modulus of two vectors
amodk -\& Modulus of a vector and a scalar
amov -\& Move (copy or shift) a vector
amovk -\& Move a scalar into a vector
amul -\& Multiply two vectors
amulk -\& Multiply a vector and a scalar
aneg -\& Negate a vector (change the sign of each pixel)
anot -\& Bitwise boolean NOT of a vector
apkx -\& Pack a complex vector given the real and imaginary parts
apol -\& Polynomial evaluation
apow -\& Vector to an integer vector power
apowk -\& Vector to an integer scalar power
arav -\& Mean and standard deviation of a vector with pixel rejection
arcp -\& Reciprocal of a scalar and a vector
arcz -\& Reciprocal with detection of divide by zero
arlt -\& Vector replace pixel if less than scalar
argt -\& Vector replace pixel if greater than scalar
asel -\& Vector select from two vectors based on boolean flag vector
asok -\& Selection of the Kth smallest element of a vector
asqr -\& Square root of a vector
asrt -\& Sort a vector in order of increasing pixel value
assq -\& Sum of squares of a vector
asub -\& Subtract two vectors
asubk -\& Subtract a scalar from a vector
asum -\& Sum of a vector
aupx -\& Unpack the real and imaginary parts of a complex vector
awsu -\& Weighted sum of two vectors
awvg -\& Mean and standard deviation of a windowed vector
axor -\& Bitwise boolean XOR (exclusive or) of two vectors
axork -\& Bitwise boolean XOR (exclusive or) of a vector and a scalar
.TE

.PP
A non-trivial example of the use of vector operators is the case of bilinear
interpolation on a two dimensional image.  The value of each pixel in the
output image is a linear sum of the values of four pixels in the input image.
The obvious solution is to set up a do-loop over the pixels in each line of
the output image, computing the linear sum over four pixels from the input
image for each pixel in the output line; this is repeated for each line in the
output image.
.PP
The solution using the VOPS operators involves the \fIalui\fR (vector look up
and interpolate) and \fIawsu\fR (weighted sum) vector operators.  A lookup table
defining the X-coordinate in the input image of each pixel in a line of the
output image is first generated.  Then, for each line of the output image,
the two lines from the input image which will contribute to the output image
line are extracted.  \fIAlui\fR is used to interpolate each line in X, then
\fIawsu\fR is used to form the weighted sum to interpolate in the Y direction.
This technique is especially efficient when bilinear interpolation is being
used to expand the image, in which case the \fIalui\fR interpolated X-vectors,
for example, are computed once but then used to generate several lines of
the output image by taking the weighted sum, a simple and fast operation.
When moving sequentially up through the image, the high X-vector becomes the
low X-vector for the next pair of input lines, hence only a single call to
\fIalui\fR is required to set up the next region.
.PP
The point of this example is that many or most image operations can be
expressed in terms of primitive one dimensional vector operations,
regardless of the dimensionality of the image being operated upon.
The resultant algorithm will often run more efficiently even on a conventional
scalar machine than the equivalent nonvectorized code, and will probably run
efficiently without modification on a vector machine.
.PP
Detailed specification sheets (manual pages) are not currently available for
the VOPS procedures.  A summary of the calling sequences is given in the file
\fLvops$vops.syn\fR, which can be paged or printed by that name while in the
CL, assuming that the system has not been stripped and that the sources are
still on line.  The lack of documentation is really not a problem for these
operators, since they are all fairly simple, and it is easy to page the source
file (in the \fIvops\fR directory) to determine the exact calling sequence.
For example, to examine the source for \fIawsu\fR, type
.DS
\fLcl> page vops$awsu.gx\fR
.DE
to page the generic source, regardless of the specific datatype of interest.
If you have trouble deciphering the generic source,
use \fLxc -f file.x\fR to produce the Fortran translation
of one of the type specific files in the subdirectories
\fLvops$ak\fR and \fLvops$lz\fR.

.NH 2
Binary File I/O (BFIO)
.PP
The IMFORT binary file i/o package (BFIO) is a small package, written
originally as an internal package for use by the IMFORT image i/o routines
for accessing header and pixel files (the VOS FIO package could not be used
in IMFORT without linking the entire IRAF/VOS runtime system into the Fortran
program).  Despite its original conception as an internal package, the package
provides a useful capability and is portable, hence has been included in the
IMFORT interface definition.  Nonetheless, the user should be warned that BFIO
is a fairly low level interface and some care is required to use it safely.
If other suitable facilities are available it may be better to use those,
although few interfaces will be found which are simpler or more efficient
than BFIO for randomly accessing pre-existing or preallocated binary files.
.PP
The principal capability provided by BFIO is the ability to randomly access
a binary file, reading or writing an arbitrary number of char-units of storage
at any (one-indexed) char offset in the file.  The file itself is a non-record
structured file containing no embedded record manager information,
hence is suitable for access by any program, including non-Fortran programs,
and for export to other machines (this is usually not the case with a Fortran
unformatted direct access file).  Unlike the mainline IMFORT procedures,
many of the BFIO procedures are integer functions returning a positive count
value if the operation is successful (e.g., the number of char units of storage
read or written), or a negative value if an error occurs.  Zero is returned
for a read at end of file.

.TS
center;
n.
\fLbfaloc (\&fname, nchars, status)\fR
\fLfd = bfopen (\&fname, acmode, advice)   \fRacmode: 1=RO,3=RW,5=NF
\fLbfclos (\&fd, status)              \fRadvice: 1=random,2=seq

\fLnchars = bfread (\&fd, buf, nchars, offset)\fR
\fLnchars = bfwrit (\&fd, buf, nchars, offset)\fR

\fLnchars = bfbsiz (\&fd)\fR
\fLnchars = bffsiz (\&fd)\fR
\fLchan = bfchan (\&fd)\fR
\fLstat = bfflsh (\&fd)\fR
.TE
.sp
.ce
Figure 8.  Low Level Binary File I/O Procedures

.PP
BFIO binary files may be preallocated with \fIbfaloc\fR, or created with
\fIbfopen\fR and then initialized by writing at the end of file.
Preallocating a file is useful when the file size is known in advance, e.g.,
when creating the pixel file for a new image.  The contents of a file
allocated with \fIbfaloc\fR are uninitialized.  To extend a file by writing
at the end of file the file size must be known; the file size may be obtained
by calling \fIbffsiz\fR on the open file.
.PP
Before i/o to a file can occur, the file must be opened with \fIbfopen\fR.
The \fIbfopen\fR procedure returns as its function value an integer
\fIfile descriptor\fR which is used to refer to the file in all subsequent
accesses until the file is closed with \fIbfclos\fR.  Binary data is read
from the file with \fIbfread\fR, and written to the file with \fIbfwrit\fR.
Any amount of data may be read or written in a single call to \fIbfread\fR
or \fIbfwrit\fR.  All user level i/o is synchronous and data is buffered
internally by BFIO to minimize disk transfers and provide for the blocking
and deblocking of data into device blocks.  Any buffered output data may be
flushed to disk with \fIbfflsh\fR.  The function \fIbfchan\fR returns the
descriptor of the raw i/o channel as required by the IRAF binary file driver.
.PP
BFIO manages an internal buffer, necessary for efficient sequential i/o and
to hide the device block size from the user program.  Larger buffers are
desirable for sequential i/o on large files; smaller buffers are best for
small files or for randomly accessing large files.  The buffer size may be
set at \fIbfopen\fR time with the \fIadvice\fR parameter.  An \fIadvice\fR
value of 1 implies random access and causes a small buffer to be allocated;
a value of 2 implies sequential access and causes a large buffer to be
allocated.  Any other value is taken to be the actual buffer size in chars,
but care must be used since the value specified must be some multiple of the
device block size, and less than the maximum transfer size permitted by the
kernel file driver.  Note that when writing at end of file, the full contents
of the internal buffer will be written, even if the entire buffer contents
were not written into in a \fIbfwrit\fR call.  The buffer size in chars is
returned by \fIbfbsiz\fR.
.PP
Since BFIO is a low level interface, the file offset must always be specified
when reading from or writing to the file, even when the file is being accessed
sequentially.  Contrary to what one might think, file offsets are one-indexed
in the Fortran tradition, and are specified in units of \fIchars\fR.
Do not confuse \fIchar\fR with the Fortran \fLCHARACTER\fR; \fIchar\fR is the
fundamental unit of storage in IRAF, the smallest datum which can be accessed
as an integer quantity with the host Fortran compiler, normally
\fLINTEGER\(**2\fR (16 bits or two bytes on all current IRAF hosts).

.bp
.SH
Appendix:  Manual Pages for the Imfort Procedures
.PP
This section presents the ``manual pages'' for the IMFORT and BFIO procedures.
The manual pages present the exact technical specifications of each procedure,
i.e., the procedure name and arguments (not necessarily obvious in the case of
a typed family of procedures), the datatypes and dimensions of the arguments,
and a precise description of the operation of the procedure.
Each procedure is presented on a separate page for ease of reference.
.PP
The following conventions have been devised to organize the information
presented in this section:
.RS
.IP \(bu
The manual pages are presented in alphabetical order indexed by the procedure
name.
.IP \(bu
A single manual page is used to present an entire family of procedures which
differ only in the datatype of their primary operand.  The name on the manual
page is the generic name of the family, e.g., \fIclargi\fR, \fIclargr\fR, etc.,
are described in the manual page \fIclarg\fR.
.IP \(bu
In some cases it makes sense to describe several related procedures with a
single manual page.  An example is the keyword-list package, consisting of
the procedures \fIimokwl\fR, \fIimgnkw\fR, and \fIimckwl\fR.  In such a case,
since the procedures have different names the manual page for the group is
duplicated for each procedure in the group, so that the user will not have
to guess which name the manual page is filed under.
.IP \(bu
The \fIsynopsis\fR section of each manual page defines the calling sequence of
each procedure, the datatypes and dimensions of the arguments, and notes
whether each argument is an input argument (\fL#I\fR) or an output argument
(\fL#O\fR).
.IP \(bu
The \fIreturn value\fR section describes the conditions required for
successful execution of the procedure, normally indicated by a zero status
in \fIier\fR.  A symbolic list of the possible error codes is also given.
The numeric values of these error codes are defined in \fLimfort$imfort.h\fR
and in \fLlib$syserr.h\fR, but the exact numeric codes should be used only for
debugging purposes or passed on to the \fIimemsg\fR procedure to get the error
message string.  The numeric error codes are likely to change in future versions
of the interface hence their values should not be "wired into" programs.
.RE
.PP
Manual pages for the VOPS procedures are not included since VOPS is not really
part of the IMFORT interface, and it is not yet clear if the VOPS procedures
are complex enough to justify the production of individual manual pages.
