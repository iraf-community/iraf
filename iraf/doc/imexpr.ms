.ce
\fBImage Expressions in the IRAF Command Language\fR
.ce
Technical Specifications
.ce
Doug Tody
.ce
November 1983
.sp 2

.NH
Requirements
.PP
We wish to be able to directly manipulate images in expressions at the CL
level as easily as we currently manipulate scalars.  This feature is
essential if the scientist is to be able to solve general image reduction
and analysis problems with minimal time spent developing special
purpose software.  Furthermore, we expect to rely heavily on image expressions
in IRAF for batch reductions of large astronomical datasets, and therefore the
implementation of image expressions must be efficient.

.RS
.IP (1)
It shall be possible to evaluate general expressions wherein the operands
are constants, scalar variables, images or image sections of any dimension
or datatype, or functions of any of the above.
.IP (2)
It shall be possible to freely use image expressions and statements within 
the standard conditional and looping constructs of the CL without any
special restrictions.
.IP (3)
It shall be possible to pass a general expression evaluating to an operand
of type image to any procedure which expects an image as an input argument.
.IP (4)
It shall be possible to pass images or \fIsubsections\fR of images to
procedures as input operands, output operands, or input/output operands.
.IP (5)
It shall be possible for both CL script tasks and external compiled programs
to be used routinely with image expression arguments.
.IP (6)
It shall be easy for the user to extend the system, adding special purpose
scripts or programs beyond those provided by the system.  Any such tools
added by the user shall be retained from session to session indefinitely.
It shall not be possible for such user extensions to affect the operation
of the basic system.
.RE

.PP
We \fImust\fR be able to process complex expressions involving very large
images without compromising the transportability of the IRAF system.
In particular, such operations shall be possible on reasonably equipped
supermicro computers (2-4 Mb main memory) which do not have good virtual
memory facilities (few do), or which have no virtual memory facilities at all.

.NH
Specifications
.PP
The proposed implementation of image expressions described here meets all
of the above requirements without sacrificing modularity or requiring
major modifications to the current CL.  The design can handle large images
very efficiently; i/o and memory requirements are minimized and hooks are
included for eventually adding a bit-mapped array processor.  For small
images, i.e., two dimensional images less than 64 pixels square, there is
significant overhead but the interactive response should still be good.
New features not found in any currently available image processing system
(so far as I am aware) include general image sections and conditional
expressions.
.NH 2
Images
.PP
An image is an IRAF imagefile.  Images are described in detail elsewhere.
In brief, images of from one to seven dimensions are (currently) supported.
The permissible imagefile datatypes are short, unsigned short, int, long,
real, double, and complex.  There is effectively no limit on the size of
an image, nor is there a fixed limit on the size of the images in image
expressions, even on modest non-virtual memory machines.
.PP
Each image has a header describing the physical and derived attributes of
the image, i.e., the dimensions and datatype, mininum and maximum pixel
values, title, history, histogram, coordinate systems, and so on.
An image may also have a bad pixel list.
In general, bad pixel lists are automatically merged in image expressions
and the resultant list passed on to the output image, but otherwise no
distinction is made between good and bad pixels in image expressions.
.NH 2
Image Operand Syntax
.PP
Any operand in a CL expression which is subscripted has the datatype
\fBimage\fR.  An image type operand or \fBimage section\fR has the form
.DS
	\fIimage_name\fR "[" \fIsubscript_list\fR "]"
.DE
where \fIimage_name\fR may be any string or integer valued expression, and
where \fIsubscript_list\fR is a (possibly null) list of subscript expressions
(described in \(sc2.5), or a string valued expression which reduces to a
string containing only section metacharacters or numeric constants.
The subscripting operator "[" is a true operator with a precedence
(binding force) greater than that of any of the
conventional operators, and which is right associative.
.PP
Parentheses may be used to change the default order of evaluation of an
expression containing a subscript, if desired.  For example, consider
the following two expressions:
.DS
	imname // imnumber []
	(imname // imnumber)[]
.DE
Due to the strong precedence of the [], the first expression is actually
equivalent to "imname // (imnumber[])", which is probably not what was
intended.
.PP
Lastly, since the image name string is a filename string, it is subject
to the usual restrictions on optional quoting of filename strings.
Consider the following two expressions:
.DS
	m92[] \(mi biasframe[]
	"m92"[] \(mi (biasframe)[]
.DE
The first expression is ambiguous, since it is not evident whether
or not the identifiers "m92" and "biasframe" are string constants (filenames)
or parameter names.  The expression is resolved by applying the usual
disambiguating rules, i.e., if a string is expected and there are no
enclosing parentheses, the identifer is assumed to be a string constant.
The second expression is unambiguous.
.PP
If the filename string is not a legal identifier, for example if it is an OS
dependent pathname or if it contains special characters (as in "PK157+23"),
the quotes are required.  If the ultimate in brevity is desired numeric
filenames may be used, as in the expression "5[] + 9[]".  The actual disk
files would be named "I5" and "I9" in this example.  Numeric filenames need
never be quoted.
.NH 2
Parameters of Type Image
.PP
CL script tasks and compiled programs may have parameters of type \fBimage\fR.
In many respects image type parameters are similar to filename or string
type parameters, i.e., they may be list structured, queries are generated
in the usual fashion, and so on.  The essential thing to keep in mind when
using image type parameters is that they have the datatype image, not
string, and therefore may not be subscripted.  An image type parameter may
be used anywhere an image type operand expression would be used, and in 
fact they are the same type of object.
.PP
Image parameters are set either by using an image type expression in
the argument list to a procedure, or by explicit assignment.  In an
assignment, the value of the image parameter on the left side is set
only if the right-hand side has the datatype string.  Otherwise, the
image parameter defines the image section to be stored into.  Thus,
.DS
	output_image = a[*,\(mi*]
.DE
is an image copy operation (it moves pixels), whereas
.DS
	output_image = str (a[*,\(mi*])
.DE
merely sets the value of the parameter "output_image" to the indicated
image section specification.
.NH 2
The Attributes of an Image
.PP
The physical attributes of an image are accessible within the CL by means
of special intrinsic functions.  The special image attribute intrinsic
functions are the following:

.RS
\fBndim\fR (\fIimage_section\fR)
.RS
Returns the number of dimensions in the referenced image.
.RE
.sp
\fBlen\fR (\fIimage_section\fR, \fIaxis\fR)
.RS
Returns the length of the indicated dimension of the named section.
The x-axis is dimension number one, the y-axis is dimension number two,
and so on.  If the axis argument is omitted, the total number of pixels
is returned.
.RE
.sp
\fBpixtype\fR (\fIimage_section\fR)
.RS
Returns a string identifying the datatype of the pixels in the image
(i.e., "ushort", "short", "real", etc.).
.RE
.RE

.PP
The remaining physical image attributes, and all of the special user
defined attributes, are accessible only via the DBIO/CL interface.
.NH 2
Image Sections
.PP
Image sections are used to specify the region of the image to be operated
upon.  If the entire image is to be operated upon, then the null section
("[]") must still be given to tell the CL that the operand is of type image.
A limited class of coordinate transformations may be specified using image
sections (but transpose is \fInot\fR one of them).  Though the use of an
explicit image section appears to require knowledge of the actual dimensions
of the image, in fact the actual image may be of a higher dimension than
indicated, with the higher dimensions being set to one.  The basic types of
image sections are noted below for two dimensional images.

.KS
.TS
center;
ci ci
l l.
section	refers to

pix[]	whole image
pix[i,j]	the pixel value (scalar) at [i,j]
pix[*,*]	whole image, two dimensions
pix[*,\(mi*]	flip y-axis
pix[*,*,b]	band B of three dimensional image
pix[*,*:s]	subsample in y by S
pix[*,l]	line L of image
pix[c,*]	column C of image
pix[i1:i2,j1:j2]	subraster of image
pix[i1:i2:sx,j1:j2:sy]	subraster with subsampling
pix[*n,*]	shift N pixels in x
.TE
.KE

The "match all" (asterisk), flip, subsample, index, range, and shift notations
may be combined just about any way that makes sense.  Some examples
are given later.
.PP
The subscript list part of an image section specification may be either an
expression list, as in the examples above, or a string valued constant,
parameter, or expression.  If the same section specification is used several
times within a procedure it may be desirable to parameterize it, so that the
procedure may be easily used to process other sections.  For example, many
modern CCD detectors produce an image with a bias strip along one side of
the image which must be removed from the data at some point in the reductions.
The coordinates of the bias strip and of the data area should be parameterized
so that the same scripts can easily be used to process data from slightly
different detectors:

.DS
bias_strip = "325:350,1:512"
data_area = "1:324,1:512"

# Display only the data pixels.
display rawpix[data_area]
.DE
.NH 2
Statements
.PP
In general, image expressions may be used wherever an ordinary arithmetic
expression would be used.  No image expression produces a boolean result
and therefore image expressions may not be used where boolean expressions
are expected (i.e., in \fBif\fR, \fBwhile\fR, etc. conditions).
Image expressions may be freely used within conditional and looping control
constructs, within the argument lists of subprocedures, and within the
subprocedures themselves.
.PP
Much of the power of image expressions derives from their use within
assignment statements wherein the left hand side (lhs) is an image or image
section.  The CL implements five kinds of assignment statements, as
illustrated in the table below.
.PP
A lhs of type image may specify either a full image (as in the examples),
or a section of an existing image.  If a full image section is specified,
a new image will be created, overwriting any existing image.  The datatype
of the new image will be that of the expression on the right hand side.
The rhs may be a scalar, a vector of length matching one of the dimensions
of the lhs, or a section with the same dimensions as the lhs.
If a subsection is specified, the named image must exist and the indicated
section will be edited as specified.

.KS
.TS
center;
ci ci
l l.
statement	meaning

a[] = expr	simple assignment
a[] += expr	add image expression to image A
a[] \(mi= expr	subtract image expression from image A
a[] *= expr	multiply image expression into image A
a[] /= expr	divide image expression into image A
.TE
.KE

.PP
When we say that a new image overwrites any existing image of the same
name, what we actually mean is that it does so only if file \fBclobber\fR
is enabled.  File clobber protection is a standard feature of the IRAF system;
if clobber is disabled (default), an abort will occur when a program attempts
to overwrite an existing file (in which case the file or image must be
explicitly deleted and the operation repeated).  There is another standard
feature allowing the user to explicitly "protect" files.
.PP
The four "exotic" assignment operators are used to edit existing images.
The same image may, if desired, appear on both sides of an assignment
statement.  The "+=" assignment is especially useful for constructing
mosaics.
.NH 2
Operators
.PP
An expression containing image type operands is grammatically like any
other CL expression (or SPP expression), and therefore the same set of
operators may be used for both types of expressions.  The precedence and
associativity of these operators are the same in all cases.

.TS
center;
ci ci
l l.
operator	meaning

+	addition
\(mi	subtraction or unary negation
*	multiply
/	divide
**	exponentiation
//	concatenation

\(eq\(eq	equal
!\(eq	not equal
<	less than
<\(eq	less than or equal
>	greater than
>\(eq 	greater than or equal
!	boolean negation

?	conditional expression
.TE

.PP
When used with image type operands, as in any expression, the concatenation
operator produces a string result.  Normally the image section will be
concatenated as a string without actually accessing an image.  If this is
not what is desired, i.e., we want to concatenate the scalar value of a single
pixel, parentheses should be used to force evaluation of the image reference
before concatenation.
.PP
The boolean operators have a different meaning when used with one or more
operands of type image then when used in in normal expressions.
In normal expressions, the result of a boolean expression is a scalar
of type boolean.  If an image type operand is present in a boolean expression,
the expression is of type image, and the output image will be a short integer
image wherein the pixels take on the values zero (false) or one (true).
Boolean images are useful for forming masks.
.PP
The final operator shown, the conditional operator, is used to specify
pointwise image operations wherein the action taken for a given output pixel
may depend on the values of each of the input pixels.
In scalar expressions, conditional expressions are useful but not really
essential, but in image expressions the only alternatives are to build special
purpose compiled procedures (time consuming), or to subscript individual pixels
at the CL level (very inefficient).
.PP
To illustrate the use of the conditional
expression, consider the following statement which divides image A into image
B, placing the result back into image B.  The operation is equivalent to a
simple division unless the value of an A pixel is less than 0.01, in which case
the corresponding output pixel is set to zero.
.sp
.DS
	b[] \(eq (a[] < .01) ? 0 : b[] / a[]
.DE
.NH 2
Intrinsic Functions
.PP
All of the standard CL/SPP arithmetic intrinsic functions are permitted
in image expressions.  Some additional intrinsic functions, useful only in
image expressions, are also available.  The standard set of intrinsic
functions used in image expressions is shown in the table below.

.KS
.TS
center;
l l l l l l.
abs	min	mod	nint	long	floor
atan2	sin	aimag	sqrt	real	max
cos	ceil	double	complex	tan	short
int	log	log10	exp	conjug	
.TE
.KE

.PP
Nearly all of these intrinsic functions, when called with an image type
argument or arguments, will return an image result.  The exceptions are
\fBmin\fR and \fBmax\fR, which return the minimum and maximum values of
an image or image section, respectively.  The functions \fBfloor\fR and
\fBceil\fR are used to perform max and min \fIvector\fR operations.
.PP
Additional special image intrinsic functions are required and will be
added once a well thought out list has been prepared.  Examples of such
operators might be vector sum, sum of squares, mean, median, and boxcar
smooth.  Major operations such as image transpose, convolution, filtering,
etc. will be implemented with external procedures, rather than as intrinsic
functions.  The intrinsic functions are evaluated with "inline" vector
instructions and this necessarily limits the range of functions that can
be provided.
.NH 2
Referencing Out of Bounds
.PP
By default, it is illegal to reference out of bounds on an image, except
when using a shift type image section specification.  If more explicit
out of bounds references are desired than one must set the \fBnboundarypix\fR
parameter to a nonzero value, and the \fBboundarytype\fR parameter to the
type of boundary extension desired.  For the convenience of
interactive users these parameters are defined globally, but they should
be redefined as local parameters in the parameter file for a task which
must reference out of bounds.
.PP
There is no one best way to satisfy out of bounds pixel references (unless
it is to abort).  The following options are currently available:

.KS
.TS
center;
ci ci
l l.
boundarytype	action

nearest	use value of nearest boundary pixel
reflect	reflect coords back into image
project	project outward from boundary
wraparound	wrap around to opposite side of image
indefinite	set pixel value to INDEF
apodize	sample a cosine (for fourier analysis)
.TE
.KE
.NH 2
Procedures
.PP
Only a limited number of predefined intrinsic functions are available
due to the way expressions containing intrinsic functions are evaluated.
The system can accomodate any number of procedures, however, and many are
available in the \fBimages\fR, \fBimred\fR, \fBartdata\fR, and other packages
in the IRAF system.  The user can easily add packages and procedures of
their own.  Refer to the \fICL Programmer's Guide\fR for information on
adding new packages and procedures.
.NH 2
Cursor Readback
.PP
Whenever an image is displayed or a vector is plotted, graphics descriptor
information is saved by the system defining the source image and coordinate
systems used to generate the graphics.  When working interactively, the
graphics or image cursor may be read merely by referencing one of the global
cursor parameters \fBgcur\fR and \fBimcur\fR in an expression.  Each reference
will cause a cursor read.  If a cursor is to be referenced within a procedure,
a local parameter of \fIdatatype\fR gcur or imcur should be defined,
to make it easier to use the procedure noninteractively with a list of cursor
coordinates.

.NH 2
Examples
.PP
A few examples should help to illustrate the use of images and image
sections in expressions in the CL.  Images of any datatype may be used in
expressions, with type coercion following the usual rules.  Explicit
type coercion may be indicated using intrinsic functions (\fBint\fR,
\fBreal\fR, etc).  Operations involving images of different dimensions are
permissible, but in general the images in an expression must be the same
size.  If the actual images are not the same size then sections must
be used to indicate the regions to be used.
.PP
For example, to compute the average of the two images A and B and display
the result, we could enter the commands
.DS
.cs 1 22
avg[] = (a[] + b[]) / 2
display avg[]
.DE
.cs 1
Although not shown that way, if A and B were filenames, not parameters,
they would have to be quoted, since they occur within parentheses.
The null subscript in the call to \fBdisplay\fR is optional since the
entire image is being passed as an argument.
The above operation could be expressed more concisely in the form
.DS
.cs 1 22
display (a[] + b[]) / 2
.DE
.cs 1
which would compute and display the same average image, without explicitly
creating an intermediate image.
.PP
To plot column COL of image PIX on a logarithmic scale, after first
subtracting a bias value, we can either explicitly take the logarithm with
an intrinsic function or let the plotting code compute the logarithm.
The latter is the best choice because \fBplot\fR will then be able to a
better job of labeling the axes.  It remains only to subtract the bias value:
.DS
.cs 1 22
plot pix[col,*] \(mi bias, logy+,
    title = "Log of column " // col // " of image " // pix
.DE
.cs 1
Now suppose we have a million point spectrum which we wish to plot.
The most straightforward way to plot such a large array on a graphics
terminal is to subsample every 1024 pixels:
.DS
.cs 1 22
plot spectrum[*:1024]
.DE
.cs 1
To display the fifth xz plane of the image cube "cube", with 
contour lines 10 graylevels wide spaced every 100 graylevels:
.DS
.cs 1 22
plane[] = cube[*,5,*]
display ((mod (plane[] / 10, 10) == 0) ? 0 : plane[])
.DE
.cs 1
Image sections may also be used to modify a section of an existing image.
Thus,
.DS
.cs 1 22
pix[col,*] += 5.3
.DE
.cs 1
would add the constant 5.3 to each pixel in column COL of image PIX.  Similar
statements may be used to copy subrasters, form mosaics, etc.
.PP
Shifting is useful for image registration and to some extent for filtering.
Linear combinations of the same image shifted a pixel or so differently
in each reference may be used to perform simple filtering operations
(i.e., gradient or laplacian), though generally it is more convenient to
use the standard \fBimages\fR procedures for filtering.  To display the
difference of two images A and B with B shifted to align it with A:
.DS
.cs 1 22
display a[] \(mi b[*3,*\(mi1], 1
.DE
.cs 1
.PP
Most expressions involving images will operate on entire images or image
sections.  Occasionally, however, it is necessary to be able to operate on
individual pixels.  This is done in the obvious way, i.e., by subscripting
individual pixels:
.sp
.DS
.cs 1 22
sum = 0
for (j = j1;  j <= j2;  j += 1)
    for (i = i1;  i <= i2;  i += 1)
        sum += pix[i,j]

# Print "sum over pix[i1:i2,j1:j2] = value".
print ("sum over ", pix, "[", str(i1), ":", str(i2), ",",
    str(j1), ":", str(j2), "] = ", sum)

# Subtract mean value from subraster.
pix[i1:i2,j1:j2] \(mi= sum / (i2\(mii1+1 * j2\(mij1+1)
.DE
.cs 1
.sp
Doing this sort of thing at the CL level is quite inefficient compared to
the equivalent compiled program, but interactive response is not hard to
achieve provided the number of pixels operated on in this fashion is small.
.PP
As a final example, suppose we have four 800-square images
which we wish to combine together to form a mosaic.  To keep things simple
we assume that no rotations are required and that the frames do not overlap.
We further assume that the procedure \fImkimage\fR is available for making
constant (default zero valued) images of arbitrary dimension and size.
One way of forming the mosaic is shown below.
.sp
.DS
.cs 1 22
mkimage mosaic, real, 800, 800
for (j=1;  j < 1600;  j += 800)
    for (i=1;  i < 1600;  i += 800)
        mosaic[i:i+800\(mi1,j:j+800\(mi1] = quadrant
.DE
.cs 1
.sp
We assume here that the parameter "quadrant" is either a query mode
or list structured parameter, so that it may take on a different value
in each reference.  Alternatively a naming convention could be used to
form the names of the quadrant images.

.NH
Implementation Strategies
.PP
The implementation proposed here requires minimal modifications to the
CL and makes full use of the facilities provided by the IRAF program interface,
in particular the IMIO and VOPS interfaces.  A separate process running
concurrently with the CL is used to execute a series of very high level
"assembler language" instructions passed it by the CL, which parses the
original expression, resolves all parameter references, and reduces all
scalar expressions to a single constant.
.PP
The design is very efficient for operations on large images because the
expression is evaluated an image line at a time, requiring one pass through
the entire image expression for each line in the output image.  This scheme
minimizes memory requirements while also minimizing i/o, since no intermediate
images need be written to disk.  The alternative strategy (whole image
operations) is simpler and more efficient for small images, but for large
images either requires an enormous amount of physical memory or leads to
excessive and needless i/o (page faulting).
.PP
At the lowest level all processing will be done with calls to the VOPS
vector operators.  This leads to increased efficiency, since the VOPS
vector "instructions" may easily be optimized in assembler or microcode if
necessary.
Furthermore, the VOPS interface makes it straightforward to interface to
the new bit-mapped (shared memory) array processors.  The use of a shared
memory AP, as opposed to a conventional AP, is desirable because such AP's
can process short vectors (image lines) with very little overhead, since
no additional i/o is required.  We merely allocate our line buffers in
a Fortran common block in the shared memory region.  The AP is then a true
coprocessor which can be used to execute logical vector instructions.
.PP
The use of a separate process is justified because the image calculator
process will probably be larger in size and comparable in complexity to the
CL itself (when one considers the complexity of the IMIO, DBIO, VOPS, and FIO
interfaces), and because the current CL does not use any of these facilities
and is already a very large and complex program.  We also nearly double the
maximum number of open files by using two processes instead of one.
By defining a simple interface between the image calculator and CL we will
find it much easier to modify and enhance both in the future.
Finally, the CL as it now stands is quite useful for non-image processing
applications (i.e., software development, database applications, graphics,
etc.), and the addition of extensive code to support special purpose
applications can only decrease the generality of the CL.
.NH 2
Scope of CL Modifications Required
.PP
All that we need add to the grammar of the CL to support image expressions
is the ability to parse image section subscripts.  To the CL an image section
subscript will be a fancy kind of string concatenation.  An identifier
followed by an image section is converted into a string-type operand with
the abstract datatype "image".  The section string is pushed on the operand
stack as a datume of datatype image, much as a filename string is currently
pushed onto the operand stack.  All processing of the actual image section
string is left to IMIO (in the image calculator process).  The current
implementation of IMIO already has the capability to process image sections
in the manner described in this document.  IMIO also does all buffering, type
conversion, and handling of out of bounds pixel references.
.PP
The image subscript notation tells the CL that an operand is of type
"image" without need for an explicit declaration, as is required for normal
CL scalar or list structured variables.  Another way of expressing this
is that an image is an external data object, like a file, and is not a
parameter and therefore need not be declared.
.PP
The first function of the CL in processing a statement involving image
type operands is to parse expressions and statements into an
internal virtual machine "assembly language" form.  The current CL already
does this, and little need be added to support image type operands.
The resultant code is then executed (interpreted) in the usual fashion by
the CL, satisfying all parameter references and evaluating all scalar
expressions.  If an image section is encountered which is merely assigned
into an image parameter as a string or which is passed to a subprocedure,
it is not an image expression and no call to the image calculator is
necessary.
.PP
If the code contains expressions containing image operands, each such
expression is first processed to satisfy all parameter references and
reduce all scalar expressions to constants,
then it is passed to the image calculator process as a sequence of
ASCII "assembler" instructions containing only image section string 
constants, numeric constants, and calls to intrinsic functions.
.PP
A separate call to the image calculator is required to evaluate each
image expression or image assignment (the image calculator process will
normally sit in the process cache, i.e. hibernate, between calls and will
not have to be reexecuted).  The image calculator process carries out
the actual expression evaluation, evaluating the entire expression once
for each line in the output image.
.PP
The result of an image type expression is a new imagefile, unless the
result is a scalar in which case the scalar is returned to the CL and left
on the CL runtime operand stack as the value of the expression.
If the expression is not explicitly assigned into a named image or
image section by the user, the CL generates a temporary image name which
receives the new image.  Image expressions used as arguments to procedures
are evaluated before the procedure is called, passing only the name of the
resultant temporary image to the subprocedure.  The temporary images are
deleted after normal or abnormal termination of the subprocedure.
.NH 2
The Image Calculator
.PP
The main function of the image calculator is to evaluate a (reverse polish)
expression and write an output image.  This is the case whether or not
the original expression was part of an assignment statement; to the image
calculator, expressions in argument lists are assignments into temporary
imagefiles.  Since the expression is evaluated for all images simultaneously,
the maximum complexity of an expression is limited by the maximum number
of open files permitted a process by the OS.  The image header file need
be open only at immap and imunmap time, so only one file descriptor is
required for each image.  Most systems permit from 16 to 25 open files;
on some systems the number is configurable.
.PP
To minimize \fBimmap\fR and \fBimunmap\fR calls when repeatedly operating
on the same image or set of images, it is desirable to maintain a cache
of mapped images in the image calculator process.  If this is done
then a CL loop which accesses an image in storage order a pixel at a time 
will require "only" four context switches and some code execution per
pixel access.  No i/o will be required since the pixels will, in general,
already be buffered within the image calculator process.  Similar gains
will result for scripts which repeatedly access lines or subrasters in the
same image in successive calls.
.PP
The \fBonexit\fR call in the program interface will be used to unmap all
cached images when the CL commands the image calculator process to exit.
The CL automatically flushes its process cache (containing the image
calculator process) whenever a background job is spawned or an OS escape
is issued, so competing processes should not hang up unnecessarily waiting
for access to an image which is opened for writing by another process (but
they will still do so if necessary).
.PP
All operators and intrinsic functions recognized by the image calculator
will be implemented with VOPS primitives.  One is required for each
data type dealt with internally.  There will be five internal datatypes,
though seven datatypes are permitted on disk.  The internal datatypes
will be short, long, real, double, and complex.  The same functionality
could be acheived with fewer internal types, but it is desirable to
minimize the expense of type conversion.  If necessary a stripped
version of the process could be provided implementing only datatypes
long and real, using an environment variable to specify whether the stripped
or the general process is to be run.
.PP
The CL need have no knowledge of the set of acceptable imcalc intrinsic
functions, except to know which ones return scalar values.  The parser will
recognize a call to an intrinsic function in an image expression and 
generate a call to the named function in the assembler code, but will 
otherwise know nothing about the class of intrinsic functions accepted by
the image calculator.  New intrinsic functions may thus be added to the
image calculator without requiring any modifications to the CL.
.PP
The evaluation of expressions in the image calculator is straightforward
because IMIO does most of the work.  Since IMIO handles image sections
and resolves out of bounds pixel references, the calculator is presented
with the simple problem of reading in N vectors M times and performing
the same set of operations each time.  The line-sequential mode IMIO
i/o calls will be used to read and write image lines, so that we need
not be concerned about dimensionality.  Some logic is required to deal
with the cases of a scalar times an image or a vector times an image.
Conditional expressions are evaluated by reducing the three parts to
vectors using the VOPS primitives, then calling another VOPS primitive
to select elements from the true and false vectors to generate the 
output vector.
