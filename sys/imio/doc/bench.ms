.OM
.TO
Steve Ridgway
.FR
Doug Tody
.SU
Performance of IRAF Image I/O
.PP
As Caty reported in her memo of 15 November, the timings of the \fIimarith\fR
task were surprisingly poor, i.e., approximately 20 cpu seconds for the
addition of two 200 column by 800 line short integer images, producing a
short integer image as output (a "short" integer is 16 bits).
A look at the code for \fIimarith\fR revealed
that the internal computations were being done in double precision floating,
regardless of the datatype of the images on disk.
I was not aware of this and I appreciate having it brought to my attention.
Fixing \fIimarith\fR took several hours and nearly cut the timings in half.
.PP
When I orginally implemented IMIO I planned to eventually make three major
optimizations (as noted in the program plan and system interface reference
manual):
.RS
.LP \(bu
Optimize the special case of line by line i/o with no automatic type
conversion, image sectioning, boundary extension, etc.
.LP \(bu
Provide direct access into the FIO file buffers when possible to eliminate
the memory to memory copy to and from the IMIO and FIO buffers.
.LP \(bu
Implement a optimal static file driver for UNIX to eliminate the overhead
of copying the data through the system buffer cache, and to permit
overlapped i/o.
.RE
.LP
I have gone ahead and implemented the first two optimizations; this took
a day and the changes were entirely internal to the interface,
requiring no changes to user code and no loss of machine independence.
After these changes were made to IMIO I ran several benchmarks with the
following results.  All benchmarks were for images with 16 bit integer pixels.
.TS
center box tab(|);
ci ci ci ci ci ci ci
r  n  n  n  nb n  n.
operation|open/close|line ovhead|kernel op|total user time|%opt|systime
-
(c=a+b)[200,800]|.38|1.43|1.69|3.50|48%|3.82
(c=a+b)[800,800]|.38|1.43|6.94|8.75|79%|12.16
minmax[800,800]|.05|0.59|11.39|12.03|95%|2.66
.TE
.PP
The columns in the table show the operation tested by the benchmark (two image
additions, each involving three images, and a computation of the minimum and
maximum of a single image), the overhead involved in opening and closing the
images (same operation on a [1,1] image), the total overhead to process the
image lines, the time consumed by the kernel operation, the total user time
for the task, the degree of optimality (ratio of time spent in the kernel
vector operation to the total time for the task),
and the system (UNIX kernel) time required.
.PP
In short, the time required by the original benchmark has decreased from
20 seconds to 3.5 seconds, disregarding the system time.  In this worst
case benchmark we still manage to come within 48% of the optimal time of
1.69 seconds for a VAX 11/750.
.PP
The short integer vector addition kernel operator was hand optimized in
assembler for these benchmarks to provide a true measure of the degree
of optimality.  The actual unoptimized UNIX vector addition operator is
slightly slower.
The last column, labelled "systime", shows the cpu time consumed
by the UNIX kernel moving the pixels to and from disk; this is the time
that will be eliminated by the static file driver optimization.
Once the static file driver is optimized any further optimizations
will be difficult.
