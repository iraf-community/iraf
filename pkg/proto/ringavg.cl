# RINGAVG (Nov02)                   proto                  RINGAVG (Nov02)
# 
# 
# NAME
#     ringavg -- compute pixel averages in concentric  rings  about  given
#     center
#     
#     
# USAGE
#     ringavg image xc yc
#     
#     
# PARAMETERS
#     
#     image
#         Image to be used.
#     
#     xc, yc
#         Pixel coordinate for center of rings.
#     
#     r1 = 0, r2 = 10, dr = 1
#         Rings to be measured.  R1 is the inner radius of the first ring,
#         R2 is the outer radius of the last bin, and DR is the widths  of
#         the rings.  The values are in units of pixels.
#     
#     labels = yes
#         Print column labels for the output?
#     
#     vebar = no
#         If  VEBAR  is yes then the standard deviation and standard error
#         will be printed as negative values for use with GRAPH.
#     
#     
# DESCRIPTION
#     Pixels are binned into a series of concentric rings  centered  on  a
#     given  position  in  the  input  image.  The rings are defined by an
#     inner radius for the first ring, an outer radius for the last  ring,
#     and  the  width of the rings.  The statistics of the pixel values in
#     each ring are then computed and list to the  standard  output.   The
#     output  lines  consist of the inner and outer ring radii, the number
#     of pixels, the average value, the standard deviation  of  the  value
#     (corrected  for  population  size),  and  the  standard  error.  The
#     parameter LABEL selects whether to include column labels.
#     
#     If the ring average are to be plotted with the task GRAPH using  the
#     option  to  plot  error  bars  based  on  the  standard deviation or
#     standard error then the VEBAR parameter may  be  set  to  write  the
#     values as negative values are required by that task.
#     
#     This  task  is  a  script  and so users may copy it and modify it as
#     desired.  Because it is a script it will be very slow if r2  becomes
#     large.
#     
#     
# EXAMPLES
#     1. Compute the ring averages with labels and output to the terminal.
#     
#         cl> ringavg pwimage 17 17
#         #  R min    R max     Npix    Average    Std Dev    Std Err
#             0.00     1.00        5      7.336       9.16      4.096
#             1.00     2.00        8     0.2416     0.2219    0.07844
#             2.00     3.00       16     0.3994     0.5327     0.1332
#             3.00     4.00       20    0.06211    0.05491    0.01228
#             4.00     5.00       32     0.0987    0.08469    0.01497
#             5.00     6.00       32    0.06983    0.06125    0.01083
#             6.00     7.00       36     0.0641     0.0839    0.01398
#             7.00     8.00       48    0.06731    0.05373   0.007755
#             8.00     9.00       56    0.06146    0.07601    0.01016
#             9.00    10.00       64    0.05626    0.05846   0.007308
#     
#     2.  Plot the ring averages with standard errors used for error bars.
#     
#         cl> ringavg pwimage 17 17 label- vebar+ | fields STDIN 2,4,6 |
#         >>> graph point+ marker=vebar
#     
#     3.  Plot ring averages for galaxy in dev$pix.
#     
#         cl> ringavg dev$pix 256 256 r2=100 dr=5 label- | fields STDIN 2,4 |
#         >>> graph logy+
#     
#     
#     
# SEE ALSO
#     pradprof, psfmeasure, radprof
#
#
# To install:
# 
# Copy to your home or other personal directory.  Enter the command
# "task ringavg = home$ringavg.cl" interactively, in login.cl or in
# your loginuser.cl.  Substitute the host or logical path for home$
# if the script is placed in a different directory.


procedure ringavg (image, xc, yc)

file	image			{prompt="Input image"}
real	xc			{prompt="X center"}
real	yc			{prompt="Y center"}

real	r1 = 0			{prompt="Inner radius of first bin"}
real	r2 = 10			{prompt="Outer radius of last bin"}
real	dr = 1			{prompt="Radial bin width"}

bool	labels = yes		{prompt="Print column labels?"}
bool	vebars = no		{prompt="Format for error bars in GRAPH?"}

struct	*fd

begin
	file	temp
	real	n, r, val, ra, rb, ravg, rstddev, rmean

	# Extract the pixel values sorted by radius.
	temp = mktemp ("temp")
	pradprof (image, xc, yc, radius=r2, center=no, list=yes) |
	    sort ("STDIN", column=1, ignore_white+, numeric+, reverse-, > temp)

	if (label)
	    printf ("# %6s %8s %8s %10s %10s %10s\n", "R min", "R max", "Npix",
		"Average", "Std Dev", "Std Err")

	# Read through the file.  Skip the first two comment lines.
	fd = temp
	i = fscan (fd)
	i = fscan (fd)
	n = 0
	rb = -1
	while (fscan (fd, r, val) != EOF) {
	    if (r < r1)
		next
	    if (r > r2)
		break
	    if (r > rb) {
		if (n > 0) {
		    ravg = ravg / n
		    rstddev = sqrt (rstddev / n - ravg ** 2)
		    if (vebar)
			rstddev = -rstddev
		    if (n > 1)
			rstddev = rstddev * sqrt (n / (n - 1.))
		    rmean = rstddev / sqrt (n)
		    printf ("%8.2f %8.2f %8d %10.4g %10.4g %10.4g\n",
			ra, rb, n, ravg, rstddev, rmean)
		}
		ravg = 0.
		rstddev = 0.
		n = 0
		ra = int (r / dr) * dr
		rb = ra + dr
	    }

	    ravg = ravg + val
	    rstddev = rstddev + val * val
	    n = n + 1
	}

	if (n > 0) {
	    ravg = ravg / n
	    rstddev = sqrt (rstddev / n - ravg ** 2)
	    if (vebar)
		rstddev = -rstddev
	    if (n > 1)
		rstddev = rstddev * sqrt (n / (n - 1.))
	    rmean = rstddev / sqrt (n)
	    printf ("%8.2f %8.2f %8d %10.4g %10.4g %10.4g\n",
		ra, rb, n, ravg, rstddev, rmean)
	}

	fd = ""
	delete (temp, verify-)
end
