# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	"display.h"

# DS_ULUTALLOC -- Generates a look up table from data supplied by user.
# The data is read from a two column text file of intensity, greyscale values.
# The input data are sorted, then mapped to the x range [0-4095].  A 
# piecewise linear look up table of 4096 values is then constructed from 
# the (x,y) pairs given.  A pointer to the look up table, as well as the z1 
# and z2 intensity endpoints, is returned.

pointer procedure ds_ulutalloc (fname, z1, z2)

char	fname[SZ_FNAME]		# Name of file with intensity, greyscale values
real	z1			# Intensity mapped to minimum gs value
real	z2			# Intensity mapped to maximum gs value

pointer	lut, sp, x, y
int	nvalues, i, j, x1, x2, y1
real	delta_gs, delta_xv, slope
errchk	ds_ulutread, ds_ulutsort, malloc		

begin
	call smark (sp)
	call salloc (x, U_MAXPTS, TY_REAL)	
	call salloc (y, U_MAXPTS, TY_REAL)

	# Read intensities and greyscales from the user's input file.  The
	# intensity range is then mapped into a standard range and the 
	# values sorted.

	call ds_ulutread (fname, Memr[x], Memr[y], nvalues)
	call alimr (Memr[x], nvalues, z1, z2)
	call amapr (Memr[x], Memr[x], nvalues, z1, z2, real(U_Z1), real(U_Z2))
	call ds_ulutsort (Memr[x], Memr[y], nvalues)

	# Fill lut in straight line segments - piecewise linear
	call malloc (lut, U_MAXPTS, TY_SHORT)	
	do i = 1, nvalues-1 {
	    delta_gs = Memr[y+i] - Memr[y+i-1]
	    delta_xv = Memr[x+i] - Memr[x+i-1]
	    slope = delta_gs / delta_xv
	    x1 = int (Memr[x+i-1]) 
	    x2 = int (Memr[x+i])
	    y1 = int (Memr[y+i-1])
	    do j = x1, x2
		Mems[lut+j] = y1 + slope * (j-x1)
	}
	Mems[lut+U_MAXPTS-1] = y1 + (slope * U_Z2)

	call sfree (sp)
	return (lut)
end


# DS_ULUTFREE -- Free the lookup table allocated by DS_ULUT.

procedure ds_ulutfree (lut)

pointer	lut

begin
	call mfree (lut, TY_SHORT)
end


# DS_ULUTREAD -- Read text file of x, y, values.

procedure ds_ulutread (utab, x, y, nvalues)

char	utab[SZ_FNAME]		# Name of list file
real	x[U_MAXPTS]		# Array of x values, filled on return
real	y[U_MAXPTS]		# Array of y values, filled on return
int	nvalues			# Number of values in x, y vectors - returned

int	n, fd
pointer	sp, lbuf, ip
real	xval, yval
int	getline(), open()
errchk	open, sscan, getline, salloc

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	iferr (fd = open (utab, READ_ONLY, TEXT_FILE))
	    call error (1, "Error opening user lookup table")

	n = 0
	while (getline (fd, Memc[lbuf]) != EOF) {
	    # Skip comment lines and blank lines.
	    if (Memc[lbuf] == '#')
		next
	    for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '\n' || Memc[ip] == EOS)
		next

	    # Decode the points to be plotted.
	    call sscan (Memc[ip])
		call gargr (xval)
		call gargr (yval)

	    n = n + 1
	    if (n > U_MAXPTS)
	        call error (2,
		    "Intensity transformation table cannot exceed 4096 values")

	    x[n] = xval
	    y[n] = yval
	}

	nvalues = n
	call close (fd)
	call sfree (sp)
end


# DS_ULUTSORT -- Bubble sort of paired arrays.  

procedure ds_ulutsort (xvals, yvals, nvals)

real	xvals[nvals]		# Array of x values
real	yvals[nvals]		# Array of y values
int	nvals			# Number of values in each array

int	i, j
real	temp
define	swap	{temp=$1;$1=$2;$2=temp}

begin
	for (i=nvals;  i > 1;  i=i-1)
	    for (j=1;  j < i;  j=j+1) 
		if (xvals[j] > xvals[j+1]) {
		    # Out of order; exchange y values
		    swap (xvals[j], xvals[j+1])
		    swap (yvals[j], yvals[j+1])
		}
end
