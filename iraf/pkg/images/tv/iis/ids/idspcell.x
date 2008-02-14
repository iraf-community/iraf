# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include <gset.h>
include "../lib/ids.h"

# number of grey scale symbols
define NSYMBOL	11
define TSIZE	(1.0/2.0)

# IDS_PUTCELLARRAY -- Draw a cell array, i.e., two dimensional array of pixels
# (greylevels or colors).

procedure ids_putcellarray (m, nc, nr, ax1,ay1, ax2,ay2)

short	m[ARB]			# cell array
int	nc, nr			# number of pixels in X and Y
				# (number of columns[x], rows[y]
int	ax1, ay1		# lower left corner of output window
int	ax2, ay2		# upper right corner of output window

int	x1,y1,x2,y2
real	px1, py1, px2, py2
int	nx, ny
real	skip_x, skip_y, sx, sy
real	blockx, blocky, bcy
int	i, j, startrow, element
real	xres, yres
pointer	sp, cell
bool	ca, use_orig, new_row

include "../lib/ids.com"

begin
	# determine if can do real cell array.

	ca = (IDS_CELLARRAY(i_kt) != 0)
	if ( !ca )
	     return

	skip_x = 1.0
	skip_y = 1.0
	blockx = 1.0
	blocky = 1.0

	xres = real(i_xres)
	yres = real(i_yres)

	# adjust pixels for edges
	x1 = ax1
	x2 = ax2
	y1 = ay1
	y2 = ay2
	call ids_cround(x1,x2,xres)
	call ids_cround(y1,y2,yres)

	# find out how many real pixels we have to fill

	px1 = real(x1) * xres /(GKI_MAXNDC+1)
	py1 = real(y1) * yres /(GKI_MAXNDC+1)
	px2 = real(x2) * xres /(GKI_MAXNDC+1)
	py2 = real(y2) * yres /(GKI_MAXNDC+1)

	nx = int( px2 ) - int( px1 ) + 1
	ny = int( py2 ) - int( py1 ) + 1

	# if too many data points in input, set skip.  If skip is close
	# enough to one, set it to one.
	# set block replication factors - will be > 1.0 if too few input points.
	# cannot set to 1.0 if "close" enough, since, if > 1.0, we don't have
	# enough points and so *some* have to be replicated.

	if ( nc > nx ) {
	    skip_x = real(nc)/nx
	    if ( (skip_x - 1.0)*(nx-1) < 1.0 )
		skip_x = 1.0
	} else
	    blockx = real(nx)/nc

	if ( nr > ny ) {
	    skip_y = real(nr)/ny
	    if ( (skip_y - 1.0)*(ny-1) < 1.0 )
		skip_y = 1.0
	} else
	    blocky = real(ny)/nr

	# initialize counters

	call smark(sp)
	sy = skip_y
	bcy = blocky
	startrow = 1
	element = startrow

	# see if we can use original data ... no massaging
	# also set the initial value of the new_row flag, which tells
	# if we have to rebuild the row data
	# note that if blockx > 1.0, skip_x must be 1.0, and vv

	if ( (skip_x == 1.0) && (blockx == 1.0) ) {
	    use_orig = true
	    new_row  = false
	} else {
	    use_orig = false
	    new_row  = true
	    # allocate storage for a row of pixels.
	    call salloc ( cell, nx, TY_SHORT)
	}

	# do it

	for ( i = 1; i <= ny ; i = i + 1) {

	    # Build the row data.

	    if (!use_orig && new_row) {
		if ( skip_x == 1.0)
		    call ids_blockit(m[element], Mems[cell], nx, blockx)
		else {
	            sx = skip_x
	            for ( j = 1; j <= nx; j = j + 1) {
		        Mems[cell+j-1] = m[element]
		        element = startrow + int(sx+0.5)
		        sx = sx + skip_x
	            }
		}
	    }

	    # Send the row data.  The writing routine will figure out
	    # how to send to the various individual frames and bitplanes.

	    call zseek (i_out, int(px1), int(py1)+i-1)
	    if (use_orig)
		call write (i_out, m[element], nx)
	    else
		call write (i_out, Mems[cell], nx)

	    # Advance a row.

	    element = startrow
	    if ( bcy <= real(i) ) {
		startrow = 1 + nc * int(sy+0.5)
		element = startrow
		sy = sy + skip_y
	        bcy = bcy + blocky
		new_row = true
	    } else {
		new_row = false
	    }
	}

	call sfree(sp)
end


# IDS_BLOCKIT -- block replication of data

procedure ids_blockit( from, to, count, factor)

short	from[ARB]			# input data
short	to[ARB]				# output data
int	count				# number of output pixels
real	factor				# blocking factor

int	i, j
real	bc

begin
	bc = factor
	j = 1
	for ( i = 1; i <= count ; i = i + 1 ) {
	    to[i] = from[j]
	    if ( bc <= real(i) ) {
		j = j + 1
		bc = bc + factor
	    }
	}
end
