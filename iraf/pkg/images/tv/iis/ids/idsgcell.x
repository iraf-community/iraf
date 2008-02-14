# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include <gki.h>
include <gset.h>
include "../lib/ids.h"

# IDS_GETCELLARRAY -- Fetch a cell array, i.e., two dimensional array of pixels
# (greylevels or colors).

procedure ids_getcellarray (nc, nr, ax1,ay1, ax2,ay2)

int	nc, nr			# number of pixels in X and Y
int	ax1, ay1		# lower left corner of input window
int	ax2, ay2		# upper right corner of input window

int	x1, y1, x2, y2
int	nx,ny			# number of device pixels in x and y
real	px1, px2, py1, py2

real	skip_x, skip_y, sx, sy
real	blockx, blocky, bcy
int	i, j, startrow, element
real	xres, yres
pointer	sp, cell
pointer	mp			# final data pointer to "array" m
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

	# find out how many real pixels we have to fetch

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

	if ( nx > nc ) {
	    skip_x = real(nx)/nc
	    if ( (skip_x - 1.0)*(nc-1) < 1.0 )
		skip_x = 1.0
	} else
	    blockx = real(nc)/nx

	if ( ny > nr ) {
	    skip_y = real(ny)/nr
	    if ( (skip_y - 1.0)*(nr-1) < 1.0 )
		skip_y = 1.0
	} else
	    blocky = real(nr)/ny

	# initialize counters

	call smark(sp)

	# allocate storage for output

	call salloc (mp, nc*nr, TY_SHORT)
	sy = 0
	bcy = blocky
	startrow = 1

	# see if we can use original data ... no massaging
	# also set the initial value of the new_row flag, which tells
	# if we have to rebuild the row data
	# note that if blockx > 1.0, skip_x must be 1.0, and vv

	if ( (skip_x == 1.0) && (blockx == 1.0) ) {
	    use_orig = true
	} else {
	    use_orig = false
	    # allocate storage for a row of pixels.
	    call salloc ( cell, nx, TY_SHORT)
	}
	new_row = true

	# do it

	for ( i = 1; i <= nr ; i = i + 1) {

	# fetch the row data.  The reading routine will figure out
	# how to read from the various individual frames and bitplanes.

	    if ( new_row) {
		if (!i_snap)
		    call zseek (i_out, int(px1), int(py1)+int(sy+0.5))
	        if ( use_orig )
		    # just copy it in
		    if (i_snap)
		        call do_snap (Mems[mp+startrow-1], nx, int(px1),
	        	    int(py1)+int(sy+0.5))
		    else
			call read (i_out, Mems[mp+startrow-1], nx)
	        else
		    # into Mems for rework
		    if (i_snap)
		        call do_snap (Mems[cell], nx, int(px1),
	        	    int(py1)+int(sy+0.5))
		    else
			call read (i_out, Mems[cell], nx)
	    }

	# rework the row data

	    if ( !use_orig && new_row ) {
		if ( skip_x == 1.0)
		    call ids_blockit(Mems[cell], Mems[mp+startrow-1], nc,
		    		blockx)
		else {
	            sx = 0
	            for ( j = 1; j <= nc; j = j + 1) {
		        element = int(sx+0.5)
		        Mems[mp+startrow-1+j-1] = Mems[cell + element]
		        sx = sx + skip_x
	            }
		}
	    }
	# if don't need new row of input data, duplicate the
	# previous one by copying within the "m" array
	    if ( ! new_row )
		call amovs (Mems[mp+startrow-1-nc], Mems[mp+startrow-1], nc)

	#advance a row

	    startrow = startrow + nc
	    if ( bcy <= real(i) ) {
		sy = sy + skip_y
	        bcy = bcy + blocky
		new_row = true
	    } else {
		new_row = false
	    }
	}

	call gki_retcellarray (i_in, Mems[mp], nr * nc)
	call sfree(sp)
end
