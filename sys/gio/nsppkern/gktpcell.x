# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gki.h>
include <gset.h>
include "gkt.h"

# Number of grey scale symbols
define NSYMBOL	11
define TSIZE	(1.0/2.0)

# GKT_PUTCELLARRAY -- Draw a cell array, i.e., two dimensional array of pixels
# (greylevels or colors).

procedure gkt_putcellarray (m, nc, nr, ax1,ay1, ax2,ay2)

short	m[ARB]			# cell array
int	nc, nr			# number of pixels in X and Y
				# (number of columns[x], rows[y]
int	ax1, ay1		# lower left corner of output window
int	ax2, ay2		# upper right corner of output window

int	x1,y1,x2,y2		# device coordinates
real	px1, py1, px2, py2
int	nx, ny, y
real	skip_x, skip_y, sx, sy
real	blockx, blocky, bcy
int	i, j, startrow, element
real	xres, yres
pointer	sp, cell, tx, txsave
bool	ca, use_orig, new_row, pr
real	z_scale
real	charheight, charwidth
real	delta_y
int	xrep, yrep

include "gkt.com"

begin
	call smark(sp)

	# Keep track of the number of drawing instructions since the last frame
	# clear.

	g_ndraw = g_ndraw + 1

	skip_x = 1.0
	skip_y = 1.0
	blockx = 1.0
	blocky = 1.0

	# Determine if can do real cell array.  If not, use character
	# sized boxes as pixels.  In that case, we need to save all
	# the character attributes since we will want to force default
	# character size, orientation, etc.

	ca = (GKT_CELLARRAY(g_kt) != 0)
	pr = false
	if ( ca ) {
	    xres = real(g_xres)
	    yres = real(g_yres)
	    pr = (GKT_PIXREP(g_kt) != 0)
	} else {
	    charwidth  = real(GKT_CHARWIDTH(g_kt,1))*TSIZE
	    charheight = real(GKT_CHARHEIGHT(g_kt,1))*TSIZE
	    xres = real(GKI_MAXNDC)/ charwidth
	    yres = real(GKI_MAXNDC)/ charheight
	    z_scale = 1.0 / sqrt ( real(max(NSYMBOL, GKT_ZRES(g_kt))) )
	    tx = GKT_TXAP(g_kt)
	    call salloc(txsave, LEN_TX, TY_INT)
	    call savetx(txsave,tx)
	}

	# Input arguments (ax, ay) refer to corners of put cell array;
	# we need corners of the corresponding device array.

	x1 = ax1
	x2 = ax2
	y1 = ay1
	y2 = ay2
	call adjust(x1,x2,xres)
	call adjust(y1,y2,yres)

	# Find out how many real pixels we have to fill
	px1 = real(x1)/(GKI_MAXNDC+1)
	py1 = real(y1)/(GKI_MAXNDC+1)
	px2 = real(x2)/(GKI_MAXNDC+1)
	py2 = real(y2)/(GKI_MAXNDC+1)

	nx = int( px2 * xres ) - int( px1 * xres ) + 1
	ny = int( py2 * yres ) - int( py1 * yres ) + 1

	if ( ny > 1)
	     delta_y = (real(y2) - real(y1))/ny
	else {
	     delta_y = 0.
	}

	# If too many data points in input, set skip.  If skip is close
	# enough to one, set it to one.
	# Set block replication factors - will be > 1.0 if too few input points.
	# Cannot set to 1.0 if "close" enough, since, if > 1.0, we don't have
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

	# Allocate storage for a row of pixels.  This is quite inefficient
	# if the x dimension of the cell array is small, but the metacode
	# won't be too much bigger (?).
	# need nx+1 in case nx odd ... pixels() wants to pad output.

	call salloc ( cell, nx+1, TY_SHORT)
	Mems[cell + nx] = 0

	# Initialize counters

	sy = skip_y
	bcy = blocky
	startrow = 1
	element = startrow

	# See if we can use original data ... no massaging
	# also set the initial value of the new_row flag, which tells
	# if we have to rebuild the row data
	# Note that if blockx > 1.0, skip_x must be 1.0, and vv

	if ( (skip_x == 1.0) && (blockx == 1.0) ) {
	    use_orig = true
	    new_row  = false
	} else {
	    use_orig = false
	    new_row  = true
	}

	# If device can pixel replicate, use that feature where we can
	if( pr) {
	    if( (skip_x == 1.0) && ( int(blockx) == blockx) ) {
		 xrep = int(blockx)
		 use_orig = true
		 nx = nc
	    } else
		 xrep = 1
	    if( (skip_y == 1.0) && ( int(blocky) == blocky) ) {
		 yrep = int(blocky)
		 ny = 1
	    } else
		 yrep = 1
	    call pixel0(1,0,xrep,0,1,yrep)
	}

	# Do it

	for ( i = 1; i <= ny ; i = i + 1) {

	    # Build the row data

	    if ( !use_orig && new_row ) {
		if ( skip_x == 1.0) {
		    call blockit(m[element], Mems[cell], nx, blockx)
		} else {
	            sx = skip_x
	            for ( j = 1; j <= nx; j = j + 1) {
		        Mems[cell+j-1] = m[element]
		        element = startrow + int(sx+0.5)
		        sx = sx + skip_x
	            }
		}
		if ( !ca )
		    if ( use_orig)
			call fakepc(m[element], Mems[cell], nx, z_scale)
		    else
			call fakepc(Mems[cell], Mems[cell], nx, z_scale)
	    }

	    # Send the row data.

	    if ( ca ) {
		y = y1 + ((i - 1)*delta_y + 0.5)
	        if ( use_orig ) {
		        call pixels( px1, real(y)/GKI_MAXNDC,
			    nx, 1, m[element])
	        } else {
		        call pixels( px1, real(y)/GKI_MAXNDC, nx, 1, Mems[cell])
	        }
	    }   
	    else
		call gkt_text( x1, y1+(i-1)*int(charheight), Mems[cell], nx)

	    # Advance a row

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

	# All done, restore text parameters and release storage

	if ( !ca )
	    call restoretx (txsave,tx)
	call sfree(sp)
end

# SAVETX --- save the current text parameters as pointed to by "txp"
# in the area pointed to by "savep", and then set the necessary
# defaults.

procedure savetx (savep, txp)
pointer savep, txp

include "gkt.com"

begin
	# save old values

	TX_UP(savep)	   = TX_UP(txp)
	TX_SIZE(savep)	   = TX_SIZE(txp)
	TX_PATH(savep)	   = TX_PATH(txp)
	TX_HJUSTIFY(savep) = TX_HJUSTIFY(txp)
	TX_VJUSTIFY(savep) = TX_VJUSTIFY(txp)
	TX_FONT(savep)	   = TX_FONT(txp)
	TX_COLOR(savep)	   = TX_COLOR(txp)
	TX_SPACING(savep)  = TX_SPACING(txp)

	# set new (default) ones

	TX_UP(txp)	= 90
	TX_SIZE(txp)	= GKI_PACKREAL(TSIZE)
	TX_PATH(txp)	= GT_RIGHT
	TX_HJUSTIFY(txp)= GT_LEFT
	TX_VJUSTIFY(txp)= GT_BOTTOM
	TX_FONT(txp)	= GT_ROMAN
	TX_COLOR(txp)	= 1
	TX_SPACING(txp)	= 0.0

	# Set the device attributes to undefined, forcing them to be reset
	# when the next output instruction is executed.

	GKT_TYPE(g_kt)		= -1
	GKT_WIDTH(g_kt)		= -1
	GKT_COLOR(g_kt)		= -1
	GKT_TXSIZE(g_kt)	= -1
	GKT_TXFONT(g_kt)	= -1
end

# RESTORETX --- restore the text parameters from the save area

procedure restoretx (savep, txp)
pointer savep, txp

include "gkt.com"

begin
	# Restore values

	TX_UP(txp)	 = TX_UP(savep)
	TX_SIZE(txp)	 = TX_SIZE(savep)
	TX_PATH(txp)	 = TX_PATH(savep)
	TX_HJUSTIFY(txp) = TX_HJUSTIFY(savep)
	TX_VJUSTIFY(txp) = TX_VJUSTIFY(savep)
	TX_FONT(txp)	 = TX_FONT(savep)
	TX_COLOR(txp)	 = TX_COLOR(savep)
	TX_SPACING(txp)  = TX_SPACING(savep)

	# Set the device attributes to undefined, forcing them to be reset
	# when the next output instruction is executed.

	GKT_TYPE(g_kt)		= -1
	GKT_WIDTH(g_kt)		= -1
	GKT_COLOR(g_kt)		= -1
	GKT_TXSIZE(g_kt)	= -1
	GKT_TXFONT(g_kt)	= -1
end

# FAKEPC --- fake putcell output by using appropriately chosen text
# characters to make grey scale.

procedure fakepc (indata, outdata, nx,  scale)
int	nx			# number of points in row
short	indata[ARB]		# input row data
short	outdata[ARB]		# output row data
real	scale			# intensity scaling factor

include "gkt.com"

int	i
real	temp
char	cdata[NSYMBOL]		# characters to represent intensity
data cdata /' ', '.', ':', '|', 'i', 'l', 'J', 'm', '#', 'S', 'B', EOS/

begin
	#
	for ( i = 1 ; i <= nx ; i = i + 1 ) {
	    temp = sqrt( max(0., real(indata[i])) )
	    outdata[i] = cdata[ min( NSYMBOL, int(NSYMBOL*scale*temp)+1 ) ]
	}
end

# BLOCKIT -- block replication of data

procedure blockit( from, to, count, factor)

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

# ADJUST -- round/truncate putcell array corners to device coordinates
# move up lower bound if it is above center point of device cell,
# move down upper bound if below.  Don't allow bounds to go beyond
# resolution or below zero.  Do not allow bounds to cross.  Part of the
# assumptions behind all this is that putcells will be continguous and
# rows/columns must not be plotted twice.

procedure adjust ( lower, upper, res)

int	lower, upper
real	res

real	factor
real	low, up

begin
	factor = res/(GKI_MAXNDC+1)
	low = real(lower) * factor
	up  = real(upper) * factor

	# if boundaries result in same row, return
	if ( int(low) == int(up) )
	    return

	# if low is in upper half of device pixel, round up
	if ( (low - int(low)) >= 0.5 ) {
	    low = int(low) + 1
	    # don't go to or beyond upper bound
	    if ( low < up ) {
	        # ... 0.2 just for "rounding protection";
		lower = (low + 0.2)/factor
	        # if now reference same cell, return
		if ( int(low) == int(up) )
		    return
	    }
	}

	# if "up" in bottom half of pixel, drop down one.  Note that
	# due to two "==" tests above, upper will not drop below lower.
	# 0.2 means drop partway down into pixel below; calling code will
	# truncate.
	if ( (up - int(up)) < 0.5 )
	    upper = real(int(up) - 0.2)/factor
end
