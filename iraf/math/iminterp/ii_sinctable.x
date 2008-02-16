include <math.h>

# II_SINCTABLE -- Compute the 1D sinc function look-up tables given the
# width of the sinc function and the number of increments.

procedure ii_sinctable (table, nconv, nincr, xshift)

real	table[nconv,nincr]		#O the computed look-up table
int	nconv				#I the sinc truncation length
int	nincr				#I the number of look-up tables
real	xshift				#I the shift of the look up table

int	i, j, nsinc
real	sconst, a2, a4, fsign, xsign, sum, dx, dx2, x, f

begin
	# Set up some constants.
	nsinc = (nconv - 1) / 2
	sconst = (HALFPI / nsinc) ** 2
	a2 = -0.49670
	a4 = 0.03705
	if (mod (nsinc, 2) == 0)
	    fsign = 1.0
        else
	    fsign = -1.0

	# Create a one entry look-up table.
	if (! IS_INDEFR(xshift)) {

	    dx = xshift
	    x = -nsinc - dx
	    xsign = fsign
	    sum = 0.0
	    do j = 1, nconv {
		if (x == 0.0)
		    f = 1.0
		else if (dx == 0.0)
		    f = 0.0
		else {
		    dx2 = sconst * (j - nsinc - 1) ** 2
		    f = xsign / x * (1.0 + a2 * dx2 + a4 * dx2 * dx2) ** 2
		}
		table[j,1] = f
		sum = sum + f
		x = x + 1.0
		xsign = -xsign
	    }
	    do j = 1, nconv
	        table[j,1] = table[j,1] / sum

	# Create a multi-entry evenly spaced look-up table.
	} else {

	    do i = 1, nincr {
	        dx = -0.5 + real (i - 1) / real (nincr - 1)
	        x = -nsinc + dx
		xsign = fsign
	        sum = 0.0
	        do j = 1, nconv {
		    if ((x >= - 0.5 / (nincr - 1)) && (x < 0.5 / (nincr - 1)))
		        f = 1.0
		    else if ((dx >= -0.5 / (nincr - 1)) &&
		        (dx < 0.5 / (nincr - 1)))
		        f = 0.0
		    else {
		        dx2 = sconst * (j - nsinc - 1) ** 2
		        f = xsign / x * (1.0 + a2 * dx2 + a4 * dx2 * dx2) ** 2
		    }
		    table[j,i] = f
		    sum = sum + f
		    x = x + 1.0
		    xsign = -xsign
	        }
	        do j = 1, nconv
		    table[j,i] = table[j,i] / sum
	    }
	}
end


# II_BISINCTABLE -- Compute the 2D sinc function look-up tables given the
# width of the sinc function and the number of increments.

procedure ii_bisinctable (table, nconv, nxincr, nyincr, xshift, yshift)

real	table[nconv,nconv,nxincr,nyincr] #O the computed look-up table
int	nconv				 #I the sinc truncation length
int	nxincr, nyincr			 #I the number of look-up tables
real	xshift, yshift			 #I the shift of the look up table

int	j, ii, jj
pointer	sp, fx, fy

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (fx, nconv * nxincr, TY_REAL)
	call salloc (fy, nconv * nyincr, TY_REAL)

	# Create a one entry look-up table.
	if (! IS_INDEFR(xshift) && ! IS_INDEFR(yshift)) {

	    call ii_sinctable (Memr[fx], nconv, 1, xshift)
	    call ii_sinctable (Memr[fy], nconv, 1, yshift)
	    do j = 1, nconv {
		call amulkr (Memr[fx], Memr[fy+j-1], table[1,j,1,1], nconv)
	    }

	} else {

	    call ii_sinctable (Memr[fx], nconv, nxincr, xshift)
	    call ii_sinctable (Memr[fy], nconv, nyincr, yshift)
	    do jj = 1, nyincr {
		do ii = 1, nxincr {
		    do j = 1, nconv
			call amulkr (Memr[fx+(ii-1)*nconv],
			    Memr[fy+(jj-1)*nconv+j-1], table[1,j,ii,jj], nconv)
		}
	    }
	}

	call sfree (sp)
end
