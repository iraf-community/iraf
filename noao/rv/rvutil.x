include <gset.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcomdef.h"
include "rvkeywords.h"
include "rvsample.h"

# RVUTIL.X -- Catch-all file that contains various and sundry utility routines
# used throughout the package.


# DEX - Raise 'cv' to a power of ten (10)

double procedure dex (cv)

real	cv

double	ln10, dx

begin
	ln10 = 2.30258509299404d0
	if ((cv*ln10) > 512) {
	    call rv_errmsg ("dex(): cv = %f")
	        call pargr (cv)
	    call flush (STDERR)
	    call error (0, "Floating overflow would have been tripped.")
	}
	dx = exp (cv * ln10)

	return (dx)
end


# RV_AVGPIX - Find average pixel value in an array

real procedure rv_avgpix (data, npts)

real	data[npts]			#I data array
int	npts				#I No. points in array

real	avg
double	sum
int	i

begin
	sum = 0.0
	do i = 1, npts 
	     sum = sum + double (data[i])

	avg = real (sum / double (npts))

	return ( avg )
end


# RV_CUT - Mark a regions of the spectrum to be used in the correlation.
# Appends to the current regions string

procedure rv_cut (rv, x, sx, ex)

pointer	rv					#I RV struct pointer
real	x					#I Current cursor x position 
real	sx					#O Start x position 
real	ex					#O End x position 

double	dex()
real 	sregion, eregion, yp

begin
    	sregion = x			# get endpoints
    	call rv_getpts (rv, eregion, yp, 1)
	if (RV_PIXCORR(rv) == NO && RV_DCFLAG(rv) != -1) {
	    call rv_fixx (sregion, eregion, real(dex(RV_GLOB_W1(rv))),
	        real(dex(RV_GLOB_W2(rv))))
	} else {
	    call rv_fixx (sregion, eregion, RV_GLOB_W1(rv), RV_GLOB_W2(rv))
	}

	sx = sregion
	ex = eregion
end


# RV_FILL_BLANKS - Given an input string, substitue blanks with an underscore.

procedure rv_fill_blanks (in, out, maxch)

char	in[maxch], out[maxch]
int	maxch
int	i

begin
	i = 1
	while (in[i] != EOS && i != maxch) {
	    if (in[i] == ' ')
		out[i] = '_'
	    else
		out[i] = in[i]
	    i = i + 1 
	}
	out[i] = EOS
end


# RV_FIXX - Check for bounds on x's.

procedure rv_fixx (x1, x2, lx1, rx2)

real	x1				#U 'left'  x cursor
real	x2				#U 'right' x cursor
real	lx1				#I min allowed x point
real	rx2				#I max allowed x point

real	temp

begin
	if (x2 < x1) {			# Swap 'em
	    temp = min (x2, rx2)
	    x2 = max (x1, lx1)
	    x1 = temp
	}
end


# RV_GETPTS - Read cursor to get another point.

procedure rv_getpts (rv, x, y, owcs)

pointer	rv				#I RV struct pointer
real	x, y				#I Cursor coords
int	owcs				#I Output wcs of coords

int	wcs, key, stat
char	command[SZ_FNAME]
int	clgcur()

begin
	call printf ("again: ")
	stat = clgcur ("cursor", x, y, wcs, key, command, SZ_LINE)
	if (owcs != wcs)
	    call gctran (RV_GP(rv), x, y, x, y, wcs, owcs)
	call printf ("      \n")
	
end


# RV_GETSHIFT - Find an extreme in the data array of type indicated and
# return the index in the array.

int procedure rv_getshift (data, npts, type)

real	data[npts]			#I data array
int	npts				#I No. points in array
int	type				#I type of extreme to find

int	i, imax, imin
real	max, min

begin
	if (type == MAXIMUM) {
	    max = data[1]
	    imax = 1
	    do i = 2,npts {
	       if (data[i] > max) {
		  max = data[i]
	  	  imax = i
	       }
	    }
	    return (imax)

	} else {
	    min = data[1]
	    imin = 1
	    do i = 2,npts {
	       if (data[i] < min) {
		  min = data[i]
		  imin = i
	       }
	    }
	    return (imin)
	}
end


# RV_MAXPIX - Find maximum pixel value in an array

real procedure rv_maxpix (data, npts)

real	data[npts]			#I data array
int	npts				#I No. points in array

real	max
int	i

begin
	max = data[1]
	do i = 2,npts 
	     if (data[i] > max)
		max = data[i]

	return (max)
end


# RV_MINPIX - Find minimum pixel value in an array

real procedure rv_minpix (data, npts)

real	data[npts]			#I data array
int	npts				#I No. points in array

real	min
int	i

begin
	min = data[1]
	do i = 2,npts 
	     if (data[i] < min)
		min = data[i]

	return (min)
end


# RV_PAUSE - Print a string and await any key for an action.

procedure rv_pause (str)
char    str[ARB]  
 
real    x, y
int     wcs, key, stat
char    command[SZ_FNAME]
int     clgcur()  
 
begin
        call printf ("%s")
             call pargstr (str)
        call flush (STDOUT)
        stat = clgcur ("cursor", x, y, wcs, key, command, SZ_FNAME)
 
        if ('I' == key)
            call error (0, "Quitting")
        
        return
end
 
 
# RV_PRSHIFT - Use the cursor to print the difference in pixels.

procedure rv_prshift (rv, xpos)

pointer	rv				#I RV struct pointer
real	xpos				#I 1st x position

real	xpos2, y, shift, pix_shift
double	rv_shift2vel()

begin
	call rv_getpts(rv, xpos2, y, 1)
	
	shift = xpos2 - xpos
	pix_shift = (log10(xpos2) - log10(xpos)) / RV_OWPC(rv)
	call rv_mode_prompt (rv)
	if (RV_DCFLAG(rv) != -1) {
	   call printf (" Difference = %.2f Km/sec (~%d pix) (~%.3f A)\n")
	        call pargd (rv_shift2vel(rv,pix_shift))
	        call pargi (int(pix_shift))
		call pargr (shift)

	} else {
	   call printf (" Difference = %.2f pixels.\n")
	        call pargr (pix_shift)
	}
end
