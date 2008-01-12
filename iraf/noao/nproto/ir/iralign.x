include <imhdr.h>
include "iralign.h"

define	NYOUT		16
define	NMARGIN		4

# IR_SHIFTS -- Compute the input and output image column limits and the
# x and y shifts.

procedure ir_shifts (ir, im, outim, xrshifts, yrshifts, xcshifts,
        ycshifts, ic1, ic2, il1, il2, oc1, oc2, ol1, ol2, deltax, deltay)

pointer	ir			# pointer to the ir structure
pointer	im			# pointer to the input image
pointer	outim			# pointer to the output image
real	xrshifts[ARB]		# x row shifts
real	yrshifts[ARB]		# y row shifts
real	xcshifts[ARB]		# x column shifts
real	ycshifts[ARB]		# y column shifts
int	ic1[ARB]		# input beginning column limits
int	ic2[ARB]		# input ending column limits
int	il1[ARB]		# input beginning line limits
int	il2[ARB]		# input ending line limits
int	oc1[ARB]		# output beginning column limits
int	oc2[ARB]		# output ending column limits
int	ol1[ARB]		# output beginning line limits
int	ol2[ARB]		# output ending line limits
real	deltax[ARB]		# x shifts
real	deltay[ARB]		# x shifts


int	i, j, k, nimages, nxsize, nysize, nimcols, nimlines
int	c1ref, c2ref, l1ref, l2ref, ideltax, ideltay

begin
	# Find the position in the output image of the reference subraster.
	nxsize = IR_NCOLS(ir) - IR_NXOVERLAP(ir)
	nysize = IR_NROWS(ir) - IR_NYOVERLAP(ir)
	c1ref = (IR_NXRSUB(ir) - 1) * nxsize + 1 + IR_XREF(ir)
	c2ref = c1ref + IR_NCOLS(ir) - 1
	l1ref = (IR_NYRSUB(ir) - 1) * nysize + 1 + IR_YREF(ir)
	l2ref = l1ref + IR_NROWS(ir) - 1
	nimages = IR_NXSUB(ir) * IR_NYSUB(ir)

	# Extract the subrasters one by one.
	do i = 1, nimages {

	    # Compute the indices of each subraster.
	    call ir_indices (i, j, k, IR_NXSUB(ir), IR_NYSUB(ir),
	        IR_CORNER(ir), IR_RASTER(ir), IR_ORDER(ir))

	    # Compute the indices of the input subraster.
	    nimcols = IM_LEN(im,1)
	    nimlines = IM_LEN(im,2)
	    ic1[i] = max (1, min (1 + (j - 1) * nxsize, nimcols)) 
	    ic2[i] = min (nimcols, max (1, ic1[i] + IR_NCOLS(ir) - 1))
	    il1[i] = max (1, min (1 + (k - 1) * nysize, nimlines)) 
	    il2[i] = min (nimlines, max (1, il1[i] + IR_NROWS(ir) - 1))

	    # Compute the shift relative to the input subraster.
	    call ir_mkshift (xrshifts, yrshifts, xcshifts, ycshifts,
	        IR_NXSUB(ir), IR_NYSUB(ir), j, k, IR_NXRSUB(ir),
		IR_NYRSUB(ir), IR_ORDER(ir), deltax[i], deltay[i])
	    ideltax = nint (deltax[i])
	    ideltay = nint (deltay[i])

	    # Get the output buffer.
	    oc1[i] = c1ref + (j - IR_NXRSUB(ir)) * IR_NCOLS(ir) +
	        ideltax
	    oc2[i] = c2ref + (j - IR_NXRSUB(ir)) * IR_NCOLS(ir) +
	        ideltax
	    ol1[i] = l1ref + (k - IR_NYRSUB(ir)) * IR_NROWS(ir) +
	        ideltay
	    ol2[i] = l2ref + (k - IR_NYRSUB(ir)) * IR_NROWS(ir) +
	        ideltay
	}
end


# IR_FSHIFTS -- Compute the input and output column limits.

procedure ir_fshifts (ir, im, outim, deltax, deltay, ic1, ic2, il1, il2,
	oc1, oc2, ol1, ol2)

pointer	ir			# pointer to the ir structure
pointer	im			# pointer to the input image
pointer	outim			# pointer to the output image
real	deltax[ARB]		# x shifts
real	deltay[ARB]		# x shifts
int	ic1[ARB]		# input beginning column limits
int	ic2[ARB]		# input ending column limits
int	il1[ARB]		# input beginning line limits
int	il2[ARB]		# input ending line limits
int	oc1[ARB]		# output beginning column limits
int	oc2[ARB]		# output ending column limits
int	ol1[ARB]		# output beginning line limits
int	ol2[ARB]		# output ending line limits


int	i, j, k, nimages, nxsize, nysize, nimcols, nimlines
int	c1ref, c2ref, l1ref, l2ref, ideltax, ideltay

begin
	# Find the position in the output image of the reference subraster.
	nxsize = IR_NCOLS(ir) - IR_NXOVERLAP(ir)
	nysize = IR_NROWS(ir) - IR_NYOVERLAP(ir)
	c1ref = (IR_NXRSUB(ir) - 1) * nxsize + 1 + IR_XREF(ir)
	c2ref = c1ref + IR_NCOLS(ir) - 1
	l1ref = (IR_NYRSUB(ir) - 1) * nysize + 1 + IR_YREF(ir)
	l2ref = l1ref + IR_NROWS(ir) - 1
	nimages = IR_NXSUB(ir) * IR_NYSUB(ir)

	# Extract the subrasters one by one.
	do i = 1, nimages {

	    # Compute the indices of each subraster.
	    call ir_indices (i, j, k, IR_NXSUB(ir), IR_NYSUB(ir),
	        IR_CORNER(ir), IR_RASTER(ir), IR_ORDER(ir))

	    # Compute the indices of the input subraster.
	    nimcols = IM_LEN(im,1)
	    nimlines = IM_LEN(im,2)
	    ic1[i] = max (1, min (1 + (j - 1) * nxsize, nimcols)) 
	    ic2[i] = min (nimcols, max (1, ic1[i] + IR_NCOLS(ir) - 1))
	    il1[i] = max (1, min (1 + (k - 1) * nysize, nimlines)) 
	    il2[i] = min (nimlines, max (1, il1[i] + IR_NROWS(ir) - 1))

	    # Compute the shift relative to the input subraster.
	    ideltax = nint (deltax[i])
	    ideltay = nint (deltay[i])

	    # Get the output buffer.
	    oc1[i] = c1ref + (j - IR_NXRSUB(ir)) * IR_NCOLS(ir) +
	        ideltax
	    oc2[i] = c2ref + (j - IR_NXRSUB(ir)) * IR_NCOLS(ir) +
	        ideltax
	    ol1[i] = l1ref + (k - IR_NYRSUB(ir)) * IR_NROWS(ir) +
	        ideltay
	    ol2[i] = l2ref + (k - IR_NYRSUB(ir)) * IR_NROWS(ir) +
	        ideltay
	}
end


# IR_SUBALIGN -- Align all the subrasters.

procedure ir_subalign (ir, im, outim, trimlimits, ic1, ic2, il1, il2,
	oc1, oc2, ol1, ol2, deltax, deltay, deltai, match, interp, verbose)

pointer	ir			# pointer to the ir structure
pointer	im			# pointer to the input image
pointer	outim			# pointer to the output image
char	trimlimits[ARB]		# compute the trim section
int	ic1[ARB]		# input image beginning columns
int	ic2[ARB]		# input image ending columns
int	il1[ARB]		# input image beginning rows
int	il2[ARB]		# input image ending rows
int	oc1[ARB]		# output image beginning columns
int	oc2[ARB]		# output image ending columns
int	ol1[ARB]		# output image beginning rows
int	ol2[ARB]		# output image ending rows
real	deltax[ARB]		# array of x shifts
real	deltay[ARB]		# array of y shifts
real	deltai[ARB]		# array of intensity shifts
int	match			# match intensities ?
int	interp			# type of interpolant
int	verbose			# print messages

int	i, k, tl1, tl2, tc1, tc2, nimcols, nimlines, nimages
int	ideltax, ideltay, lxoffset, hxoffset, lyoffset, hyoffset
int	ixoffset, iyoffset, nocols, norows, cin1, cin2, nicols
int	tlin1, lin1, lin2, nilines, lout1, lout2, nyout, fstline, lstline
pointer	sp,  x, y, msi, inbuf, outbuf, ptr
real	dx, dy, ytemp
int	ir_decode_section()
pointer	imps2r()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (x, IR_NCOLS(ir), TY_REAL)
	call salloc (y, IR_NCOLS(ir), TY_REAL)

	# Decode the trimsection.
	if (ir_decode_section (trimlimits, IR_NCOLS(ir), IR_NROWS(ir),
	    tc1, tc2, tl1, tl2) == ERR) {
	    tc1 = 0
	    tc2 = 0
	    tl1 = 0
	    tl2 = 0
	} else {
	    tc1 = max (0, min (tc1, IR_NCOLS(ir)))
	    tc2 = max (0, min (tc2, IR_NCOLS(ir)))
	    tl1 = max (0, min (tl1, IR_NROWS(ir)))
	    tl2 = max (0, min (tl2, IR_NROWS(ir)))
	}

	# Initialize the interpolant.
	call msiinit (msi, interp)

	nimcols = IM_LEN(outim,1)
	nimlines = IM_LEN(outim,2)

	# Extract the subrasters one by one.
	nimages = IR_NXSUB(ir) * IR_NYSUB(ir)
	do i = 1, nimages {

	    inbuf = NULL

	    # Reject and subraster which is off the image.
	    if (oc1[i] > nimcols || oc2[i] < 1 || ol1[i] > nimlines ||
		ol2[i] < 1)
		next

	    # Compute the integer and fractional part of the shift.
	    ideltax = nint (deltax[i])
	    ideltay = nint (deltay[i])
	    dx = deltax[i] - ideltax
	    dy = deltay[i] - ideltay

	    # Compute the output image limits.
	    lxoffset = max (1 - oc1[i], tc1)
	    hxoffset  = max (oc2[i] - nimcols, tc2)
	    oc1[i] = max (1, min (nimcols, oc1[i] + lxoffset))
	    oc2[i] = min (nimcols, max (1, oc2[i] - hxoffset))
	    nocols = oc2[i] - oc1[i] + 1
	    lyoffset = max (1 - ol1[i], tl1)
	    hyoffset  = max (ol2[i] - nimlines, tl2)
	    ol1[i] = max (1, min (nimlines, ol1[i] + lyoffset))
	    ol2[i] = min (nimlines, max (1, ol2[i] - hyoffset))
	    norows = ol2[i] - ol1[i] + 1

	    # Compute some input image parameters.
	    cin1 = max (ic1[i], min (ic1[i] + lxoffset - NMARGIN, ic2[i]))
	    cin2 = min (ic2[i], max (ic2[i] - hxoffset + NMARGIN, ic1[i]))
	    nicols = cin2 - cin1 + 1
	    
	    # Compute the x offset and x interpolation coordinates.
	    ixoffset = min (lxoffset, NMARGIN)
	    do k = 1, nicols
		Memr[x+k-1] = max (1.0, min (real (nicols), real (k + ixoffset -
		    dx)))

	    # Subdivide the image and do the shifting.
	    for (lout1 = ol1[i]; lout1 <= ol2[i]; lout1 = lout1 + NYOUT) {
		
		# Compute the output image limits.
		lout2 = min (ol2[i], lout1 + NYOUT - 1)
		nyout = lout2 - lout1 + 1

		# Compute the input image limits.
		tlin1 = il1[i] + lyoffset + lout1 - ol1[i]
		lin2 = min (il2[i], max (tlin1 + nyout + NMARGIN - 1, il1[i]))
		lin1 = max (il1[i], min (tlin1 - NMARGIN, il2[i]))
		nilines = lin2 - lin1 + 1

		# Get the appropriate input image section and fit the
		# interpolant.
		if ((inbuf == NULL) || (lin1 < fstline) || (lin2 > lstline)) {
		    fstline = lin1
		    lstline = lin2
		    call ir_buf (im, cin1, cin2, lin1, lin2, inbuf)
		    call msifit (msi, Memr[inbuf], nicols, nilines, nicols)
		}

		# Get the y offset and y interpolation coordinates. 
		#iyoffset = max (0, lout1 - ideltay - lin1)
		if (lout1 == ol1[i])
		    iyoffset = min (lyoffset, NMARGIN)
		else
		    iyoffset = tlin1 - lin1

	        # Shift the input images.
		outbuf = imps2r (outim, oc1[i], oc2[i], lout1, lout2)
		ptr = outbuf
		do k = 1, nyout {
		    ytemp = max (1.0, min (real (nilines), real (k + iyoffset -
		    dy))) 
		    call amovkr (ytemp, Memr[y], nocols)
		    call msivector (msi, Memr[x], Memr[y], Memr[ptr], nocols)
		    ptr = ptr + nocols
	        }

	        # Shift the intensities.
	        if (match == YES && ! IS_INDEFR(deltai[i]))
		    call aaddkr (Memr[outbuf], deltai[i], Memr[outbuf],
		        nocols * nyout)
	    }

	    if (inbuf != NULL)
	        call mfree (inbuf, TY_REAL)
	    inbuf = NULL

	    # Print a message.
	    if (verbose == YES) {
		call printf (" %s[%d:%d,%d:%d]  [%d:%d,%d:%d]  %g  %g")
		    call pargstr (IM_HDRFILE(im))
		    call pargi (ic1[i])
		    call pargi (ic2[i])
		    call pargi (il1[i])
		    call pargi (il2[i])
		    call pargi (lxoffset + 1)
		    call pargi (lxoffset + nocols) 
		    call pargi (lyoffset + 1)
		    call pargi (lyoffset + norows)
		    call pargr (deltax[i])
		    call pargr (deltay[i])
		call printf ("  %s[%d:%d,%d:%d]  %g\n")
		    call pargstr (IM_HDRFILE(outim))
		    call pargi (oc1[i])
		    call pargi (oc2[i])
		    call pargi (ol1[i])
		    call pargi (ol2[i])
		    call pargr (deltai[i])
	    }

	}

	call msifree (msi)
	call sfree (sp)
end


# IR_BUF -- Procedure to provide a buffer of image lines with minimum reads.

procedure ir_buf (im, col1, col2, line1, line2, buf)

pointer	im		# pointer to input image
int	col1, col2	# column range of input buffer
int	line1, line2	# line range of input buffer
pointer	buf		# buffer

int	i, ncols, nlines, nclast, llast1, llast2, nllast
pointer	buf1, buf2

pointer	imgs2r()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	}

	if (line1 < llast1) {
	    do i = line2, line1, -1 {
		if (i > llast1)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	} else if (line2 > llast2) {
	    do i = line1, line2 {
		if (i < llast2)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	}

	llast1 = line1
	llast2 = line2
	nclast = ncols
	nllast = nlines
end
