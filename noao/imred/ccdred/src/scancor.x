include	<imhdr.h>
include	<imset.h>

define	SCANTYPES	"|shortscan|longscan|"
define	SHORTSCAN	1	# Short scan accumulation, normal readout
define	LONGSCAN	2	# Long scan continuous readout

# SCANCOR -- Create a scan flat from an unscanned flat field.

procedure scancor (input)

char	input[ARB]		# Input image
int	scantype		# Type of scan format
int	nscan			# Number of scan lines
int	readaxis		# Readout axis

int	clgwrd(), clgeti()
bool	clgetb(), ccdflag()
pointer	sp, output, str, in, out, immap()
errchk	immap, ccddelete

begin
	# Check if this operation is desired.
	if (!clgetb ("scancor"))
	    return

	# Check if this operation has been done.  Unfortunately this requires
	# mapping the image.

	in = immap (input, READ_ONLY, 0)
	if (ccdflag (in, "scancor")) {
	    call imunmap (in)
	    return
	}

	if (clgetb ("noproc")) {
	    call eprintf (
		"  [TO BE DONE] Convert %s to scan mode calibration\n")
		call pargstr (input)
	    call imunmap (in)
	    return
	}

	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Determine readout axis and create the temporary output image.
	scantype = clgwrd ("scantype", Memc[str], SZ_LINE, SCANTYPES)
	readaxis = clgwrd ("readaxis", Memc[str], SZ_LINE, "|lines|columns|")
	nscan = clgeti ("nscan")

	call mktemp ("tmp", Memc[output], SZ_FNAME)
	call set_output (in, out, Memc[output])

	# Make the output scan flat.
	switch (scantype) {
	case SHORTSCAN:
	    call shortscan (in, out, nscan, readaxis)
	case LONGSCAN:
	    call longscan (in, out, readaxis)
	}
		
	# Log the operation.
	switch (scantype) {
	case SHORTSCAN:
	    call sprintf (Memc[str], SZ_LINE,
		"Converted to shortscan with nscan=%d")
	    call pargi (nscan)
	case LONGSCAN:
	    call sprintf (Memc[str], SZ_LINE, "Converted to longscan format")
	}
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (in, Memc[str])
	call hdmpstr (out, "scancor", Memc[str])

	# Replace the input image by the output image.
	call imunmap (in)
	call imunmap (out)
	call ccddelete (input)
	call imrename (Memc[output], input)

	call sfree (sp)
end


# SHORTSCAN -- Make a shortscan mode flat field image by using a moving average.

procedure shortscan (in, out, nscan, readaxis)

pointer	in		# Input image
pointer	out		# Output image
int	nscan		# Number of lines scanned before readout
int	readaxis	# Readout axis

real	nscanr, sum
int	i, j, k, l, len1, len2, nc, nl, nscani, c1, c2, cs, l1, l2, ls
pointer	sp, str, bufs, datain, data, imgl2r(), impl2r()
errchk	malloc, calloc

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# The default data section is the entire image.
	len1 = IM_LEN(in,1)
	len2 = IM_LEN(in,2)
	c1 = 1
	c2 = len1
	cs = 1
	l1 = 1
	l2 = len2
	ls = 1
	call hdmgstr (in, "datasec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((c1<1)||(c2>len1)||(l1<1)||(l2>len2)||(cs!=1)||(ls!=1))
	    call error (0, "Error in DATASEC parameter")
	nc = c2 - c1 + 1
	nl = l2 - l1 + 1

	switch (readaxis) {
	case 1:
	    nscani = max (1, min (nscan, nl))
	    nscanr = nscani
	    call imseti (in, IM_NBUFS, nscani)
	    call malloc (bufs, nscani, TY_INT)
	    call calloc (data, nc, TY_REAL)
	    j = 1
	    k = 1
	    l = 1

	    # Copy initial lines.
	    do i = 1, l1 - 1
		call amovr (Memr[imgl2r(in,i)], Memr[impl2r(out,i)], len1)

	    # Ramp up
	    while (j <= nscani) {
		i = j + l1 - 1
		datain = imgl2r (in, i)
		call amovr (Memr[datain], Memr[impl2r(out,i)], len1)
		datain = datain + c1 - 1
		Memi[bufs+mod(j,nscani)] = datain
	        call aaddr (Memr[data], Memr[datain], Memr[data], nc)
	        j = j + 1
	    }
	    call adivkr (Memr[data], nscanr,
		Memr[impl2r(out,l+l1-1)+c1-1], nc)
	    l = l + 1

	    # Moving average
	    while (j <= nl) {
		datain = Memi[bufs+mod(k,nscani)]
	        call asubr (Memr[data], Memr[datain], Memr[data], nc)
		i = j + l1 - 1
		datain = imgl2r (in, i)
		call amovr (Memr[datain], Memr[impl2r(out,i)], len1)
		datain = datain + c1 - 1
		Memi[bufs+mod(j,nscani)] = datain
	        call aaddr (Memr[data], Memr[datain], Memr[data], nc)
	        call adivkr (Memr[data], nscanr,
		    Memr[impl2r(out,l+l1-1)+c1-1], nc)
	        j = j + 1
	        k = k + 1
	        l = l + 1
	    }

	    # Ramp down.
	    while (l <= nl) {
		datain = Memi[bufs+mod(k,nscani)]
	        call asubr (Memr[data], Memr[datain], Memr[data], nc)
	        call adivkr (Memr[data], nscanr,
		    Memr[impl2r(out,l+l1-1)+c1-1], nc)
	        k = k + 1
	        l = l + 1
	    }

	    # Copy final lines.
	    do i = l2+1, len2
		call amovr (Memr[imgl2r(in,i)], Memr[impl2r(out,i)], len1)

	    call mfree (bufs, TY_INT)
	    call mfree (data, TY_REAL)

	case 2:
	    nscani = max (1, min (nscan, nc))
	    nscanr = nscani
	    do i = 1, nl {
	        datain = imgl2r (in, i + l1 - 1)
		datain = datain + c1 - 1
	        data = impl2r (out, i + l1 - 1)
		call amovr (Memr[datain], Memr[data], len1)
		datain = datain + c1 - 1
		data = data + c1 - 1
		sum = 0
		j = 0
		k = 0
		l = 0

		# Ramp up
		while (j < nscani) {
		    sum = sum + Memr[datain+j]
		    j = j + 1
		}
		Memr[data] = sum / nscani
		l = l + 1

		# Moving average
		while (j < nl) {
		    sum = sum + Memr[datain+j] - Memr[datain+k]
		    Memr[data+l] = sum / nscani
		    j = j + 1
		    k = k + 1
		    l = l + 1
		}

		# Ramp down
		while (l < nl) {
		    sum = sum - Memr[datain+k]
		    Memr[data+l] = sum / nscani
		    k = k + 1
		    l = l + 1
		}
	    }
	}

	call sfree (sp)
end


# LONGSCAN -- Make a longscan mode readout flat field correction by averaging
# across the readout axis.

procedure longscan (in, out, readaxis)

pointer	in		# Input image
pointer	out		# Output image
int	readaxis	# Readout axis

int	i, nc, nl, c1, c2, cs, l1, l2, ls
int	in_c1, in_c2, in_l1, in_l2, ccd_c1, ccd_c2, ccd_l1, ccd_l2
real	asumr()
pointer	sp, str, data, imgl2r(), impl2r(), imps2r()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# The default data section is the entire image.
	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
	c1 = 1
	c2 = nc
	cs = 1
	l1 = 1
	l2 = nl
	ls = 1
	call hdmgstr (in, "datasec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((c1<1)||(c2>nc)||(l1<1)||(l2>nl)||(cs!=1)||(ls!=1))
	    call error (0, "Error in DATASEC parameter")
	in_c1 = c1
	in_c2 = c2
	in_l1 = l1
	in_l2 = l2

	# The default ccd section is the data section.
	call hdmgstr (in, "ccdsec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((cs != 1) || (ls != 1))
	    call error (0, "Error in CCDSEC parameter")
	ccd_c1 = c1
	ccd_c2 = c2
	ccd_l1 = l1
	ccd_l2 = l2
	if ((in_c2-in_c1 != ccd_c2-ccd_c1) || (in_l2-in_l1 != ccd_l2-ccd_l1))
	    call error (0, "Size of DATASEC and CCDSEC do not agree")

	switch (readaxis) {
	case 1:
	    IM_LEN(out,2) = 1
	    data = impl2r (out, 1)
	    call aclrr (Memr[data], nc)
	    nc = in_c2 - in_c1 + 1
	    nl = in_l2 - in_l1 + 1
	    data = data + in_c1 - 1
	    do i = in_l1, in_l2
		call aaddr (Memr[imgl2r(in,i)+in_c1-1], Memr[data],
		    Memr[data], nc)
	    call adivkr (Memr[data], real (nl), Memr[data], nc)
	    call sprintf (Memc[str], SZ_LINE, "[%d:%d,1:1]")
		call pargi (in_c1)
		call pargi (in_c2)
	    call hdmpstr (out, "datasec", Memc[str])
	    call sprintf (Memc[str], SZ_LINE, "[%d:%d,*]")
		call pargi (ccd_c1)
		call pargi (ccd_c2)
	    call hdmpstr (out, "ccdsec", Memc[str])
	case 2:
	    IM_LEN(out,1) = 1
	    data = imps2r (out, 1, 1, 1, nl)
	    call aclrr (Memr[data], nl)
	    nc = in_c2 - in_c1 + 1
	    nl = in_l2 - in_l1 + 1
	    do i = in_l1, in_l2
		Memr[data+i-1] = asumr (Memr[imgl2r(in,i)+in_c1-1], nc) / nc
	    call sprintf (Memc[str], SZ_LINE, "[1:1,%d:%d]")
		call pargi (in_l1)
		call pargi (in_l2)
	    call hdmpstr (out, "datasec", Memc[str])
	    call sprintf (Memc[str], SZ_LINE, "[*,%d:%d]")
		call pargi (ccd_l1)
		call pargi (ccd_l2)
	    call hdmpstr (out, "ccdsec", Memc[str])
	}

	call sfree (sp)
end
