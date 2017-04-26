include	<imhdr.h>

# READCOR -- Create a readout image.
# Assume it is appropriate to perform this operation on the input image.
# There is no CCD type checking.

procedure readcor (input)

char	input[ARB]		# Input image
int	readaxis		# Readout axis

int	i, nc, nl, c1, c2, cs, l1, l2, ls
int	in_c1, in_c2, in_l1, in_l2, ccd_c1, ccd_c2, ccd_l1, ccd_l2
pointer	sp, output, str, in, out, data

real	asumr()
int	clgwrd()
bool	clgetb(), ccdflag()
pointer	immap(), imgl2r(), impl2r(), imps2r()
errchk	immap, ccddelete

begin
	# Check if this operation is desired.
	if (!clgetb ("readcor"))
	    return

	# Check if this operation has been done.  Unfortunately this requires
	# mapping the image.

	in = immap (input, READ_ONLY, 0)
	if (ccdflag (in, "readcor")) {
	    call imunmap (in)
	    return
	}

	if (clgetb ("noproc")) {
	    call eprintf (
		"  [TO BE DONE] Convert %s to readout correction\n")
		call pargstr (input)
	    call imunmap (in)
	    return
	}

	call smark (sp)
	call salloc (output, SZ_FNAME, TY_CHAR)
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

	# Determine the readout axis.
	readaxis = clgwrd ("readaxis", Memc[str], SZ_LINE, "|lines|columns|")

	# Create output.
	call mktemp ("tmp", Memc[output], SZ_FNAME)
	call set_output (in, out, Memc[output])

	# Average across the readout axis.
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

	# Log the operation.
	call sprintf (Memc[str], SZ_LINE,
	    "Converted to readout format")
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (in, Memc[str])
	call hdmpstr (out, "readcor", Memc[str])

	# Replace the input image by the output image.
	call imunmap (in)
	call imunmap (out)
	call ccddelete (input)
	call imrename (Memc[output], input)

	call sfree (sp)
end
