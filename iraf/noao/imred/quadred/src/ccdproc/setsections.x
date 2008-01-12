include	<imhdr.h>
include	"ccdred.h"

# SET_SECTIONS -- Set the data section, ccd section, trim section and
# bias section.

procedure set_sections (ccd)

pointer	ccd			# CCD structure (returned)

pointer	sp, str
int	nc, nl, c1, c2, cs, l1, l2, ls
bool	streq()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	nc = IM_LEN(IN_IM(ccd),1)
	nl = IM_LEN(IN_IM(ccd),2)

	# The default data section is the entire image.
	c1 = 1
	c2 = nc
	cs = 1
	l1 = 1
	l2 = nl
	ls = 1
	call hdmgstr (IN_IM(ccd), "datasec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((c1<1)||(c2>nc)||(l1<1)||(l2>nl)||(cs!=1)||(ls!=1))
	    call error (0, "Error in DATASEC parameter")
	IN_C1(ccd) = c1
	IN_C2(ccd) = c2
	IN_L1(ccd) = l1
	IN_L2(ccd) = l2

	# The default trim section is the data section.
	# Defer limit checking until actually used.
	c1 = IN_C1(ccd)
	c2 = IN_C2(ccd)
	l1 = IN_L1(ccd)
	l2 = IN_L2(ccd)
	call clgstr ("trimsec", Memc[str], SZ_LINE)
	if (streq (Memc[str], "image"))
	    call hdmgstr (IN_IM(ccd), "trimsec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((cs!=1)||(ls!=1))
	    call error (0, "Error in TRIMSEC parameter")
	TRIM_C1(ccd) = c1
	TRIM_C2(ccd) = c2
	TRIM_L1(ccd) = l1
	TRIM_L2(ccd) = l2

	# The default bias section is the whole image.
	# Defer limit checking until actually used.
	c1 = 1
	c2 = nc
	l1 = 1
	l2 = nl
	call clgstr ("biassec", Memc[str], SZ_LINE)
	if (streq (Memc[str], "image"))
	    call hdmgstr (IN_IM(ccd), "biassec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((cs!=1)||(ls!=1))
	    call error (0, "Error in BIASSEC parameter")
	BIAS_C1(ccd) = c1
	BIAS_C2(ccd) = c2
	BIAS_L1(ccd) = l1
	BIAS_L2(ccd) = l2

	# The default ccd section is the size of the data section.
	c1 = 1
	c2 = IN_C2(ccd) - IN_C1(ccd) + 1
	l1 = 1
	l2 = IN_L2(ccd) - IN_L1(ccd) + 1
	call hdmgstr (IN_IM(ccd), "ccdsec", Memc[str], SZ_LINE)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((cs != 1) || (ls != 1))
	    call error (0, "Error in CCDSEC parameter")
	CCD_C1(ccd) = c1
	CCD_C2(ccd) = c2
	CCD_L1(ccd) = l1
	CCD_L2(ccd) = l2
	if ((IN_C2(ccd)-IN_C1(ccd) != CCD_C2(ccd)-CCD_C1(ccd)) ||
	    (IN_L2(ccd)-IN_L1(ccd) != CCD_L2(ccd)-CCD_L1(ccd)))
	    call error (0, "Size of DATASEC and CCDSEC do not agree")

	# The default output data section is the input data section.
	OUT_C1(ccd) = IN_C1(ccd)
	OUT_C2(ccd) = IN_C2(ccd)
	OUT_L1(ccd) = IN_L1(ccd)
	OUT_L2(ccd) = IN_L2(ccd)

	# Set ARCON style sections.
	call set_arcon (ccd)

	call sfree (sp)
end


# SET_ARCON -- Set the data section, ccd section, trim section and
# bias section.

procedure set_arcon (ccd)

pointer	ccd			# CCD structure (returned)

pointer	sp, amplist, amp, key, str
int	i, ip, nc, nl, c1, c2, cs, l1, l2, ls, ctowrd()
int	xt1, xt2, yt1, yt2
bool	trim, clgetb()

begin
	call smark (sp)
	call salloc (amplist, SZ_LINE, TY_CHAR)
	call salloc (amp, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	trim = clgetb ("trim")

	# Get AMPLIST and determine the number of amplifiers.
	# If there is no AMPLIST or missing BSEC keywords return.
	call hdmgstr (IN_IM(ccd), "amplist", Memc[amplist], SZ_LINE)
	if (Memc[amplist] == EOS) {
	    call sfree (sp)
	    return
	}

	ip = 1
	for (i=0; ctowrd(Memc[amplist],ip,Memc[amp],SZ_LINE)!=0; i=i+1) {
	    call sprintf (Memc[key], SZ_LINE, "bsec%s")
		call pargstr (Memc[amp])
	    call hdmgstr (IN_IM(ccd), Memc[key], Memc[str], SZ_LINE)
	    if (Memc[str] == EOS) {
		call sfree (sp)
		return
	    }
	}
	if (i == 0) {
	    call sfree (sp)
	    return
	}

	IN_NSEC(ccd) = i
	call malloc (IN_SEC(ccd), 4*i, TY_INT)
	call malloc (OUT_SEC(ccd), 4*i, TY_INT)
	call malloc (BIAS_SEC(ccd), 4*i, TY_INT)

	nc = IM_LEN(IN_IM(ccd),1)
	nl = IM_LEN(IN_IM(ccd),2)

	ip = 1
	for (i=1; ctowrd(Memc[amplist],ip,Memc[amp],SZ_LINE)!=0; i=i+1) {

	    # Use amp section if no trim and data section if trim.
	    c1 = 1; c2 = nc; cs = 1; l1 = 1; l2 = nl; ls = 1
	    if (trim)
		call sprintf (Memc[key], SZ_LINE, "dsec%s")
	    else
		call sprintf (Memc[key], SZ_LINE, "asec%s")
		call pargstr (Memc[amp])
	    call hdmgstr (IN_IM(ccd), Memc[key], Memc[str], SZ_LINE)
	    if (Memc[str] == EOS) {
		call sprintf (Memc[str], SZ_LINE, "Keyword %s not found")
		    call pargstr (Memc[key])
		call error (0, Memc[str])
	    }
	    call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	    if ((c1<1)||(c2>nc)||(l1<1)||(l2>nl)) {
		call sprintf (Memc[str], SZ_LINE, "Error in %s parameter")
		    call pargstr (Memc[key])
		call error (0, Memc[str])
	    }
	    IN_SC1(ccd,i) = c1
	    IN_SC2(ccd,i) = c2
	    IN_SL1(ccd,i) = l1
	    IN_SL2(ccd,i) = l2

	    # If trimming match dsec with csec and then use tsec.
	    if (trim) {
		c1 = IN_SC1(ccd,i); c2 = IN_SC2(ccd,i); cs = 1
		l1 = IN_SL1(ccd,i); l2 = IN_SL2(ccd,i); ls = 1
		call sprintf (Memc[key], SZ_LINE, "tsec%s")
		    call pargstr (Memc[amp])
		call hdmgstr (IN_IM(ccd), Memc[key], Memc[str], SZ_LINE)
		if (Memc[str] != EOS)
		    call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
		if ((c1<IN_SC1(ccd,i))||(c2>IN_SC2(ccd,i))||
		    (l1<IN_SL1(ccd,i))||(l2>IN_SL2(ccd,i))) {
		    call sprintf (Memc[str], SZ_LINE, "Error in %s parameter")
			call pargstr (Memc[key])
		    call error (0, Memc[str])
		}
		xt1 = max (0, c1 - IN_SC1(ccd,i))
		xt2 = min (0, c2 - IN_SC2(ccd,i))
		yt1 = max (0, l1 - IN_SL1(ccd,i))
		yt2 = min (0, l2 - IN_SL2(ccd,i))

		call sprintf (Memc[key], SZ_LINE, "csec%s")
		    call pargstr (Memc[amp])
		call hdmgstr (IN_IM(ccd), Memc[key], Memc[str], SZ_LINE)
		if (Memc[str] == EOS) {
		    call sprintf (Memc[str], SZ_LINE, "Keyword %s not found")
			call pargstr (Memc[key])
		    call error (0, Memc[str])
		}
		call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
		if ((c2-c1) != (IN_SC2(ccd,i)-IN_SC1(ccd,i)) ||
		    (l2-l1) != (IN_SL2(ccd,i)-IN_SL1(ccd,i)))
		    call error (1, "DSEC and CSEC are different sizes")

		IN_SC1(ccd,i) = IN_SC1(ccd,i) + xt1
		IN_SC2(ccd,i) = IN_SC2(ccd,i) + xt2
		IN_SL1(ccd,i) = IN_SL1(ccd,i) + yt1
		IN_SL2(ccd,i) = IN_SL2(ccd,i) + yt2
		OUT_SC1(ccd,i) = c1 + xt1
		OUT_SC2(ccd,i) = c2 + xt2
		OUT_SL1(ccd,i) = l1 + yt1
		OUT_SL2(ccd,i) = l2 + yt2

	    } else {
		OUT_SC1(ccd,i) = c1
		OUT_SC2(ccd,i) = c2
		OUT_SL1(ccd,i) = l1
		OUT_SL2(ccd,i) = l2
	    }

	    # The default bias section is the whole image.
	    # Defer limit checking until actually used.
	    c1 = 1
	    c2 = nc
	    l1 = 1
	    l2 = nl
	    call sprintf (Memc[key], SZ_LINE, "bsec%s")
		call pargstr (Memc[amp])
	    call hdmgstr (IN_IM(ccd), Memc[key], Memc[str], SZ_LINE)
	    if (Memc[str] == EOS) {
		call sprintf (Memc[str], SZ_LINE, "Keyword %s not found")
		    call pargstr (Memc[key])
		call error (0, Memc[str])
	    }
	    call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	    if ((cs!=1)||(ls!=1))
		call error (0, "Error in BSEC parameter")
	    BIAS_SC1(ccd,i) = c1
	    BIAS_SC2(ccd,i) = c2
	    BIAS_SL1(ccd,i) = l1
	    BIAS_SL2(ccd,i) = l2

	    if (trim) {
		#iferr (call hdmdelf (OUT_IM(ccd), "amplist"))
		#    ;
		#call sprintf (Memc[key], SZ_LINE, "asec%s")
		#    call pargstr (Memc[amp])
		#iferr (call hdmdelf (OUT_IM(ccd), Memc[key]))
		#    ;
		call sprintf (Memc[key], SZ_LINE, "bsec%s")
		    call pargstr (Memc[amp])
		iferr (call hdmdelf (OUT_IM(ccd), Memc[key]))
		    ;
		call sprintf (Memc[key], SZ_LINE, "csec%s")
		    call pargstr (Memc[amp])
		iferr (call hdmdelf (OUT_IM(ccd), Memc[key]))
		    ;
		call sprintf (Memc[key], SZ_LINE, "dsec%s")
		    call pargstr (Memc[amp])
		iferr (call hdmdelf (OUT_IM(ccd), Memc[key]))
		    ;
		call sprintf (Memc[key], SZ_LINE, "tsec%s")
		    call pargstr (Memc[amp])
		iferr (call hdmdelf (OUT_IM(ccd), Memc[key]))
		    ;
	    }
	}

	# Set global sections.
	IN_C1(ccd) = IN_SC1(ccd,1)
	IN_C2(ccd) = IN_SC2(ccd,1)
	IN_L1(ccd) = IN_SL1(ccd,1)
	IN_L2(ccd) = IN_SL2(ccd,1)
	CCD_C1(ccd) = OUT_SC1(ccd,1)
	CCD_C2(ccd) = OUT_SC2(ccd,1)
	CCD_L1(ccd) = OUT_SL1(ccd,1)
	CCD_L2(ccd) = OUT_SL2(ccd,1)
	do i = 2, IN_NSEC(ccd) {
	    IN_C1(ccd) = min (IN_SC1(ccd,i), IN_C1(ccd))
	    IN_C2(ccd) = max (IN_SC2(ccd,i), IN_C2(ccd))
	    IN_L1(ccd) = min (IN_SL1(ccd,i), IN_L1(ccd))
	    IN_L2(ccd) = max (IN_SL2(ccd,i), IN_L2(ccd))
	    CCD_C1(ccd) = min (OUT_SC1(ccd,i), CCD_C1(ccd))
	    CCD_C2(ccd) = max (OUT_SC2(ccd,i), CCD_C2(ccd))
	    CCD_L1(ccd) = min (OUT_SL1(ccd,i), CCD_L1(ccd))
	    CCD_L2(ccd) = max (OUT_SL2(ccd,i), CCD_L2(ccd))
	}
	if (trim) {
	    OUT_C1(ccd) = CCD_C1(ccd) - CCD_C1(ccd) + 1
	    OUT_C2(ccd) = CCD_C2(ccd) - CCD_C1(ccd) + 1
	    OUT_L1(ccd) = CCD_L1(ccd) - CCD_L1(ccd) + 1
	    OUT_L2(ccd) = CCD_L2(ccd) - CCD_L1(ccd) + 1
	    ip = 1
	    for (i=1; ctowrd(Memc[amplist],ip,Memc[amp],SZ_LINE)!=0; i=i+1) {
		OUT_SC1(ccd,i) = OUT_SC1(ccd,i) - CCD_C1(ccd) + 1
		OUT_SC2(ccd,i) = OUT_SC2(ccd,i) - CCD_C1(ccd) + 1
		OUT_SL1(ccd,i) = OUT_SL1(ccd,i) - CCD_L1(ccd) + 1
		OUT_SL2(ccd,i) = OUT_SL2(ccd,i) - CCD_L1(ccd) + 1
		call sprintf (Memc[key], SZ_LINE, "asec%s")
		    call pargstr (Memc[amp])
		call sprintf (Memc[str], SZ_LINE, "[%d:%d,%d:%d]")
		    call pargi (OUT_SC1(ccd,i))
		    call pargi (OUT_SC2(ccd,i))
		    call pargi (OUT_SL1(ccd,i))
		    call pargi (OUT_SL2(ccd,i))
		call hdmpstr (OUT_IM(ccd), Memc[key], Memc[str])
	    }
	    IM_LEN(OUT_IM(ccd),1) = OUT_C2(ccd)
	    IM_LEN(OUT_IM(ccd),2) = OUT_L2(ccd)
	} else {
	    OUT_C1(ccd) = IN_C1(ccd)
	    OUT_C2(ccd) = IN_C2(ccd)
	    OUT_L1(ccd) = IN_L1(ccd)
	    OUT_L2(ccd) = IN_L2(ccd)
	}

	call sfree (sp)
end
