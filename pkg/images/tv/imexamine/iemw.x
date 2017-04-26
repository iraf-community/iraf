# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mwset.h>
include	"imexam.h"


# IE_MWINIT -- Initialize MWCS

procedure ie_mwinit (ie)

pointer	ie			# IMEXAM descriptor

int	i, j, wcsdim, mw_stati(), nowhite(), stridxs()
pointer	im, mw, ctlw, ctwl, mw_openim(), mw_sctran()
pointer	sp, axno, axval, str1, str2
bool	streq()
errchk	mw_openim, mw_sctran

begin
	im = IE_IM(ie)
	mw = IE_MW(ie)

	if (mw != NULL) {
	    call mw_close (mw)
	    IE_MW(ie) = mw
	}

	IE_XLABEL(ie) = EOS
	IE_YLABEL(ie) = EOS
	call clgstr ("xformat", IE_XFORMAT(ie), IE_SZFORMAT)
	call clgstr ("yformat", IE_YFORMAT(ie), IE_SZFORMAT)
	i = nowhite (IE_XFORMAT(ie), IE_XFORMAT(ie), IE_SZFORMAT)
	i = nowhite (IE_YFORMAT(ie), IE_YFORMAT(ie), IE_SZFORMAT)

	if (im == NULL || im == IE_DS(ie))
	    return

	call smark (sp)
	call salloc (axno, IM_MAXDIM, TY_INT)
	call salloc (axval, IM_MAXDIM, TY_INT)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	mw = mw_openim (im)
	call mw_seti (mw, MW_USEAXMAP, NO)
	wcsdim = mw_stati (mw, MW_NDIM)
	call mw_gaxmap (mw, Memi[axno], Memi[axval], wcsdim)
	IE_P1(ie) = 1
	IE_P2(ie) = 2
	do i = 1, wcsdim {
	    j = Memi[axno+i-1]
	    if (j == 0)
		IE_IN(ie,i) = 1
	    else if (j == 1)
		IE_P1(ie) = i
	    else if (j == 2)
		IE_P2(ie) = i
	}
	ctlw = mw_sctran (mw, "logical", IE_WCSNAME(ie), 0)
	ctwl = mw_sctran (mw, IE_WCSNAME(ie), "logical", 0)

	# Set coordinate labels and formats
	i = IE_P1(ie)
	j = IE_P2(ie)
	if (streq (IE_WCSNAME(ie), "logical")) {
	    call strcpy ("Column (pixels)", IE_XLABEL(ie), IE_SZFNAME)
	    call strcpy ("Line (pixels)", IE_YLABEL(ie), IE_SZFNAME)
	} else if (streq (IE_WCSNAME(ie), "physical")) {
	    if (i == 1)
		call strcpy ("Column (pixels)", IE_XLABEL(ie), IE_SZFNAME)
	    else if (i == 2)
		call strcpy ("Line (pixels)", IE_XLABEL(ie), IE_SZFNAME)
	    else
		call strcpy ("Pixels", IE_XLABEL(ie), IE_SZFNAME)
	    if (j == 1)
		call strcpy ("Column (pixels)", IE_YLABEL(ie), IE_SZFNAME)
	    else if (j == 2)
		call strcpy ("Line (pixels)", IE_YLABEL(ie), IE_SZFNAME)
	    else
		call strcpy ("Pixels", IE_YLABEL(ie), IE_SZFNAME)
	} else {
	    ifnoerr (call mw_gwattrs (mw, i, "label", Memc[str1], SZ_LINE)) {
		ifnoerr (call mw_gwattrs (mw, i, "units", Memc[str2],SZ_LINE)) {
		    call sprintf (IE_XLABEL(ie), IE_SZFNAME, "%s (%s)")
			call pargstr (Memc[str1])
			call pargstr (Memc[str2])
		} else {
		    call sprintf (IE_XLABEL(ie), IE_SZFNAME, "%s")
			call pargstr (Memc[str1])
		}
	    }
	    if (IE_XFORMAT(ie) != '%')
		ifnoerr (call mw_gwattrs (mw, i, "format", Memc[str1], SZ_LINE))
		    call strcpy (Memc[str1], IE_XFORMAT(ie), IE_SZFORMAT)

	    ifnoerr (call mw_gwattrs (mw, j, "label", Memc[str1], SZ_LINE)) {
		ifnoerr (call mw_gwattrs (mw, j, "units", Memc[str2],SZ_LINE)) {
		    call sprintf (IE_YLABEL(ie), IE_SZFNAME, "%s (%s)")
			call pargstr (Memc[str1])
			call pargstr (Memc[str2])
		} else {
		    call sprintf (IE_YLABEL(ie), IE_SZFNAME, "%s")
			call pargstr (Memc[str1])
		}
	    }
	    if (IE_YFORMAT(ie) != '%')
		ifnoerr (call mw_gwattrs (mw, j, "format", Memc[str1], SZ_LINE))
		    call strcpy (Memc[str1], IE_YFORMAT(ie), IE_SZFORMAT)

	    # Check for equitorial coordinate and reversed formats.
	    ifnoerr (call mw_gwattrs (mw, i, "axtype", Memc[str1], SZ_LINE))
		if ((streq(Memc[str1],"ra")&&stridxs("hm",IE_XFORMAT(ie))>0) ||
		    (streq(Memc[str1],"dec")&&stridxs("HM",IE_XFORMAT(ie))>0)) {
		    call strcpy (IE_XFORMAT(ie), Memc[str1], IE_SZFORMAT)
		    call strcpy (IE_YFORMAT(ie), IE_XFORMAT(ie),IE_SZFORMAT)
		    call strcpy (Memc[str1], IE_YFORMAT(ie), IE_SZFORMAT)
		}
	}

	IE_MW(ie) = mw
	IE_CTLW(ie) = ctlw
	IE_CTWL(ie) = ctwl
	IE_WCSDIM(ie) = wcsdim

	call sfree (sp)
end


# IE_MWCTRAN -- Evaluate MWCS coordinate

procedure  ie_mwctran (ie, xin, yin, xout, yout) 

pointer	ie			# IMEXAM descriptor
real	xin,  yin		# Input coordinate
real	xout, yout		# Output coordinate

begin
	if (IE_MW(ie) == NULL) {
	    xout = xin
	    yout = yin
	    return
	}

	IE_IN(ie,IE_P1(ie)) = xin
	IE_IN(ie,IE_P2(ie)) = yin
	call mw_ctranr (IE_CTLW(ie), IE_IN(ie,1), IE_OUT(ie,1), IE_WCSDIM(ie))
	xout = IE_OUT(ie,IE_P1(ie))
	yout = IE_OUT(ie,IE_P2(ie))
end


# IE_IMWCTRAN -- Evaluate inverse MWCS coordinate

procedure  ie_imwctran (ie, xin, yin, xout, yout) 

pointer	ie			# IMEXAM descriptor
real	xin,  yin		# Input coordinate
real	xout, yout		# Output coordinate

begin
	if (IE_MW(ie) == NULL) {
	    xout = xin
	    yout = yin
	    return
	}

	IE_OUT(ie,IE_P1(ie)) = xin
	IE_OUT(ie,IE_P2(ie)) = yin
	call mw_ctranr (IE_CTWL(ie), IE_OUT(ie,1), IE_IN(ie,1), IE_WCSDIM(ie))
	xout = IE_IN(ie,IE_P1(ie))
	yout = IE_IN(ie,IE_P2(ie))
end


# IE_IFORMATR -- Determine the inverse formatted real value
# This temporary routine is used to account for scaling of the H and M formats.

real procedure ie_iformatr (value, format)

real	value			# Value to be inverse formated
char	format[ARB]		# Format

int	strldxs()

begin
	if (!IS_INDEF(value) && strldxs ("HM", format) > 0)
	    return (value * 15.)
	else
	    return (value)
end
