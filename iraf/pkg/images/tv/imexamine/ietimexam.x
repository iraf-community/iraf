# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	"imexam.h"


# IE_TIMEXAM -- Extract a subraster image.
# This routine does not currently update the WCS but it does clear it.

procedure ie_timexam (ie, x, y)
 
pointer	ie			# IE pointer
real	x, y			# Center

int	i, x1, x2, y1, y2, nx, ny
pointer	sp, root, extn, output
pointer	im, out, data, outbuf, mw

int	clgeti(), fnextn(), iki_validextn(), strlen(), imaccess()
pointer	ie_gimage(), ie_gdata(), immap(), impl2r(), mw_open()
errchk	impl2r

begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	# Get parameters.
	call clgstr ("output", Memc[root], SZ_FNAME)
	nx = clgeti ("ncoutput")
	ny = clgeti ("nloutput")

	# Strip the extension.
	call imgimage (Memc[root], Memc[root], SZ_FNAME)
	if (Memc[root] == EOS)
	    call strcpy (IE_IMAGE(ie), Memc[root], SZ_FNAME)
	i = fnextn (Memc[root], Memc[extn+1], SZ_FNAME)
	Memc[extn] = EOS
	if (i > 0) {
	    call iki_init()
	    if (iki_validextn (0, Memc[extn+1]) != 0) {
		Memc[root+strlen(Memc[root])-i-1] = EOS
		Memc[extn] = '.'
	    }
	}

	do i = 1, ARB {
	    call sprintf (Memc[output], SZ_FNAME, "%s.%03d%s")
		call pargstr (Memc[root])
		call pargi (i)
		call pargstr (Memc[extn])
	    if (imaccess (Memc[output], 0) == NO)
		break
	}

	# Set section to be extracted.
	if (!IS_INDEF(x))
	    IE_X1(ie) = x
	if (!IS_INDEF(y))
	    IE_Y1(ie) = y

	x1 = IE_X1(ie) - (nx - 1) / 2 + 0.5
	x2 = IE_X1(ie) + nx / 2 + 0.5
	y1 = IE_Y1(ie) - (ny - 1) / 2 + 0.5
	y2 = IE_Y1(ie) + ny / 2 + 0.5
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1

	# Set output.
	iferr (out = immap (Memc[output], NEW_COPY, im)) {
	    call erract (EA_WARN)
	    return
	}
	IM_NDIM(out) = 2
	IM_LEN(out,1) = nx
	IM_LEN(out,2) = ny

	# Extract the section.
	iferr {
	    do i = y1, y2 {
	        data = ie_gdata (im, x1, x2, i, i)
		outbuf = impl2r (out, i-y1+1)
		call amovr (Memr[data], Memr[outbuf], nx)
	    }
	    mw = mw_open (NULL, 2)
	    call mw_saveim (mw, out)
	    call imunmap (out)
	} then {
	    call imunmap (out)
	    iferr (call imdelete (Memc[output]))
		;
	    call sfree (sp)
	    call erract (EA_WARN)
	    return
	}

	call printf ("%s[%d:%d,%d:%d] -> %s\n")
	    call pargstr (IE_IMAGE(ie))
	    call pargi (x1)
	    call pargi (x2)
	    call pargi (y1)
	    call pargi (y2)
	    call pargstr (Memc[output])
	if (IE_LOGFD(ie) != NULL) {
	    call fprintf (IE_LOGFD(ie), "%s[%d:%d,%d:%d] -> %s\n")
		call pargstr (IE_IMAGE(ie))
		call pargi (x1)
		call pargi (x2)
		call pargi (y1)
		call pargi (y2)
	}

	call sfree (sp)
end
