# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"

# GT_SETI -- Set integer parameters.

procedure gt_seti (gt, param, ival)

pointer	gt			# GTOOLS pointer
int	param			# Parameter to set
int	ival			# Integer value to set

begin
	if (gt == NULL)
	    return

	switch (param) {
	case GTLINE:
	    GT_LINE(gt) = ival
	case GTTRANSPOSE:
	    GT_TRANSPOSE(gt) = ival
	case GTSYSID:
	    GT_SYSID(gt) = ival
	}
end


# GT_SETR -- Set real parameters.

procedure gt_setr (gt, param, rval)

pointer	gt			# GTOOLS pointer
int	param			# Parameter to set
real	rval			# Real value to set

begin
	if (gt == NULL)
	    return

	switch (param) {
	case GTXMIN:
	    GT_XMIN(gt) = rval
	case GTXMAX:
	    GT_XMAX(gt) = rval
	case GTYMIN:
	    GT_YMIN(gt) = rval
	case GTYMAX:
	    GT_YMAX(gt) = rval
	case GTXSIZE:
	    GT_XSIZE(gt) = rval
	case GTYSIZE:
	    GT_YSIZE(gt) = rval
	}
end


# GT_SETS -- Set string parameters.

procedure gt_sets (gt, param, str)

pointer	gt			# GTOOLS pointer
int	param			# Parameter to set
char	str[ARB]		# String

char	dummy[10]
int	len

int	marks[10]
data	marks /GM_POINT,GM_BOX,GM_PLUS,GM_CROSS,GM_DIAMOND,GM_HLINE,GM_VLINE,
		GM_HEBAR,GM_VEBAR,GM_CIRCLE/
int	trans[2]
data	trans /GW_LINEAR, GW_ELOG/

int	strlen(), strdic()

begin
	if (gt == NULL)
	    return

	len = strlen (str)
	switch (param) {
	case GTPARAMS:
	    call mfree (GT_PARAMS(gt), TY_CHAR)
	    if (len > 0) {
	        call malloc (GT_PARAMS(gt), len, TY_CHAR)
	        call strcpy (str, Memc[GT_PARAMS(gt)], len)
	    }
	case GTTITLE:
	    call mfree (GT_TITLE(gt), TY_CHAR)
	    if (len > 0) {
	        call malloc (GT_TITLE(gt), len, TY_CHAR)
	        call strcpy (str, Memc[GT_TITLE(gt)], len)
	    }
	case GTSUBTITLE:
	    call mfree (GT_SUBTITLE(gt), TY_CHAR)
	    if (len > 0) {
	        call malloc (GT_SUBTITLE(gt), len, TY_CHAR)
	        call strcpy (str, Memc[GT_SUBTITLE(gt)], len)
	    }
	case GTCOMMENTS:
	    call mfree (GT_COMMENTS(gt), TY_CHAR)
	    if (len > 0) {
	        call malloc (GT_COMMENTS(gt), len, TY_CHAR)
	        call strcpy (str, Memc[GT_COMMENTS(gt)], len)
	    }
	case GTXLABEL:
	    call mfree (GT_XLABEL(gt), TY_CHAR)
	    if (len > 0) {
	        call malloc (GT_XLABEL(gt), len, TY_CHAR)
	        call strcpy (str, Memc[GT_XLABEL(gt)], len)
	    }
	case GTYLABEL:
	    call mfree (GT_YLABEL(gt), TY_CHAR)
	    if (len > 0) {
	        call malloc (GT_YLABEL(gt), len, TY_CHAR)
	        call strcpy (str, Memc[GT_YLABEL(gt)], len)
	    }
	case GTXUNITS:
	    call mfree (GT_XUNITS(gt), TY_CHAR)
	    if (len > 0) {
	        call malloc (GT_XUNITS(gt), len, TY_CHAR)
	        call strcpy (str, Memc[GT_XUNITS(gt)], len)
	    }
	case GTYUNITS:
	    call mfree (GT_YUNITS(gt), TY_CHAR)
	    if (len > 0) {
	        call malloc (GT_YUNITS(gt), len, TY_CHAR)
	        call strcpy (str, Memc[GT_YUNITS(gt)], len)
	    }
	case GTXTRAN:
	    len = strdic (str, dummy, 10, "|linear|logrithmic|")
	    if (len == 0) {
		call eprintf ("Unknown X transformation type `%s'\n")
		    call pargstr (str)
	    } else
	        GT_XTRAN(gt) = trans[len]
	case GTYTRAN:
	    len = strdic (str, dummy, 10, "|linear|logrithmic|")
	    if (len == 0) {
		call eprintf ("Unknown Y transformation type `%s'\n")
		    call pargstr (str)
	    } else
	        GT_YTRAN(gt) = trans[len]
	case GTTYPE:
	    len = strdic (str, dummy, 10, GTTYPES)
	    if (len == 0) {
		call eprintf ("Unknown graph type `%s'\n")
		    call pargstr (str)
	    } else
	        GT_TYPE(gt) = len
	case GTMARK:
	    len = strdic (str, dummy, 10, GTMARKS)
	    if (len == 0) {
		call eprintf ("Unknown mark type `%s'\n")
		    call pargstr (str)
	    } else
	        GT_MARK(gt) = marks[len]
	}
end
