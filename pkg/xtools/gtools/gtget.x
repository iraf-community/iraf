# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"gtools.h"

# GT_GETI -- Set integer parameters.

int procedure gt_geti (gt, param)

pointer	gt			# GTOOLS pointer
int	param			# Parameter to set

begin
	switch (param) {
	case GTLINE:
	    return (GT_LINE(gt))
	case GTTRANSPOSE:
	    return (GT_TRANSPOSE(gt))
	}
end


# GT_GETR -- Set real parameters.

real procedure gt_getr (gt, param)

pointer	gt			# GTOOLS pointer
int	param			# Parameter to set

begin
	switch (param) {
	case GTXMIN:
	    return (GT_XMIN(gt))
	case GTXMAX:
	    return (GT_XMAX(gt))
	case GTYMIN:
	    return (GT_YMIN(gt))
	case GTYMAX:
	    return (GT_YMAX(gt))
	case GTXSIZE:
	    return (GT_XSIZE(gt))
	case GTYSIZE:
	    return (GT_YSIZE(gt))
	}
end


# GT_GETS -- Get string parameters.

procedure gt_gets (gt, param, str, sz_str)

pointer	gt			# GTOOLS pointer
int	param			# Parameter to set
char	str[sz_str]		# String
int	sz_str			# Size of string

begin
	str[1] = EOS
	switch (param) {
	case GTPARAMS:
	    if (GT_PARAMS(gt) != NULL)
	        call strcpy (Memc[GT_PARAMS(gt)], str, sz_str)
	case GTTITLE:
	    if (GT_TITLE(gt) != NULL)
	        call strcpy (Memc[GT_TITLE(gt)], str, sz_str)
	case GTSUBTITLE:
	    if (GT_SUBTITLE(gt) != NULL)
	        call strcpy (Memc[GT_SUBTITLE(gt)], str, sz_str)
	case GTCOMMENTS:
	    if (GT_COMMENTS(gt) != NULL)
	        call strcpy (Memc[GT_COMMENTS(gt)], str, sz_str)
	case GTXLABEL:
	    if (GT_XLABEL(gt) != NULL)
	        call strcpy (Memc[GT_XLABEL(gt)], str, sz_str)
	case GTYLABEL:
	    if (GT_YLABEL(gt) != NULL)
	        call strcpy (Memc[GT_YLABEL(gt)], str, sz_str)
	case GTXUNITS:
	    if (GT_XUNITS(gt) != NULL)
	        call strcpy (Memc[GT_XUNITS(gt)], str, sz_str)
	case GTYUNITS:
	    if (GT_YUNITS(gt) != NULL)
	        call strcpy (Memc[GT_YUNITS(gt)], str, sz_str)
	case GTXTRAN:
	    switch (GT_XTRAN(gt)) {
	    case 1:
	        call strcpy ("linear", str, sz_str)
	    case 2:
	        call strcpy ("logrithmic", str, sz_str)
	    }
	case GTYTRAN:
	    switch (GT_YTRAN(gt)) {
	    case 1:
	        call strcpy ("linear", str, sz_str)
	    case 2:
	        call strcpy ("logrithmic", str, sz_str)
	    }
	case GTTYPE:
	    switch (GT_TYPE(gt)) {
	    case 1:
	        call strcpy ("mark", str, sz_str)
	    case 2:
	        call strcpy ("line", str, sz_str)
	    }
	case GTMARK:
	    switch (GT_MARK(gt)) {
	    case GM_POINT:
	        call strcpy ("point", str, sz_str)
	    case GM_BOX:
	        call strcpy ("box", str, sz_str)
	    case GM_PLUS:
	        call strcpy ("plus", str, sz_str)
	    case GM_CROSS:
	        call strcpy ("cross", str, sz_str)
	    case GM_DIAMOND:
	        call strcpy ("diamond", str, sz_str)
	    case GM_HLINE:
	        call strcpy ("hline", str, sz_str)
	    case GM_VLINE:
	        call strcpy ("vline", str, sz_str)
	    case GM_HEBAR:
	        call strcpy ("hebar", str, sz_str)
	    case GM_VEBAR:
	        call strcpy ("vebar", str, sz_str)
	    case GM_CIRCLE:
	        call strcpy ("circle", str, sz_str)
	    }
	}
end
