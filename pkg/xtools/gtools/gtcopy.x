# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"gtools.h"

# GT_COPY -- Copy values of one structure to another.

procedure gt_copy (gt1, gt2)

pointer	gt1, gt2

int	len, strlen()
pointer	gt_init()

begin
	if (gt1 == NULL)
	    call error (0, "gt_copy: Undefined gtools structure")

	if (gt2 == NULL)
	    gt2 = gt_init ()

	GT_XMIN(gt2) = GT_XMIN(gt1)
	GT_XMAX(gt2) = GT_XMAX(gt1)
	GT_YMIN(gt2) = GT_YMIN(gt1)
	GT_YMAX(gt2) = GT_YMAX(gt1)
	GT_XTRAN(gt2) = GT_XTRAN(gt1)
	GT_YTRAN(gt2) = GT_YTRAN(gt1)
	GT_TYPE(gt2) = GT_TYPE(gt1)
	GT_MARK(gt2) = GT_MARK(gt1)
	GT_LINE(gt2) = GT_LINE(gt1)
	GT_TRANSPOSE(gt2) = GT_TRANSPOSE(gt1)

	call mfree (GT_PARAMS(gt2), TY_CHAR)
	if (GT_PARAMS(gt1) != NULL) {
	    len = strlen (Memc[GT_PARAMS(gt1)])
	    call malloc (GT_PARAMS(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_PARAMS(gt1)], Memc[GT_PARAMS(gt2)], len)
	}
	call mfree (GT_TITLE(gt2), TY_CHAR)
	if (GT_TITLE(gt1) != NULL) {
	    len = strlen (Memc[GT_TITLE(gt1)])
	    call malloc (GT_TITLE(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_TITLE(gt1)], Memc[GT_TITLE(gt2)], len)
	}
	call mfree (GT_SUBTITLE(gt2), TY_CHAR)
	if (GT_SUBTITLE(gt1) != NULL) {
	    len = strlen (Memc[GT_SUBTITLE(gt1)])
	    call malloc (GT_SUBTITLE(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_SUBTITLE(gt1)], Memc[GT_SUBTITLE(gt2)], len)
	}
	call mfree (GT_COMMENTS(gt2), TY_CHAR)
	if (GT_COMMENTS(gt1) != NULL) {
	    len = strlen (Memc[GT_COMMENTS(gt1)])
	    call malloc (GT_COMMENTS(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_COMMENTS(gt1)], Memc[GT_COMMENTS(gt2)], len)
	}
	call mfree (GT_XLABEL(gt2), TY_CHAR)
	if (GT_XLABEL(gt1) != NULL) {
	    len = strlen (Memc[GT_XLABEL(gt1)])
	    call malloc (GT_XLABEL(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_XLABEL(gt1)], Memc[GT_XLABEL(gt2)], len)
	}
	call mfree (GT_YLABEL(gt2), TY_CHAR)
	if (GT_YLABEL(gt1) != NULL) {
	    len = strlen (Memc[GT_YLABEL(gt1)])
	    call malloc (GT_YLABEL(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_YLABEL(gt1)], Memc[GT_YLABEL(gt2)], len)
	}
	call mfree (GT_XUNITS(gt2), TY_CHAR)
	if (GT_XUNITS(gt1) != NULL) {
	    len = strlen (Memc[GT_XUNITS(gt1)])
	    call malloc (GT_XUNITS(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_XUNITS(gt1)], Memc[GT_XUNITS(gt2)], len)
	}
	call mfree (GT_YUNITS(gt2), TY_CHAR)
	if (GT_YUNITS(gt1) != NULL) {
	    len = strlen (Memc[GT_YUNITS(gt1)])
	    call malloc (GT_YUNITS(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_YUNITS(gt1)], Memc[GT_YUNITS(gt2)], len)
	}
end
