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
	else {
	    call mfree (GT_PARAMS(gt2), TY_CHAR)
	    call mfree (GT_TITLE(gt2), TY_CHAR)
	    call mfree (GT_SUBTITLE(gt2), TY_CHAR)
	    call mfree (GT_COMMENTS(gt2), TY_CHAR)
	    call mfree (GT_XLABEL(gt2), TY_CHAR)
	    call mfree (GT_YLABEL(gt2), TY_CHAR)
	    call mfree (GT_XUNITS(gt2), TY_CHAR)
	    call mfree (GT_YUNITS(gt2), TY_CHAR)
	    call mfree (GT_XFORMAT(gt2), TY_CHAR)
	    call mfree (GT_YFORMAT(gt2), TY_CHAR)
	}

	call amovi (Memi[gt1], Memi[gt2], LEN_GT)

	if (GT_PARAMS(gt1) != NULL) {
	    len = strlen (Memc[GT_PARAMS(gt1)])
	    call malloc (GT_PARAMS(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_PARAMS(gt1)], Memc[GT_PARAMS(gt2)], len)
	}
	if (GT_TITLE(gt1) != NULL) {
	    len = strlen (Memc[GT_TITLE(gt1)])
	    call malloc (GT_TITLE(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_TITLE(gt1)], Memc[GT_TITLE(gt2)], len)
	}
	if (GT_SUBTITLE(gt1) != NULL) {
	    len = strlen (Memc[GT_SUBTITLE(gt1)])
	    call malloc (GT_SUBTITLE(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_SUBTITLE(gt1)], Memc[GT_SUBTITLE(gt2)], len)
	}
	if (GT_COMMENTS(gt1) != NULL) {
	    len = strlen (Memc[GT_COMMENTS(gt1)])
	    call malloc (GT_COMMENTS(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_COMMENTS(gt1)], Memc[GT_COMMENTS(gt2)], len)
	}
	if (GT_XLABEL(gt1) != NULL) {
	    len = strlen (Memc[GT_XLABEL(gt1)])
	    call malloc (GT_XLABEL(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_XLABEL(gt1)], Memc[GT_XLABEL(gt2)], len)
	}
	if (GT_YLABEL(gt1) != NULL) {
	    len = strlen (Memc[GT_YLABEL(gt1)])
	    call malloc (GT_YLABEL(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_YLABEL(gt1)], Memc[GT_YLABEL(gt2)], len)
	}
	if (GT_XUNITS(gt1) != NULL) {
	    len = strlen (Memc[GT_XUNITS(gt1)])
	    call malloc (GT_XUNITS(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_XUNITS(gt1)], Memc[GT_XUNITS(gt2)], len)
	}
	if (GT_YUNITS(gt1) != NULL) {
	    len = strlen (Memc[GT_YUNITS(gt1)])
	    call malloc (GT_YUNITS(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_YUNITS(gt1)], Memc[GT_YUNITS(gt2)], len)
	}
	if (GT_XFORMAT(gt1) != NULL) {
	    len = strlen (Memc[GT_XFORMAT(gt1)])
	    call malloc (GT_XFORMAT(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_XFORMAT(gt1)], Memc[GT_XFORMAT(gt2)], len)
	}
	if (GT_YFORMAT(gt1) != NULL) {
	    len = strlen (Memc[GT_YFORMAT(gt1)])
	    call malloc (GT_YFORMAT(gt2), len, TY_CHAR)
	    call strcpy (Memc[GT_YFORMAT(gt1)], Memc[GT_YFORMAT(gt2)], len)
	}
end
