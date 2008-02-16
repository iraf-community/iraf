include	<pkg/inlfit.h>


# ING_GETLABEL -- Get label and units for a given axis

procedure ing_getlabel (in, xtype, xnum, label, units, maxch)

pointer	in			# INLFIT descriptor
int	xtype			# axis type
int	xnum			# axis number
char	label[ARB]		# label
char	units[ARB]		# units
int	maxch			# max chars. in label and units

int	dummy
pointer	sp, str
pointer	labels, lunits, vlabels, vunits
pointer	userlabels, userunits

int	inlstrwrd()

begin
	# Begin allocation of string space.
	call smark (sp)
	call salloc (str, SZ_LINE + 1, TY_CHAR)

	# Branch on axis type.
	switch (xtype) {
	case KEY_VARIABLE:
	    call salloc (labels,  SZ_LINE, TY_CHAR)
	    call salloc (vlabels, SZ_LINE, TY_CHAR)
	    call salloc (vunits,  SZ_LINE, TY_CHAR)
	    call in_gstr (in, INLLABELS,  Memc[labels],  SZ_LINE)
	    call in_gstr (in, INLVLABELS, Memc[vlabels], SZ_LINE)
	    call in_gstr (in, INLVUNITS,  Memc[vunits],  SZ_LINE)

	    if (inlstrwrd (xnum, label, maxch, Memc[vlabels]) == 0) {
		if (inlstrwrd (xtype, Memc[str], SZ_LINE, Memc[labels]) != 0) {
		    call sprintf (label, maxch, "%s%d")
		        call pargstr (Memc[str])
		        call pargi (xnum)
		}
	    }
	    dummy = inlstrwrd (xnum, units, maxch, Memc[vunits])

	case KEY_FUNCTION, KEY_FIT, KEY_RESIDUALS, KEY_RATIO, KEY_NONLINEAR:
	    call salloc (labels, SZ_LINE, TY_CHAR)
	    call salloc (lunits, SZ_LINE, TY_CHAR)
	    call in_gstr (in, INLLABELS, Memc[labels], SZ_LINE)
	    call in_gstr (in, INLUNITS,  Memc[lunits], SZ_LINE)

	    dummy = inlstrwrd (xtype, label, maxch, Memc[labels])
	    dummy = inlstrwrd (xtype, units, maxch, Memc[lunits])

	case KEY_UAXIS:
	    call salloc (labels,     SZ_LINE, TY_CHAR)
	    call salloc (userlabels, SZ_LINE, TY_CHAR)
	    call salloc (userunits,  SZ_LINE, TY_CHAR)
	    call in_gstr (in, INLLABELS, Memc[labels], SZ_LINE)
	    call in_gstr (in, INLUSERLABELS, Memc[userlabels], SZ_LINE)
	    call in_gstr (in, INLUSERUNITS,  Memc[userunits],  SZ_LINE)

	    if (inlstrwrd (xnum, label, maxch, Memc[userlabels]) == 0) {
		if (inlstrwrd (xtype, Memc[str], SZ_LINE, Memc[labels]) != 0) {
		    call sprintf (label, maxch, "%s%d")
		        call pargstr (Memc[str])
		        call pargi (xnum)
		}
	    }
	    dummy = inlstrwrd (xnum, units, maxch, Memc[userunits])

	default:
	    call error (0, "INLFIT, ing_getlabel: Unknown axis type")
	}

	# Free memory.
	call sfree (sp)
end
