include	<pkg/inlfit.h>


# ING_GETLABEL -- Get label and units for a given axis

procedure ing_getlabel (in, xtype, xnum, label, units, maxch)

pointer	in			# INLFIT descriptor
int	xtype			# axis type
int	xnum			# axis number
char	label[ARB]		# label
char	units[ARB]		# units
int	maxch			# max chars. in label and units

size_t	sz_val
int	dummy
long	l_val
pointer	sp, str
pointer	labels, lunits, vlabels, vunits
pointer	userlabels, userunits

long	inlstrwrd()

begin
	# Begin allocation of string space.
	call smark (sp)
	sz_val = SZ_LINE + 1
	call salloc (str, sz_val, TY_CHAR)

	# Branch on axis type.
	switch (xtype) {
	case KEY_VARIABLE:
	    sz_val = SZ_LINE
	    call salloc (labels, sz_val, TY_CHAR)
	    call salloc (vlabels, sz_val, TY_CHAR)
	    call salloc (vunits, sz_val, TY_CHAR)
	    call in_gstr (in, INLLABELS,  Memc[labels],  SZ_LINE)
	    call in_gstr (in, INLVLABELS, Memc[vlabels], SZ_LINE)
	    call in_gstr (in, INLVUNITS,  Memc[vunits],  SZ_LINE)

	    l_val = xnum
	    if (inlstrwrd (l_val, label, maxch, Memc[vlabels]) == 0) {
		l_val = xtype
		if (inlstrwrd (l_val, Memc[str], SZ_LINE, Memc[labels]) != 0) {
		    call sprintf (label, maxch, "%s%d")
		        call pargstr (Memc[str])
		        call pargi (xnum)
		}
	    }
	    l_val = xnum
	    dummy = inlstrwrd (l_val, units, maxch, Memc[vunits])

	case KEY_FUNCTION, KEY_FIT, KEY_RESIDUALS, KEY_RATIO, KEY_NONLINEAR:
	    sz_val = SZ_LINE
	    call salloc (labels, sz_val, TY_CHAR)
	    call salloc (lunits, sz_val, TY_CHAR)
	    call in_gstr (in, INLLABELS, Memc[labels], SZ_LINE)
	    call in_gstr (in, INLUNITS,  Memc[lunits], SZ_LINE)

	    l_val = xtype
	    dummy = inlstrwrd (l_val, label, maxch, Memc[labels])
	    dummy = inlstrwrd (l_val, units, maxch, Memc[lunits])

	case KEY_UAXIS:
	    sz_val = SZ_LINE
	    call salloc (labels, sz_val, TY_CHAR)
	    call salloc (userlabels, sz_val, TY_CHAR)
	    call salloc (userunits, sz_val, TY_CHAR)
	    call in_gstr (in, INLLABELS, Memc[labels], SZ_LINE)
	    call in_gstr (in, INLUSERLABELS, Memc[userlabels], SZ_LINE)
	    call in_gstr (in, INLUSERUNITS,  Memc[userunits],  SZ_LINE)

	    l_val = xnum
	    if (inlstrwrd (l_val, label, maxch, Memc[userlabels]) == 0) {
		l_val = xtype
		if (inlstrwrd (l_val, Memc[str], SZ_LINE, Memc[labels]) != 0) {
		    call sprintf (label, maxch, "%s%d")
		        call pargstr (Memc[str])
		        call pargi (xnum)
		}
	    }
	    l_val = xnum
	    dummy = inlstrwrd (l_val, units, maxch, Memc[userunits])

	default:
	    call error (0, "INLFIT, ing_getlabel: Unknown axis type")
	}

	# Free memory.
	call sfree (sp)
end
