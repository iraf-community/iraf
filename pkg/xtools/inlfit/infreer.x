include "inlfitdef.h"


# IN_FREE -- Free INLFIT parameter structure, substructures, and auxiliary
# buffers.

procedure in_freer (in)

pointer	in		# INLFIT pointer

begin

#	# Debug.
#	call eprintf ("in_free: in=%d\n")
#	    call pargi (in)

	# Free only if it's not NULL.
	if (in != NULL) {

	    # Free parameter values, changes, and list.
	    call mfree (IN_PARAM  (in), TY_REAL)
	    call mfree (IN_DPARAM (in), TY_REAL)
	    call mfree (IN_PLIST  (in), TY_INT)

	    # Free string space.
	    call mfree (IN_LABELS     (in), TY_CHAR)
	    call mfree (IN_UNITS      (in), TY_CHAR)
	    call mfree (IN_PLABELS    (in), TY_CHAR)
	    call mfree (IN_PUNITS     (in), TY_CHAR)
	    call mfree (IN_VLABELS    (in), TY_CHAR)
	    call mfree (IN_VUNITS     (in), TY_CHAR)
	    call mfree (IN_USERLABELS (in), TY_CHAR)
	    call mfree (IN_USERUNITS  (in), TY_CHAR)
	    call mfree (IN_HELP       (in), TY_CHAR)
	    call mfree (IN_PROMPT     (in), TY_CHAR)

	    # Free rejected point list, and limit values for variables.
	    if (IN_REJPTS (in) != NULL)
	        call mfree (IN_REJPTS (in), TY_INT)
	    if (IN_XMIN (in) != NULL)
	        call mfree (IN_XMIN (in), TY_REAL)
	    if (IN_XMAX (in) != NULL)
	        call mfree (IN_XMAX (in), TY_REAL)

	    # Free substructures.
	    call mfree (IN_SFLOAT (in), TY_REAL)
	    call mfree (IN_SGAXES (in), TY_INT)

	    # Free structure.
	    call mfree (in, TY_STRUCT)
	}
end
