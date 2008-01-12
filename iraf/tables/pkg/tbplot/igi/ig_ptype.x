include "igi.h"

procedure ig_ptype (igs)

#  IG_PTYPE -- the igi PTYPE command.  Set the marker style, consisting
#  of the number of vertices and type of marker.

#  2/7/91 Allow a single argument only, the number of vertices, if it's < 2
#  and Add ii_ptype().  ZGL

pointer	igs		# Parameters structure

int	in		# Input stream descriptor
pointer	tokvals		# Token value structure
pointer	igps		# Plot parameters structure

int	token
int	nvert		# Number of vertices
int	ptype		# Style
char	ptypec[SZ_LINE]

int	gettok(), ptypci()

begin
	call lcmdcat (igs, YES)
	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	token = gettok (igs)

	if (IS_NEWCOMMAND(token)) {
	    # No arguments;  print the current parameters
	    call printf ("Point style:  %d vertices;  %s (%d) ")
		call pargi (MG_PTYPN(igps))
		call pargstr (MG_PTYPE(igps))
		call pargi (MG_PTYPS(igps))
	    return

	} else if (token != CONSTANT) {
	    call eprintf ("Number of vertices must be a constant ")
	    return
	}

	call lcmdcat (igs, NO)

	# Get the number of vertices
	if (LOP_TYPE(tokvals) == TY_INT)
	    nvert = LOP_VALI(tokvals)

	else
	    nvert = int (LOP_VALR(tokvals))

	# Get the next argument:  style
	token = gettok (igs)

	if (IS_NEWCOMMAND(token)) {
	    # No argument;  OK if # verts < 2

	    if (nvert > 1) {
		call eprintf ("Need the marker style ")
		return

	    } else
		ptype = 0

	} else if (token == CONSTANT) {
	    # Numerical value

	    if (LOP_TYPE(tokvals) == TY_INT)
		ptype = LOP_VALI(tokvals)

	    else
		ptype = int (LOP_VALR(tokvals))

	    # Convert numeric to text style
	    call ptypic (ptype, ptypec, SZ_LINE)

	} else {
	    # String value;  convert to numeric
	    call strcpy (LOP_VALC(tokvals), ptypec, SZ_LINE)
	    ptype = ptypci (ptypec)
	}

	call ii_ptype (igs, nvert, ptype)

	call lcmdcat (igs, NO)
	call cmdcat  (igs, NO)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Point style:  %s (%d);  %d vertices ")
		call pargi (MG_PTYPS(igps))
		call pargstr (MG_PTYPE(igps))
		call pargi (MG_PTYPN(igps))
	}
end


procedure ptypic (ptype, ctype, maxch)

#  PTYPIC -- Convert numeric marker style code to text

int	ptype
char	ctype[ARB]
int	maxch

begin
	switch (ptype) {

	case OPEN_MARKER:
	    call strcpy ("open", ctype, maxch)

	case SKELETAL_MARKER:
	    call strcpy ("skeletal", ctype, maxch)

	case STARRED_MARKER:
	    call strcpy ("starred", ctype, maxch)

	case SOLID_MARKER:
	    call strcpy ("solid", ctype, maxch)

	case HALF_MARKER:
	    call strcpy ("half", ctype, maxch)
	}
end


int procedure ptypci (ctype)

#  PTYPCI -- Convert text marker style to numeric code

char	ctype[ARB]

int	pt
string	typdic	"|open|skeletal|starred|solid|half|"

int	strdic()

begin
	call strlwr (ctype)
	pt = strdic (ctype, ctype, SZ_LINE, typdic)

	if (pt == 0) {
	    call eprintf ("Unrecognized or ambiguous point type:  %s ")
		call pargstr (ctype)
	}

	return (pt)
end


procedure ii_ptype (igs, nvert, ptype)

#  II_PTYPE -- Set the igi parameters to specify the marker type by
#  style and number of vertices.

pointer	igs		# Parameters structure
int	nvert		# Number of vertices
int	ptype		# Marker style

pointer	igps		# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	MG_PTYPN(igps) = nvert
	MG_PTYPS(igps) = ptype

	call ptypic (ptype, MG_PTYPE(igps), SZ_LINE)
end
