include "igi.h"

#  8/20/91 Removed ^Ls. ZGL

procedure ig_justify (igs)

pointer	igs		# igi parameters structure

pointer	tokvals		# Token value structure
int	token
int	igps
int	just
pointer	sp, cjust

int	gettok(), justci()

begin
	call lcmdcat (igs, YES)

	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	call smark (sp)
	call salloc (cjust, SZ_LINE, TY_CHAR)

	token = gettok (igs)

	if (token == CONSTANT) {
	    # Numeric justification code argument
	    call lcmdcat (igs, NO)
	    if (LOP_TYPE(tokvals) == TY_INT)
		just = LOP_VALI(tokvals)
	    else
		just = int (LOP_VALR(tokvals))
	    call justic (just, Memc[cjust], SZ_LINE)

	} else if (token == IDENTIFIER || token == STRING) {
	    # First (horizontal) justification argument
	    call lcmdcat (igs, NO)
	    call strcpy (LOP_VALC(tokvals), Memc[cjust], SZ_LINE]

	    token = gettok (igs)
	    if (token == IDENTIFIER || token == STRING) {
		# Second (vertical) justification argument
		call lcmdcat (igs, NO)
		call strcat (" ", Memc[cjust], SZ_LINE]
		call strcat (LOP_VALC(tokvals), Memc[cjust], SZ_LINE]
	    } else {
		call eprintf ("Invalid justification argument ")
		call sfree (sp)
		return
	    }
	    just = justci (Memc[cjust])
	    if (just == 0) {
		call eprintf ("Invalid justification:  %s ")
		    call pargstr (Memc[cjust])
		call sfree (sp)
		return
	    }

	} else if (IS_NEWCOMMAND(token)) {
	    # No argument;  list the current justification
	    call justic (MG_IJUSTC(igps), Memc[cjust], SZ_LINE)
	    call eprintf ("Label justificatin:  %s (%d) ")
		call pargstr (Memc[cjust])
		call pargi (just)
	    call sfree (sp)
	    return

	} else {
	    call eprintf ("Invalid justification argument ")
	    call sfree (sp)
	    return
	}

	call ii_justify (igs, just)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Justify:  %s (%d) ")
		call pargstr (Memc[cjust])
		call pargi (just)
	}

	call cmdcat (igs, NO)

	call sfree (sp)
end


procedure ii_justify (igs, just)

pointer	igs		# igi parameters structure
int	just

begin
	MG_IJUSTC(PLOT_PARMS(igs)) = just
end


procedure justic (just, cjust, maxch)

int	just		# Justification code
char	cjust[ARB]	# Justification text
int	maxch		# String size

begin
	switch (just) {
	case 1:
	    call strcpy ("right top", cjust, maxch)
	case 2:
	    call strcpy ("center top", cjust, maxch)
	case 3:
	    call strcpy ("left top", cjust, maxch)
	case 4:
	    call strcpy ("right center", cjust, maxch)
	case 5:
	    call strcpy ("center center", cjust, maxch)
	case 6:
	    call strcpy ("left center", cjust, maxch)
	case 7:
	    call strcpy ("right bottom", cjust, maxch)
	case 8:
	    call strcpy ("center bottom", cjust, maxch)
	case 9:
	    call strcpy ("left bottom", cjust, maxch)
	default:
	    call strcpy ("center center", cjust, maxch)
	}
end


int procedure justci (cjust)

char	cjust[ARB]

char	first[10], second[10]
int	ip
int	nchar
int	d1, d2
int	just

string	justdic	"|left|right|bottom|top|center|"

int	ctowrd(), strdic()

begin
	if (cjust[1] == EOS)
	    return (0)

	ip = 1

	nchar = ctowrd (cjust, ip, first, 10)
	if (nchar == 0)
	    return (0)
	call strlwr (first)
	d1 = strdic (first, first, 10, justdic)
	if (d1 == 0)
	    return (0)

	nchar = ctowrd (cjust, ip, second, 10)
	if (nchar == 0)
	    return (0)
	call strlwr (second)
	d2 = strdic (second, second, 10, justdic)
	if (d2 == 0)
	    return (0)

	switch (d1) {
	# Horizontal justification
	case 1:
	    # Left
	    switch (d2) {
		case 3:
		    # Bottom
		    just = 9
		case 4:
		    # Top
		    just = 3
		case 5:
		    # Center
		    just = 6
	    }
	case 2:
	    # Right
	    switch (d2) {
		case 3:
		    # Bottom
		    just = 7
		case 4:
		    # Top
		    just = 1
		case 5:
		    # Center
		    just = 4
	    }
	case 5:
	    # Center
	    switch (d2) {
		case 3:
		    # Bottom
		    just = 8
		case 4:
		    # Top
		    just = 2
		case 5:
		    # Center
		    just = 5
	    }
	}
	return (just)
end
