include "igi.h"

procedure ig_fontset (igs)

#  ig_fontset -- the igi FONTSET command.  Use "soft" (igi) or "hard"
#  (gio) fonts.  A text keyword specifies which.
## 16 June 1992 ZGL
## 21 July 1992 Add check for alternate symbols (HARD_FONTS, SOFT_FONTS).  ZGL

pointer	igs		# Parameters structure

int	in		# Input stream descriptor
pointer	tokvals		# Token value structure
pointer	igps		# Plot parameters structure

int	token
char	fsetc[SZ_LINE]

int	gettok(), fsetci()

begin
	call lcmdcat (igs, YES)
	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	token = gettok (igs)

	if (IS_NEWCOMMAND(token)) {
	    # No arguments;  print the current font set

	    if (MG_FONTSET(igps) == IGI_FONTS ||
		MG_FONTSET(igps) == SOFT_FONTS)
	        call printf ("Using soft (igi) fonts ")

	    else if (MG_FONTSET(igps) == GIO_FONTS ||
		     MG_FONTSET(igps) == HARD_FONTS)
	        call printf ("Using hard (gio) fonts ")

	    else
	        call printf ("Don't know which fonts to use ")

	    return

	} else if (token == IDENTIFIER) {
	    # Keyword font set name
	    call strcpy (LOP_VALC(tokvals), fsetc, SZ_LINE)

	    call ii_fontset (igs, fsetci (fsetc))
	    return

	} else
	    call eprintf ("Font set must be Soft or Hard ")

	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Font Set %s (%d) ")
		call pargstr (fsetc)
		call pargi (MG_FONTSET(igps))
	}
end


int procedure fsetci (fsetc)

#  PTYPCI -- Convert text marker style to numeric code

char	fsetc[ARB]

int	fs
string	fsdict	"|igi|gio|soft|hard|"

int	strdic()

begin
	call strlwr (fsetc)

	fs = strdic (fsetc, fsetc, SZ_LINE, fsdict)

	if (fs == 0) {
	    call eprintf ("Unrecognized or ambiguous font set:  %s ")
		call pargstr (fsetc)
	}

	return (fs)
end


procedure ii_fontset (igs, fontset)

#  ii_fontset -- Set the igi parameter to specify the font set.

pointer	igs		# Parameters structure
int	fontset		# Font set code

pointer	igps		# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	MG_FONTSET(igps) = fontset
end
