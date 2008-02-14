# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<ctype.h>
include	"fxf.h"


# FXF_CTYPE -- Determine the type of a FITS card. 

int procedure fxf_ctype (card, kwindex)

char	card[ARB]			#I FITS card (or keyword)
int	kwindex				#O index number, if any

pointer sp, kwname
char	kw[SZ_KEYWORD]
int	index, ch, i, ip
int	strncmp(), strdic(), strlen(), ctoi()
string  keywords FK_KEYWORDS

begin
	call smark (sp)
	call salloc (kwname, LEN_CARD, TY_CHAR)

	# Check for a reference to one of the NAXIS keywords.
	kwindex= 0
	if (card[1] == 'N')
	    if (strncmp (card, "NAXIS", 5) == 0) {
		ch = card[6]
		if (ch == EOS || (IS_DIGIT(ch) && card[7] == ' ')) {
		    kwindex = TO_INTEG(ch)
		}
		call sfree (sp)
		return (KW_NAXIS)
	    }

	# See if it is one of the "T"-prefixed (binary table) keywords.
	if (card[1] == 'T') {
	    ip = 6
	    if (strncmp (card, "TFORM", 5) == 0) {
		if (ctoi (card, ip,  kwindex) < 1)
		    kwindex = 0
		call sfree (sp)
		return (KW_TFORM)
	    }
	    if (strncmp (card, "TTYPE", 5) == 0) {
		if (ctoi (card, ip,  kwindex) < 1)
		    kwindex = 0
		call sfree (sp)
		return (KW_TTYPE)
	    }
	}

	# Get keyword name in lower case with no blanks. 
        do i = 1, SZ_KEYWORD {
	    if (IS_WHITE(card[i])) {
		kw[i] = EOS
		break
	    } else if (IS_UPPER(card[i]))
		kw[i] = TO_LOWER (card[i])
	    else
		kw[i] = card[i]
	}

	# Look up keyword in dictionary.  Abbreviations are not permitted.
	index = strdic (kw, Memc[kwname], LEN_CARD, keywords)
	if (index != 0)
	    if (strlen(kw) != strlen(Memc[kwname]))
		index = 0

	call sfree (sp)
	return (index)
end
