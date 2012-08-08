# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"stf.h"

# STF_GETI -- Return the integer value of a FITS encoded card.

procedure stf_geti (card, ival)

char	card[ARB]		# card to be decoded
int	ival			# receives integer value

int	ip, ctoi()
char	sval[FITS_SZVALSTR]

begin
	call stf_gets (card, sval, FITS_SZVALSTR)
	ip = 1
	if (ctoi (sval, ip, ival) <= 0)
	    ival = 0
end


# STF_GETB -- Return the boolean/integer value of a FITS encoded card.

procedure stf_getb (card, bval)

char	card[ARB]		# card to be decoded
int	bval			# receives YES/NO

char	sval[FITS_SZVALSTR]

begin
	call stf_gets (card, sval, FITS_SZVALSTR)
	if (sval[1] == 'T')
	    bval = YES
	else
	    bval = NO
end


# STF_GETS -- Get the string value of a FITS encoded card.  Strip leading
# and trailing whitespace and any quotes.

procedure stf_gets (card, outstr, maxch)

char	card[ARB]		# FITS card to be decoded
char	outstr[ARB]		# output string to receive parameter value
int	maxch

int	ip, op
int	ctowrd(), strlen()

begin
	ip = FITS_STARTVALUE
	if (ctowrd (card, ip, outstr, maxch) > 0) {
	    # Strip trailing whitespace.
	    op = strlen (outstr)
	    while (op > 0 && (IS_WHITE(outstr[op]) || outstr[op] == '\n'))
		op = op - 1
	    outstr[op+1] = EOS
	} else
	    outstr[1] = EOS
end


# STF_GETCMT -- Get the comment field of a FITS encoded card.

procedure stf_getcmt (card, comment, maxch)

char	card[ARB]		#I FITS card to be decoded
char	comment[ARB]		#O output string to receive comment
int	maxch			#I max chars out

int	ip, op
int	lastch

begin
	# Find the slash which marks the beginning of the comment field.
	ip = FITS_ENDVALUE + 1
	while (card[ip] != EOS && card[ip] != '\n' && card[ip] != '/')
	    ip = ip + 1

	# Copy the comment to the output string, omitting the /, any
	# trailing blanks, and the newline.

	lastch = 0
	do op = 1, maxch {
	    if (card[ip] == EOS)
		break
	    ip = ip + 1
	    comment[op] = card[ip]
	    if (card[ip] > ' ')
		lastch = op
	}
	comment[lastch+1] = EOS
end
