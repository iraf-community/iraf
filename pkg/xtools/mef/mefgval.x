include	<ctype.h>
include <pkg/mef.h>


# MEFGVAL.X -- Set of routines to decode the value  of a FITS keyword given
# the whole card.


# MEF_GVALI -- Return the integer value of a FITS encoded card.

procedure mef_gvali (card, ival)

char	card[ARB]		#I card to be decoded
int	ival			#O receives integer value

int	ip, ctoi()
char	sval[MEF_SZVALSTR]

begin
	call mef_gvalt (card, sval, MEF_SZVALSTR)
	ip = 1
	if (ctoi (sval, ip, ival) <= 0)
	    ival = 0
end


# MEF_GVALR -- Return the real value of a FITS encoded card.

procedure mef_gvalr (card, rval)

char	card[ARB]		#I card to be decoded
real	rval			#O receives integer value

int	ip, ctor()
char	sval[MEF_SZVALSTR]

begin
	call mef_gvalt (card, sval, MEF_SZVALSTR)
	ip = 1
	if (ctor (sval, ip, rval) <= 0)
	    rval = 0.0
end


# MEF_GVALD -- Return the double value of a FITS encoded card.

procedure mef_gvald (card, dval)

char	card[ARB]		#I card to be decoded
double	dval			#O receives integer value

int	ip, ctod()
char	sval[MEF_SZVALSTR]

begin
	call mef_gvalt (card, sval, MEF_SZVALSTR)
	ip = 1
	if (ctod (sval, ip, dval) <= 0)
	    dval = 0.0
end


# MEF_GVALB -- Return the boolean/integer value of a FITS encoded card.

procedure mef_gvalb (card, bval)

char	card[ARB]		#I card to be decoded
int	bval			#O receives YES/NO

char	sval[MEF_SZVALSTR]

begin
	call mef_gvalt (card, sval, MEF_SZVALSTR)
	if (sval[1] == 'T')
	    bval = YES
	else
	    bval = NO
end


# MEF_GVALT -- Get the string value of a FITS encoded card.  Strip leading
# and trailing whitespace and any quotes.

procedure mef_gvalt (card, outstr, maxch)

char	card[ARB]		#I FITS card to be decoded
char	outstr[ARB]		#O output string to receive parameter value
int	maxch			#I length of outstr

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


# MEF_GETCMT -- Get the comment field of a FITS encoded card.

procedure mef_getcmt (card, comment, maxch)

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


# MEF_GLTM -- Procedure to convert an input time stream with hh:mm:ss
#	     and date stream dd/mm/yy into seconds from jan 1st 1980.

procedure mef_gltm (time, date, limtime)

char    time[ARB]	#I time
char	date[ARB]	#I date
int	limtime		#O seconds

int     hr,mn,sec,days,month,year, ip, iy, days_per_year, ctoi(),i
int	month_to_days[12], adays

data    month_to_days / 0,31,59,90,120,151,181,212,243,273,304,334/

begin
	ip = 1
	ip = ctoi (time, ip, hr)
	ip = 1 
	ip = ctoi (time[4], ip, mn)
	ip = 1 
	ip = ctoi (time[7], ip, sec)

	sec = sec + mn * 60 + hr * 3600

	ip = 1
	ip = ctoi (date, ip, days)
	ip = 1 
	ip = ctoi (date[4], ip, month)
	ip = 1 
	ip = ctoi (date[7], ip, year)

	days_per_year = 0

	iy = year + 1900
	do i = 1, iy - 1980
	    days_per_year = days_per_year + 365

	adays= (year-80)/4
	if (month > 2) adays=adays+1

	days = adays + days-1 + days_per_year + month_to_days[month]

	limtime = sec + days * 86400
end
