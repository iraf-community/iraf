include <ctotok.h>
include <lexnum.h>
include <pkg/mef.h>

define	KS_EXTNAME	1
define	KS_EXTVER	2

# MEF_KSECTION -- Procedure to parse and analyze a string of the form
#
#  "(extname=)name,(extver=)23"
#
#   The numeric field is position depend if it does not have 'extver'.

procedure mef_ksection (ksection, extname, extver)

char 	ksection[ARB]	#I String with kernel section
char	extname[ARB]	#O Extname
int	extver		#O Extver

int     ctotok(),ip, jp, nident, nexpr
int	junk, nch, lexnum(), ty, token, ival
char    outstr[LEN_CARD]
char 	identif[LEN_CARD]
int	lex_type, mef_klex(), ctoi()
 
begin
	
        extname[1] = EOS
        extver = INDEFL
	ip = 1
	nident = 0
	nexpr = 0
	identif[1] = EOS

	repeat {
	    # Advance to the next keyword.
	    token = ctotok (ksection, ip, outstr, LEN_CARD)

	    switch (token) {
	    case TOK_EOS:
		break
	    case TOK_NEWLINE:
		break
	    case TOK_NUMBER:
	       if (nexpr != 1)
		  call error(13,
		  "Numeric value only allow as second term in ksection")
	       jp = 1
	       ty = lexnum (outstr, jp, nch)
	       if (ty != LEX_DECIMAL)
		  call error(13, "Number is not decimal")
	       jp = 1
	       junk = ctoi(outstr, jp, ival)
	       extver = ival
	       nexpr = nexpr + 1
	    case TOK_PUNCTUATION:
		if (outstr[1] == ',' && identif[1] == EOS)
		   call error(13,"syntax error in kernel section")
	    case TOK_STRING:
	       if (nexpr != 0)
		  call error(13,
		   "String value only allow as first term in ksection")

		call strcpy (outstr, extname, LEN_CARD)
	       nexpr = nexpr + 1
	    case TOK_IDENTIFIER:
		nident = nident + 1
		call strcpy(outstr, identif, LEN_CARD] 
		call strlwr(outstr)
		lex_type = mef_klex (outstr)
		# See if it is a reserved keyword.
		jp = ip
		# look for =, + or -
	        if (lex_type > 0) {
	            # Now see if of the type lex=<value> or lex+/-
		    if (ctotok (ksection, ip, outstr, LEN_CARD) == 
						TOK_OPERATOR) {
		        if (outstr[1] == '=' ) {
		            token = ctotok (ksection, ip, outstr, LEN_CARD)
		            if (token != TOK_IDENTIFIER &&
				token != TOK_STRING &&
			        token != TOK_NUMBER)
			           call error(13,
				   "syntax error in kernel section")
		             else
		                 call mef_kvalue(outstr, lex_type, 
					   extname, extver)
	                } else
		            ip = jp
	            }
	        } else {
	            if (nexpr == 0)
		        call strcpy (identif, extname, LEN_CARD)
	            else {
		        call error(13,
		           "String value only allow as first term in ksection")
	            }
	        }
		nexpr = nexpr + 1
	    default:
	        call error (13, "Syntax error in ksection")
	    }
	}	   
end


# MEF_KLEX -- Returns the lexival value of a parameter in string.

int procedure mef_klex (outstr)

char    outstr[ARB]	#I string

int	len, strlen(), strncmp()
char	tmp[LEN_CARD]

begin
	len = strlen(outstr)
	# See if it is extname or extversion
	if (strncmp (outstr, "ext", 3) == 0 && len < 8 ) {
	   if (len == 3)
	      call error(13, "'ext' is ambiguous in ksection")
	   call strcpy ("name", tmp, 4)
	   if (strncmp(outstr[4], tmp, len-3) == 0)
	      return (KS_EXTNAME)
	   else {
	      call strcpy ("ver", tmp, 3)
	      if (strncmp(outstr[4], tmp, len-3) == 0)
		 return (KS_EXTVER)
	   }
	}

	return (0)	# Is a value

end


define	ERROR   -2
# MEF_KVALUE -- Get the value from a string of extname and extver.

procedure mef_kvalue(outstr, lex_type, extname, extver)

char	outstr[ARB]	#I Input string
int	lex_type	#I Type of value
char    extname[ARB]	#O Extname
int	extver		#O Extver

int	ty, lexnum(), ip, ival, ctoi(), nch, junk
int	strcmp()

begin
	call strlwr(outstr)
	if (strcmp (outstr, "yes") == 0)
	   ival = YES
	else if (strcmp (outstr, "no") == 0)
	   ival = NO
	else
	   ival = ERROR

	switch (lex_type) {
	case  KS_EXTNAME:
	   call strcpy (outstr, extname, LEN_CARD)
	case  KS_EXTVER:
	   ip = 1
	   ty = lexnum (outstr, ip, nch)
	   if (ty != LEX_DECIMAL)
	      call error(13, "Number is not a decimal")
	   ip = 1
	   junk = ctoi(outstr, ip, ival)
	   extver = ival
	default:
	     call error(13, "Syntax error in ksection")

	}
end
