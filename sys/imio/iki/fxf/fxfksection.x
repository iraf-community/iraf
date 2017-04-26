# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<syserr.h>
include	<error.h>
include <ctotok.h>
include <lexnum.h>
include <imhdr.h>
include "fxf.h"

# FXFKSECTION.X -- Routines to parse the FITS kernel section into 
# parameter names and values.

define	KS_EXTNAME	1
define	KS_EXTVER	2
define	KS_APPEND	3
define	KS_NOAPPEND	4
define	KS_OVERWRITE	5
define	KS_DUPNAME	6
define	KS_INHERIT	7
define	KS_NOINHERIT    8 
define	KS_NODUPNAME	9
define	KS_NOOVERWRITE	10
define	KS_EXPAND	11
define	KS_PHULINES	12
define	KS_EHULINES	13
define	KS_PADLINES	14
define	KS_NOEXPAND	15
define	KS_CACHESIZE	16
define	KS_TYPE		17
define	ERROR           -2


# FXF_KSECTION -- Procedure to parse and analyze a string of the form:
#
#     "keyword=value,keyword+,keyword-,..."
# e.g.,
#     "[extname=]name,[extver=]23,append,inherit+,overwrite+,dupname-"
#
# The 'extver' numeric field is position dependent if it does not have
# the parameter name.  The 'group' output variable is not -1 when specified
# as the 1st number in the section.

procedure fxf_ksection (ksection, fit, group)

char 	ksection[ARB]		#I String with kernel section
pointer fit			#I Fits structure pointer
int	group			#O Extension number

bool    extn
char    outstr[LEN_CARD]
char 	identif[LEN_CARD]
int     ip, jp, nident, nexpr, junk, nch, ty, token, ival
int	lex_type, fxf_ks_lex(), ctoi(), ctotok(), lexnum()
errchk	syserr, syserrs

begin
       # The default values should have been already initialized 
       # with a call fxf_ksinit().

       ip = 1
       nexpr = 0
       nident = 0
       extn = false
       group = -1
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
	       if (nexpr != 1 && nexpr != 2 && extn) 
		   call syserr (SYS_FXFKSNV)
	       jp = 1
	       ty = lexnum (outstr, jp, nch)
	       if (ty != LEX_DECIMAL)
		   call syserr (SYS_FXFKSNDEC)
	       jp = 1
	       junk = ctoi (outstr, jp, ival)
	       if (nexpr == 0) {
		   group = ival
		   identif[1] = 1
	       } else
	           FKS_EXTVER(fit) = ival
	       nexpr = nexpr + 1

	   case TOK_PUNCTUATION:
	       if (outstr[1] == ',' && identif[1] == EOS)
		   call syserr (SYS_FXFKSSYN)

	   case TOK_STRING:
	       if (nexpr != 0 && nexpr != 1)
		   call syserr (SYS_FXFKSSVAL)
	       call strcpy (outstr, FKS_EXTNAME(fit), LEN_CARD)
	       nexpr = nexpr + 1
	       extn = true

	   case TOK_IDENTIFIER:
	       nident = nident + 1
	       call strcpy (outstr, identif, LEN_CARD] 
	       call strlwr (outstr)
	       lex_type = fxf_ks_lex (outstr)

	       # look for =<value>, + or -
	       if (lex_type > 0) {
	   	   call fxf_ks_gvalue (lex_type, ksection, ip, fit)
	       } else {
	           if (nexpr == 0 || nexpr == 1)
		       call strcpy (identif, FKS_EXTNAME(fit), LEN_CARD)
	           else
		       call syserr (SYS_FXFKSSVAL)
	       }
	       nexpr = nexpr + 1
	       extn = true

	   default:
		call syserr (SYS_FXFKSSYN)
	   }
	}	   
end


# FXF_KS_LEX -- Map an identifier into a FITS kernel parameter code.

int procedure fxf_ks_lex (outstr)

char    outstr[ARB]

int	len, strlen(), strncmp()
errchk	syserr, syserrs

begin
	len = strlen (outstr)

	# Allow for small string to be taken as extname values and not
	# kernel paramaters; like 'ap' instead of 'ap(ppend)'.
	if (len < 3)
	    return(0)

	# See if it is extname or extver.
	if (strncmp (outstr, "ext", 3) == 0 && len < 8 ) {
	    if (len == 3)
	        call syserr (SYS_FXFKSEXT)
	    if (strncmp (outstr[4], "name", len-3) == 0)
		return (KS_EXTNAME)
	    else if (strncmp (outstr[4], "ver", len-3) == 0)
		return (KS_EXTVER)

	# Check for the "no" versions of selected keywords.
	} else if (strncmp (outstr, "no", 2) == 0 && len < 12) {
	    if (strncmp (outstr[3], "append", len-2) == 0)
	        return (KS_NOAPPEND)
	    if (strncmp (outstr[3], "inherit", len-2) == 0)
	        return (KS_NOINHERIT)
	    if (strncmp (outstr[3], "overwrite", len-2) == 0)
	        return (KS_NOOVERWRITE)
	    if (strncmp (outstr[3], "dupname", len-2) == 0)
	        return (KS_NODUPNAME)
	    if (strncmp (outstr[3], "expand", len-2) == 0)
		return (KS_NOEXPAND)
	}

	# Other kernel keywords.
	if (strncmp (outstr, "inherit", len) == 0)
	    return (KS_INHERIT)
	if (strncmp (outstr, "overwrite", len) == 0)
	    return (KS_OVERWRITE)
	if (strncmp (outstr, "dupname", len) == 0)
	    return (KS_DUPNAME)
	if (strncmp (outstr, "append", len) == 0)
	    return (KS_APPEND)
	if (strncmp (outstr, "noappend", len) == 0)
	    return (KS_NOAPPEND)
	if (strncmp (outstr, "type", len) == 0)
	    return (KS_TYPE)
	if (strncmp (outstr, "expand", len) == 0)
	    return (KS_EXPAND)
	if (strncmp (outstr, "phulines", len) == 0)
	    return (KS_PHULINES)
	if (strncmp (outstr, "ehulines", len) == 0)
	    return (KS_EHULINES)
	if (strncmp (outstr, "padlines", len) == 0)
	    return (KS_PADLINES)
	if (strncmp (outstr, "cachesize", len) == 0)
	    return (KS_CACHESIZE)

	return (0)	# not recognized; probably a value
end


# FXF_KS_GVALUE -- Given a parameter code get its value at the 'ip' character
# position in the 'ksection' string.  Put the values in the FKS structure.

procedure fxf_ks_gvalue (param, ksection, ip, fit)

int	param			#I parameter code
char	ksection[ARB]		#I Ksection
int	ip			#I Current parsing pointer in ksection
pointer	fit			#U Update the values in the FKS structure

pointer sp, ln
int     jp, token
int	ctotok()
errchk	syserr, syserrs

begin
	jp = ip

	call smark (sp)
	call salloc (ln, LEN_CARD, TY_CHAR)

	# See if the parameter value is given as par=<value> or '+/-'
	if (ctotok (ksection, jp, Memc[ln], LEN_CARD) == TOK_OPERATOR) {
	    if (Memc[ln] == '=' ) {
		token = ctotok (ksection, jp, Memc[ln], LEN_CARD)
		if (token != TOK_IDENTIFIER &&
			token != TOK_STRING && token != TOK_NUMBER) {
		    call syserr (SYS_FXFKSSYN)
		} else {
		    call fxf_ks_val (Memc[ln], param, fit)
		    ip = jp
	        }
	    } else if (Memc[ln] == '+' || Memc[ln] == '-') {
		call fxf_ks_pm (Memc[ln], param, fit)
		ip = jp
	    }
	} else {
	    switch (param) {
	    case  KS_APPEND:
		FKS_APPEND(fit) = YES
	    case  KS_NOAPPEND:
		FKS_APPEND(fit) = NO
	    case  KS_OVERWRITE:
		FKS_OVERWRITE(fit) = YES
	    case  KS_NOOVERWRITE:
		FKS_OVERWRITE(fit) = NO
	    case  KS_DUPNAME:
		FKS_DUPNAME(fit) = YES
	    case  KS_INHERIT:
		FKS_INHERIT(fit) = YES
	    case  KS_NOINHERIT:
		FKS_INHERIT(fit) = NO
	    case  KS_EXPAND:
		FKS_EXPAND(fit) = YES
	    case  KS_NOEXPAND:
		FKS_EXPAND(fit) = NO
	    default:
		call syserr (SYS_FXFKSSYN)
	    }
	} 

	call sfree (sp)
end


# FXF_KS_VALUE -- Returns the value of a parameter in the kernel section.

procedure fxf_ks_val (outstr, param, fit)

char	outstr[ARB] 		#I Input string with value		
int	param			#I Parameter code
pointer fit			#U Fits kernel descriptor

int	ty, ip, ival, nchars
int	lexnum(), ctoi(), strcmp()
errchk	syserr, syserrs

begin
	call strlwr (outstr)
	if (strcmp (outstr, "yes") == 0)
	    ival = YES
	else if (strcmp (outstr, "no") == 0)
	    ival = NO
	else
	    ival = ERROR

	switch (param) {
	case  KS_EXTNAME:
	    call strcpy (outstr, FKS_EXTNAME(fit), LEN_CARD)

	case  KS_TYPE:
	    call strlwr (outstr)
	    if (strcmp ("mask", outstr) == 0)
		FKS_SUBTYPE(fit) = FK_PLIO
	    else
		call syserrs (SYS_FXFKSINVAL, "type")
	case KS_EXTVER:
	    ip = 1
	    ty = lexnum (outstr, ip, nchars)
	    if (ty != LEX_DECIMAL)
	        call syserr (SYS_FXFKSNDEC)
	    ip = 1
	    nchars = ctoi (outstr, ip, ival)
	    if (nchars <= 0)
	        call syserrs (SYS_FXFKSINVAL, "extver")
	    FKS_EXTVER(fit) = ival

	case KS_APPEND:
	    if (ival != ERROR)
	        FKS_APPEND(fit) = ival
	    else
	        call syserrs (SYS_FXFKSINVAL, "append")

	case KS_OVERWRITE:
	    if (ival != ERROR)
	        FKS_OVERWRITE(fit) = ival
	    else
	        call syserrs (SYS_FXFKSINVAL, "overwrite")

	case KS_DUPNAME:
	    if (ival != ERROR)
	        FKS_DUPNAME(fit) = ival
	    else
	        call syserrs (SYS_FXFKSINVAL, "dupname")

	case KS_INHERIT:
	    if (ival != ERROR)
	        FKS_INHERIT(fit) = ival
	    else
	        call syserrs (SYS_FXFKSINVAL, "inherit")

	case KS_EXPAND:
	    if (ival != ERROR)
	        FKS_EXPAND(fit) = ival
	    else
	        call syserrs (SYS_FXFKSINVAL, "expand")

	case KS_PHULINES:
	    ip = 1
	    ty = lexnum (outstr, ip, nchars)
	    if (ty != LEX_DECIMAL)
	        call syserr (SYS_FXFKSNDEC)
	    ip = 1
	    nchars = ctoi (outstr, ip, ival)
	    if (nchars <= 0 || ival < 0)
	        call syserrs (SYS_FXFKSPVAL, "phulines")
	    FKS_PHULINES(fit) = ival

	case KS_EHULINES:
	    ip = 1
	    ty = lexnum (outstr, ip, nchars)
	    if (ty != LEX_DECIMAL)
	        call syserr (SYS_FXFKSNDEC)
	    ip = 1
	    nchars = ctoi (outstr, ip, ival)
	    if (nchars <= 0 || ival < 0)
	        call syserrs (SYS_FXFKSPVAL, "ehulines")
	    FKS_EHULINES(fit) = ival

	case KS_PADLINES:
	    ip = 1
	    ty = lexnum (outstr, ip, nchars)
	    if (ty != LEX_DECIMAL)
	        call syserr (SYS_FXFKSNDEC)
	    ip = 1
	    nchars = ctoi (outstr, ip, ival)
	    if (nchars <= 0 || ival < 0)
	        call syserrs (SYS_FXFKSPVAL, "padlines")
	    FKS_PADLINES(fit) = ival

	case KS_CACHESIZE:
	    ip = 1
	    ty = lexnum (outstr, ip, nchars)
	    if (ty != LEX_DECIMAL)
	        call syserr (SYS_FXFKSNDEC)
	    ip = 1
	    nchars = ctoi (outstr, ip, ival)
	    if (nchars <= 0 || ival < 0)
	        call syserrs (SYS_FXFKSPVAL, "cachesize")
	    FKS_CACHESIZE(fit) = ival

	default:
            call syserr (SYS_FXFKSSYN)
	}
end


# FXF_KS_PM -- Return the character YES or NO based on the value '+' or '-'
 
procedure fxf_ks_pm (pm, param, fit)

char	pm[1]		#I contains "+" or "-"
int	param		#I Parameter code
pointer fit		#U Fits kernel descriptor

int	ival
errchk	syserr, syserrs

begin
	if (pm[1] == '+') 
	   ival = YES
	else
	   ival = NO

	switch (param) {
	case KS_APPEND:
	    FKS_APPEND(fit) = ival
	case KS_OVERWRITE:
	    FKS_OVERWRITE(fit) = ival
	case KS_DUPNAME:
	    FKS_DUPNAME(fit) = ival
	case KS_INHERIT:
	    FKS_INHERIT(fit) = ival
	case KS_EXPAND:
	    FKS_EXPAND(fit) = ival
	default:
	    call syserr (SYS_FXFKSSYN)
	}
end


# FXF_KS_ERRORS -- Handle an error condition in the kernel section.

procedure fxf_ks_errors (fit, acmode)

pointer fit 			#I fits kernel descriptor
int	acmode			#I image access mode

int	group
errchk	syserr, syserrs

begin
	group = FIT_GROUP(fit)

	if (FKS_OVERWRITE(fit) == YES) {
	    if (FIT_NEWIMAGE(fit) == YES)
		iferr (call syserrs (SYS_FOPNNEXFIL, IM_HDRFILE(FIT_IM(fit))))
		    call erract (EA_WARN)
	    if (acmode == APPEND)
		call syserrs (SYS_FXFKSNOVR, "APPEND")
	    if (group == -1 &&
		    (FKS_EXTNAME(fit) == EOS && IS_INDEFL(FKS_EXTVER(fit))))
		call syserr (SYS_FXFKSOVR)
	} else {
	    switch (acmode) {
	    case NEW_COPY:
		if (group != -1 && FKS_APPEND(fit) == NO)
		    call syserr (SYS_FXFKSBOP)
	    case NEW_IMAGE:
		if (group != -1)
		    call syserrs (SYS_FXFKSNEXT, "NEW_IMAGE" )
	    case APPEND:
	        if (group != -1)
		    call syserrs (SYS_FXFKSNEXT, "APPEND" )
	    }
        }
end			


# FXF_KSINIT -- Initialize default values for ks parameters.

procedure fxf_ksinit (fit)

pointer	fit			#I fits kernel descriptor

begin
	FKS_EXTNAME(fit)   = EOS
	FKS_SUBTYPE(fit)   = NO
	FKS_EXTVER(fit)    = INDEFL
	FKS_APPEND(fit)    = NO
	FKS_OVERWRITE(fit) = NO
	FKS_DUPNAME(fit)   = NO
	FKS_EXPAND(fit)    = YES
	FKS_PHULINES(fit)  = DEF_PHULINES
	FKS_EHULINES(fit)  = DEF_EHULINES
	FKS_PADLINES(fit)  = DEF_PADLINES
	FKS_INHERIT(fit)   = YES
	FKS_CACHESIZE(fit) = DEF_CACHE
end
