include <syserr.h>
include	<error.h>
include <ctotok.h>
include <lexnum.h>

# parameter names and values.

define  HS_ADD		1
define  HS_ADDONLY	2
define	HS_UPDATE	3
define	HS_VERIFY	4
define	HS_SHOW		5
define  HS_DELETE	6
define  HS_RENAME	7
define  HS_FIELD	8
define  HS_VALUE	9
define  HS_COMMENT	10
define  HS_BEFORE	11
define  HS_AFTER	12
define	ERROR           -2

define  HADD		Memi[$1]
define  HADDONLY	Memi[$1+1]
define	HUPDATE         Memi[$1+2]
define	HVERIFY         Memi[$1+3]
define	HSHOW           Memi[$1+4]
define  HDELETE         Memi[$1+5]
define  HRENAME         Memi[$1+6]
define  HBAF	        Memi[$1+7]
define	HFIELD          Memc[P2C($1+10)]
define  HVALUE	        Memc[P2C($1+46)]
define  HCOMMENT        Memc[P2C($1+86)]
define  HBAFVALUE       Memc[P2C($1+126)]

define  HSZ             200

define  OP_EDIT         1               # hedit opcodes
define  OP_INIT         2
define  OP_ADD          3
define  OP_DELETE       4
define  OP_DEFPAR       5
define  OP_RENAME       6
define  BEFORE		1
define  AFTER		2

define  LEN_CARD 80

# HE_CMDPARS -- Procedure to parse and analyze a string of the form:
#

procedure he_getcmdf (cmd, operation, fields, valexpr, comment, pkey, baf,
      update, verify, show)


char 	cmd[ARB]		#I String with kernel section
int	operation
char	fields[ARB]
char	valexpr[ARB]
char    comment[ARB]
char    pkey[ARB]
int	baf
int	update
int	verify
int	show

pointer hc
char    outstr[LEN_CARD]
char 	identif[LEN_CARD], dot
int     ip, nexpr, token, add, addonly, delete, rename, nch
bool    streq()
int	lex_type, ctotok(), he_ks_lex(), ctowrd()
errchk	syserr, syserrs

begin
	# The default values should have been already initialized 
	# with a call fxf_ksinit().

	call calloc(hc, HSZ, TY_STRUCT)
	call  he_ksinit (hc)

	ip = 1
	nexpr = 0
	identif[1] = EOS

	repeat {
	    # Advance to the next keyword.
	    if (ip == 1) {
	       nch= ctowrd(cmd, ip, outstr, LEN_CARD)
	       token = TOK_IDENTIFIER
	    } else {
	       token = ctotok (cmd, ip, outstr, LEN_CARD)
	    } 

	    if (token == TOK_CHARCON) {
	       ip = ip - 2
	       nch= ctowrd(cmd, ip, outstr, LEN_CARD)
	       if (nexpr >= 1)
		   token = TOK_STRING
	       if (nch <=3) {
		   #ctowrd will not parse one letter string, doit in here.
		   outstr[1]=cmd[ip-2]
		   outstr[2]=EOS
	       }
	    }

	    if (token == TOK_STRING && nexpr == 0)
	       token = TOK_IDENTIFIER
	    switch (token) {
	    case TOK_EOS:
	       break
	    case TOK_NEWLINE:
	       break

	    case TOK_NUMBER:
	       if (nexpr != 1) {
		   call eprintf ("%s\n")
		       call pargstr (cmd)
		   call error (13,"Numeric value not allow in this field")
		}
	       call strcpy (outstr, HVALUE(hc), LEN_CARD)
	       nexpr = nexpr + 1
	    case TOK_CHARCON:
	       ip = ip - 1
	    case TOK_STRING:
	       if (nexpr != 1 && nexpr != 2) {
		   call eprintf ("%s\n")
		       call pargstr (cmd)
		   call error(13, "Value or comment error")
		}
	       if (nexpr == 1) 
		   call strcpy (outstr, HVALUE(hc), LEN_CARD)
	       if (nexpr == 2) 
		   call strcpy (outstr, HCOMMENT(hc), LEN_CARD)
	       nexpr = nexpr + 1

	    case TOK_IDENTIFIER:
	       call strcpy (outstr, identif, LEN_CARD)
	       call strlwr (outstr)
	       lex_type = he_ks_lex (outstr)

	       if (streq(identif, "comment") && nexpr == 0)
		   lex_type = 0
	       # look for =<value>, + or -
	       if (lex_type > 0) {
		   call he_ks_gvalue (lex_type, cmd, ip, hc)
	       } else {
		   #if (nexpr == 0 || nexpr == 1)
		   if (nexpr == 0)
		       call strcpy (identif, HFIELD(hc), LEN_CARD)
		   else if (nexpr == 1)
		       call strcpy (outstr, HVALUE(hc), LEN_CARD)
		   else {
		       call eprintf ("%s\n")
			   call pargstr (cmd)
		       call error(13, "Field or value error")
		    }
	       }
	       nexpr = nexpr + 1

	    case TOK_OPERATOR:
		dot = outstr[1]
		if (nexpr == 1 && dot == '.')
		   call strcpy (outstr, HVALUE(hc), LEN_CARD)
		else if (nexpr == 2 && dot == '.')
		   call strcpy (outstr, HCOMMENT(hc), LEN_CARD)
		else {
		   call eprintf ("%s\n")
		       call pargstr (cmd)
		   call error(13,"error in tok_operator value")
		}
		nexpr = nexpr + 1

	    default:
		#call error(13,"error in command line")
	    }
	}	   

	call strcpy (HFIELD(hc), fields, LEN_CARD)
	call strcpy (HVALUE(hc), valexpr, LEN_CARD)
	call strcpy (HCOMMENT(hc), comment, LEN_CARD)
	call strcpy (HBAFVALUE(hc), pkey, LEN_CARD)
	baf	= HBAF(hc)
	add	= HADD(hc)
	addonly	= HADDONLY(hc)
	update	= HUPDATE(hc)
	verify	= HVERIFY(hc)
	show	= HSHOW(hc)
	delete	= HDELETE(hc)
	rename	= HRENAME(hc)

	operation = OP_EDIT
	if (add == -1 && addonly == -1 && delete == -1 && rename == -1)
	    operation = OP_DEFPAR
	else if (add == YES)
	    operation = OP_ADD
	else if (addonly == YES)
	    operation = OP_INIT
	else if (delete == YES)
	    operation = OP_DELETE
	else if (rename == YES)
	    operation = OP_RENAME

	if (streq (fields, "default_pars"))
	    operation = -operation

	call mfree(hc, TY_STRUCT)
end


# HE_KS_LEX -- Map an identifier into a header parameter code.

int procedure he_ks_lex (outstr)

char    outstr[ARB]

int	len, strlen(), strncmp()
errchk	syserr, syserrs

begin
	len = strlen (outstr)

	# Allow for small string to be taken as keyword names
	# and not hedit parameters, like 'up' instead of 'up(date)'.
	if (len < 3)
	    return(0)

        # Other kernel keywords.
        if (strncmp (outstr, "field", len) == 0)
            return (HS_FIELD)
        if (strncmp (outstr, "value", len) == 0)
            return (HS_VALUE)
        if (strncmp (outstr, "comment", len) == 0)
            return (HS_COMMENT)
        if (strncmp (outstr, "after", len) == 0) 
            return (HS_AFTER)
        if (strncmp (outstr, "before", len) == 0)
            return (HS_BEFORE)
        if (strncmp (outstr, "add", len) == 0)
            return (HS_ADD)
        if (strncmp (outstr, "addonly", len) == 0)
            return (HS_ADDONLY)
        if (strncmp (outstr, "delete", len) == 0)
            return (HS_DELETE)
        if (strncmp (outstr, "rename", len) == 0)
            return (HS_RENAME)
        if (strncmp (outstr, "verify", len) == 0)
            return (HS_VERIFY)
        if (strncmp (outstr, "show", len) == 0) {
            return (HS_SHOW)
	}
        if (strncmp (outstr, "update", len) == 0)
            return (HS_UPDATE)

	return (0)	# not recognized; probably a value
end


# FXF_KS_GVALUE -- Given a parameter code get its value at the 'ip' character
# position in the 'ksection' string.  Put the values in the FKS structure.

procedure he_ks_gvalue (param, cmd, ip, hc)

int	param			#I parameter code
char	cmd[ARB]		#I Ksection
int	ip			#I Current parsing pointer in ksection
pointer	hc			#U Update the values in the FKS structure

pointer sp, ln
int     jp, token
int	ctotok()
errchk	syserr, syserrs

begin
	jp = ip

	call smark (sp)
	call salloc (ln, LEN_CARD, TY_CHAR)

	# See if the parameter value is given as par=<value> or '+/-'
	if (ctotok (cmd, jp, Memc[ln], LEN_CARD) == TOK_OPERATOR) {
	    if (Memc[ln] == '=' ) {
		token = ctotok (cmd, jp, Memc[ln], LEN_CARD)
		if (token != TOK_IDENTIFIER &&
			token != TOK_STRING && token != TOK_NUMBER) {
		    call syserr (SYS_FXFKSSYN)
		} else {
		    call he_ks_val (Memc[ln], param, hc)
		    ip = jp
	        }
	    } else if (Memc[ln] == '+' || Memc[ln] == '-') {
		call he_ks_pm (Memc[ln], param, hc)
		ip = jp
	    }
	}

	call sfree (sp)
end


# FXF_KS_VALUE -- Returns the value of a parameter in the kernel section.

procedure he_ks_val (outstr, param, hc)

char	outstr[ARB] 		#I Input string with value		
int	param			#I Parameter code
pointer hc			#U Fits kernel descriptor

int     ival
int	strcmp()
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
	case  HS_FIELD:
	    call strcpy (outstr, HFIELD(hc), LEN_CARD)
	case  HS_VALUE:
	    call strcpy (outstr, HVALUE(hc), LEN_CARD)
	case HS_COMMENT:
            call strcpy (outstr, HCOMMENT(hc), LEN_CARD)
	case HS_BEFORE:
	    HBAF(hc) = BEFORE
            call strcpy (outstr, HBAFVALUE(hc), LEN_CARD)
	case HS_AFTER:
	    HBAF(hc) = AFTER
            call strcpy (outstr, HBAFVALUE(hc), LEN_CARD)
	case HS_ADD:
	    HADD(hc) = ival
	case HS_ADDONLY:
	    HADDONLY(hc) = ival
	case HS_UPDATE:
	    HUPDATE(hc) = ival
	case HS_VERIFY:
	    HVERIFY(hc) = ival
	case HS_SHOW:
	    HSHOW(hc) = ival
	case HS_DELETE:
	    HDELETE(hc) = ival
	case HS_RENAME:
	    HRENAME(hc) = ival
	default:
            call syserr (SYS_FXFKSSYN)
	}
end


# HE_KS_PM -- Return the character YES or NO based on the value '+' or '-'
 
procedure he_ks_pm (pm, param, hc)

char	pm[1]		#I contains "+" or "-"
int	param		#I Parameter code
pointer hc		#U Fits kernel descriptor

int	ival
errchk	syserr, syserrs

begin
	if (pm[1] == '+') 
	   ival = YES
	else
	   ival = NO

	switch (param) {
	case HS_ADD:
	    HADD(hc) = ival
	case HS_ADDONLY:
	    HADDONLY(hc) = ival
	case HS_UPDATE:
	    HUPDATE(hc) = ival
	case HS_VERIFY:
	    HVERIFY(hc) = ival
	case HS_SHOW:
	    HSHOW(hc) = ival
	case HS_DELETE:
	    HDELETE(hc) = ival
	case HS_RENAME:
	    HRENAME(hc) = ival
	default:
	    call error(13, "ks_pm: invalid value")
	}
end


# FXF_KSINIT -- Initialize default values for ks parameters.

procedure he_ksinit (hc)

pointer	hc			#I 

begin
	HADD(hc)	= -1
	HADDONLY(hc)    = -1
	HDELETE(hc)     = -1
	HRENAME(hc)     = -1
	HUPDATE(hc)     = -1
	HVERIFY(hc)     = -1
	HSHOW(hc)       = -1
end
