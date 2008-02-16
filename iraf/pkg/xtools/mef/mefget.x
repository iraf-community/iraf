# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<pkg/mef.h>

# MEFGETB -- Get an image header parameter of type boolean.  False is returned
# if the parameter cannot be found or if the value is not true.

bool procedure mefgetb (mef, key)

pointer	mef			# image descriptor
char	key[ARB]		# parameter to be returned

pointer	sp, kv, line
int	strlen()
bool    bval

errchk mef_findkw

begin
	call smark (sp)
	call salloc (kv, LEN_CARD, TY_CHAR)
	call salloc (line, LEN_CARD, TY_CHAR)
	
	call mef_findkw (MEF_HDRP(mef), key, Memc[kv])
	if (strlen(Memc[kv]) != 1) {
	    call sprintf(Memc[line], LEN_CARD, "Invalid boolean value: '%s'")
		 call pargstr (Memc[kv])
	    call error (0,Memc[line]) 
	}else
	    bval = Memc[kv] == 'T'

	call sfree (sp)
	return (bval)
end


# MEFGETC -- Get an image header parameter of type char.

char procedure mefgetc (mef, key)

pointer	mef			# image descriptor
char	key[ARB]		# parameter to be returned
long	mefgetl()

begin
	return (mefgetl (mef, key))
end


# MEFGETD -- Get an image header parameter of type double floating.  If the
# named parameter is a standard parameter return the value directly,
# else scan the user area for the named parameter and decode the value.

double procedure mefgetd (mef, key)

pointer	mef			# image descriptor
char	key[ARB]		# parameter to be returned

int	ip
double	dval
pointer	sp, sval
int	ctod()
errchk	syserrs, mefgstr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)

	ip = 1
	call mefgstr (mef, key, Memc[sval], SZ_LINE)
	if(Memc[sval]==EOS)
	    call syserrs (SYS_IDBKEYNF, key)
	if (ctod (Memc[sval], ip, dval) == 0)
	    call syserrs (SYS_IDBTYPE, key)

	call sfree (sp)
	return (dval)
end


# MEFGETI -- Get an image header parameter of type integer.

int procedure mefgeti (mef, key)

pointer	mef			# image descriptor
char	key[ARB]		# parameter to be returned

long	lval, mefgetl()
errchk	mefgetl

begin
	lval = mefgetl (mef, key)
	if (IS_INDEFL(lval))
	    return (INDEFI)
	else
	    return (lval)
end


# MEFGETL -- Get an image header parameter of type long integer.

long procedure mefgetl (mef, key)

pointer	mef			# image descriptor
char	key[ARB]		# parameter to be returned

double	dval, mefgetd()
errchk	mefgetd

begin
	dval = mefgetd (mef, key)
	if (IS_INDEFD(dval))
	    return (INDEFL)
	else
	    return (nint (dval))
end


# MEFGETR -- Get an image header parameter of type real.

real procedure mefgetr (mef, key)

pointer	mef			# image descriptor
char	key[ARB]		# parameter to be returned

double	dval, mefgetd()
errchk	mefgetd

begin
	dval = mefgetd (mef, key)
	if (IS_INDEFD(dval))
	    return (INDEFR)
	else
	    return (dval)
end


# MEFGETS -- Get an image header parameter of type short integer.

short procedure mefgets (mef, key)

pointer	mef			# image descriptor
char	key[ARB]		# parameter to be returned

long	lval, mefgetl()
errchk	mefgetl

begin
	lval = mefgetl (mef, key)
	if (IS_INDEFL(lval))
	    return (INDEFS)
	else
	    return (lval)
end


# MEFGSTR -- Get an image header parameter of type string.  If the named
# parameter is a standard parameter return the value directly, else scan
# the user area for the named parameter and decode the value.

procedure mefgstr (mef, key, outstr, maxch)

pointer	mef			# image descriptor
char	key[ARB]		# parameter to be returned
char	outstr[ARB]		# output string to receive parameter value
int	maxch

pointer	sp, kv

begin
	call smark (sp)
	call salloc (kv, LEN_CARD, TY_CHAR)

	# Find the record.
	iferr (call mef_findkw (MEF_HDRP(mef), key, Memc[kv]))
	    Memc[kv] = EOS

	call strcpy (Memc[kv], outstr, min (maxch, LEN_CARD))

	call sfree (sp)
end
