include <math/iminterp.h>

procedure apopset (pset)

char	pset[ARB]		# Pset name
pointer	pp, clopset ()
common	/apparam/ pp

begin
	pp = clopset (pset)
end


procedure apcpset ()

pointer	pp
common	/apparam/ pp

begin
	call clcpset (pp)
end


procedure apgstr (param, str, maxchar)

char	param[ARB]		# Parameter name
char	str[ARB]		# String to return
int	maxchar			# Maximum length of string

pointer	pp
common	/apparam/ pp

begin
	call clgpset (pp, param, str, maxchar)
end


bool procedure apgetb (param)

char	param[ARB]		# Parameter name
bool	clgpsetb()
pointer	pp
common	/apparam/ pp

begin
	return (clgpsetb (pp, param))
end


int procedure apgeti (param)

char	param[ARB]		# Parameter name
int	clgpseti()
pointer	pp
common	/apparam/ pp

begin
	return (clgpseti (pp, param))
end


real procedure apgetr (param)

char	param[ARB]		# Parameter name
real	clgpsetr()
pointer	pp
common	/apparam/ pp

begin
	return (clgpsetr (pp, param))
end


real procedure apgimr (param, im)

char	param[ARB]		# Parameter name
pointer	im			# IMIO pointer
int	i, ctor()
pointer	pp, sp, str
real	rval, imgetr()
common	/apparam/ pp
errchk	imgetr

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call clgpset (pp, param, Memc[str], SZ_FNAME)
	i = 1
	if (ctor (Mems[str], i, rval) == 0)
	    rval = imgetr (im, Memc[str])
	call sfree (sp)
	return (rval)
end


int procedure apgwrd (param, keyword, maxchar, dictionary)

char	param[ARB]		# CL parameter string
char	keyword[ARB]		# String matched in dictionary
int	maxchar			# Maximum size of str
char	dictionary[ARB]		# Dictionary string

int	i, strdic()
pointer	pp
common	/apparam/ pp

begin
	call clgpset (pp, param, keyword, maxchar)
	i = strdic (keyword, keyword, maxchar, dictionary)
	if (i <= 0)
	    call error (1, "Ambiguous or unknown parameter value")
	return (i)
end


# APGINTERP -- Select an interpolator from a CL input string.  The procedure
# is coded to be protected from changes in the values of the interpolator
# types in interpdef.h.

int procedure apginterp (param)

char	param[ARB]		# CL parameter prompt string
int	index, iicodes[5]
pointer	sp, word
int	apgwrd()
errchk	apgwrd
data	iicodes /II_NEAREST, II_LINEAR, II_POLY3, II_POLY5, II_SPLINE3/

pointer	pp
common	/apparam/ pp

begin
	call smark (sp)
	call salloc (word, SZ_FNAME, TY_CHAR)

	index = max (1, min (5, apgwrd (param, Memc[word], SZ_FNAME,
	    "|nearest|linear|poly3|poly5|spline3|")))

	call sfree (sp)
	return (iicodes[index])
end


procedure appstr (param, str)

char	param[ARB]		# Parameter name
char	str[ARB]		# String to be put
pointer	pp, sp, str1, str2
common	/apparam/ pp

int	i, strmatch(), stridxs()

begin
	if (strmatch (param, "p_") == 0) {
	    call smark (sp)
	    call salloc (str1, SZ_FNAME, TY_CHAR)
	    call salloc (str2, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[str1], SZ_FNAME, "%s.p_prompt")
		call pargstr (param)
	    call clgpset (pp, Memc[str1], Memc[str2], SZ_LINE)
	    if (Memc[str2] == '>') {
		i = stridxs (" \\\t\n", Memc[str2])
		if (i > 0)
		    Memc[str2+i-1] = EOS
		call clpstr (Memc[str2+1], str)
	    } else
		call clppset (pp, param, str)
	    call sfree (sp)
	} else
	    call clppset (pp, param, str)
end


procedure apputb (param, bval)

char	param[ARB]		# Parameter name
bool	bval			# Value to be put
pointer	pp, sp, str1, str2
common	/apparam/ pp

int	i, strmatch(), stridxs()

begin
	if (strmatch (param, "p_") == 0) {
	    call smark (sp)
	    call salloc (str1, SZ_FNAME, TY_CHAR)
	    call salloc (str2, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[str1], SZ_FNAME, "%s.p_prompt")
		call pargstr (param)
	    call clgpset (pp, Memc[str1], Memc[str2], SZ_LINE)
	    if (Memc[str2] == '>') {
		i = stridxs (" \\\t\n", Memc[str2])
		if (i > 0)
		    Memc[str2+i-1] = EOS
		call clputb (Memc[str2+1], bval)
	    } else
		call clppsetb (pp, param, bval)
	    call sfree (sp)
	} else
	    call clppsetb (pp, param, bval)
end


procedure apputi (param, ival)

char	param[ARB]		# Parameter name
int	ival			# Value to be put
pointer	pp, sp, str1, str2
common	/apparam/ pp

int	i, strmatch(), stridxs()

begin
	if (strmatch (param, "p_") == 0) {
	    call smark (sp)
	    call salloc (str1, SZ_FNAME, TY_CHAR)
	    call salloc (str2, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[str1], SZ_FNAME, "%s.p_prompt")
		call pargstr (param)
	    call clgpset (pp, Memc[str1], Memc[str2], SZ_LINE)
	    if (Memc[str2] == '>') {
		i = stridxs (" \\\t\n", Memc[str2])
		if (i > 0)
		    Memc[str2+i-1] = EOS
		call clputi (Memc[str2+1], ival)
	    } else
		call clppseti (pp, param, ival)
	    call sfree (sp)
	} else
	    call clppseti (pp, param, ival)
end


procedure apputr (param, rval)

char	param[ARB]		# Parameter name
real	rval			# Value to be put
pointer	pp, sp, str1, str2
common	/apparam/ pp

int	i, strmatch(), stridxs()

begin
	if (strmatch (param, "p_") == 0) {
	    call smark (sp)
	    call salloc (str1, SZ_FNAME, TY_CHAR)
	    call salloc (str2, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[str1], SZ_FNAME, "%s.p_prompt")
		call pargstr (param)
	    call clgpset (pp, Memc[str1], Memc[str2], SZ_LINE)
	    if (Memc[str2] == '>') {
		i = stridxs (" \\\t\n", Memc[str2])
		if (i > 0)
		    Memc[str2+i-1] = EOS
		call clputr (Memc[str2+1], rval)
	    } else
		call clppsetr (pp, param, rval)
	    call sfree (sp)
	} else
	    call clppsetr (pp, param, rval)
end
