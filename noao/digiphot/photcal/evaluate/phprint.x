include "../lib/parser.h"

# PH_MKPLIST -- Construct the list of variables to be printed.

int procedure ph_mkplist (plist, cmap, omap, nobsvars, psym, pcols,
	max_len_plist)

int	plist		# pointer to the list of variables
int	cmap		# catalog column to data column mapping
int	omap		# observations column to data column mapping
int	nobsvars	# number of observations variables
int	psym[ARB]	# the output array of variable symbols
int	pcols[ARB]	# offset in the data array of the symbol
int	max_len_plist	# the maximum length of the variables list

int	len_plist, sym, col
pointer	sp, pname
int	fntgfnb(), pr_getsym(), pr_gsymi(), pr_findmap1()

begin
	call smark (sp)
	call salloc (pname, SZ_FNAME, TY_CHAR)

	len_plist = 0
	while (fntgfnb (plist, Memc[pname], SZ_FNAME) != EOF) {
	    if (len_plist >= max_len_plist)
		break
	    sym = pr_getsym (Memc[pname])
	    if (IS_INDEFI(sym))
		next
	    switch (pr_gsymi (sym, PSYMTYPE)) {
	    case PTY_CATVAR:
		psym[len_plist+1] = sym
		col  = pr_gsymi (sym, PINPCOL)
		pcols[len_plist+1] = pr_findmap1 (cmap, col) + nobsvars 
	    case PTY_OBSVAR:
		psym[len_plist+1] = sym
		col  = pr_gsymi (sym, PINPCOL)
		pcols[len_plist+1] = pr_findmap1 (omap, col)
	    case PTY_SETEQ:
		psym[len_plist+1] = sym
		pcols[len_plist+1] = 0 
	    default:
		next
	    }
	    len_plist = len_plist + 1
	}

	call sfree (sp)
	return (len_plist)
end


# PH_OFORMAT -- Construct the output formatstr string.

procedure ph_oformatstr (getid, ncols, formatstr, maxch)

int	getid		# output the object id
int	ncols		# number of columns in the output text file
char	formatstr[ARB]	# the output formatstr string
int	maxch		# maximum number of characters in the formatstr string

int	i, fcol

begin
	if (getid == YES) {
	    call strcpy ("%-10s ", formatstr, maxch)
	    fcol = 2
	} else {
	    formatstr[1] = EOS
	    fcol = 1
	}

	do i = fcol, ncols {
	    if (i == ncols)
		call strcat ("%-7.3f\n", formatstr, maxch)
	    else
		call strcat ("%-7.3f ", formatstr, maxch)
	}
end


# PH_OFIELDS -- Count the number of fields in the formatstr string.

int procedure ph_ofields (formatstr)

char	formatstr[ARB]		# the input formatstr string

char	percent
int	ip, findex, nfields
int	stridx()
data	percent /'%'/

begin
	nfields = 0

	ip = 1
	while (formatstr[ip] != EOS) {
	    findex = stridx (percent, formatstr[ip])
	    if (findex == 0)
		break
	    ip = findex + ip
	    if (formatstr[ip] == percent)
		ip = ip + 1
	    else
	        nfields = nfields + 1
	}

	return (nfields)
end
