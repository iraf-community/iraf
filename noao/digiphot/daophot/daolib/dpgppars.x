include <ctotok.h>
include "../lib/daophotdef.h"

# DP_GPPARS -- Procedure to fetch the daophot task parameters.

procedure dp_gppars (dao)

pointer	dao		# pointer to daophot structure

int	dp, dap
pointer	mp, str, tstr
real	scale, fwhmpsf, psfrad, matchrad, fitrad, annulus, dannulus, mergerad

bool	clgetb(), clgpsetb()
int	clgpseti(), btoi(), dp_fctdecode(), dp_strwrd()
pointer	clopset()
real	clgpsetr()

begin
	# Allocate working space.
	call smark (mp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (tstr, SZ_FNAME, TY_CHAR)

	# Open the daophot structure.
	call dp_init (dao)

	# Set the package parameter text and initialize the verbose switch.
	call dp_seti (dao, TEXT, btoi (clgetb ("text")))
	call dp_seti (dao, VERBOSE, btoi (false))

	# Open the datapars parameter set.
	dp = clopset ("datapars")

	# Set the datapars parameters.
	scale = clgpsetr (dp, "scale")
	call dp_setr (dao, SCALE, scale)
	fwhmpsf = clgpsetr (dp, "fwhmpsf")
	call dp_setr (dao, SFWHMPSF, fwhmpsf)
	call dp_setr (dao, MAXGDATA, clgpsetr (dp, "datamax"))
	call dp_setr (dao, MINGDATA, clgpsetr (dp, "datamin"))

	# Initialize the noise parameters.
	call clgpset (dp, "ccdread", Memc[str], SZ_FNAME)
	call dp_sets (dao, CCDREAD, Memc[str])
	call dp_setr (dao, READNOISE, clgpsetr (dp, "readnoise"))
	call clgpset (dp, "gain", Memc[str], SZ_FNAME)
	call dp_sets (dao, CCDGAIN, Memc[str])
	call dp_setr (dao, PHOTADU, clgpsetr (dp, "epadu"))

	# Initialize the observing parameters. Note that whitespace
	# is removed from the filter id.
	call clgpset (dp, "exposure", Memc[str], SZ_FNAME)
	call dp_sets (dao, EXPTIME, Memc[str])
	call dp_setr (dao, ITIME, 1.0)
	call clgpset (dp, "airmass", Memc[str], SZ_FNAME)
	call dp_sets (dao, AIRMASS, Memc[str])
	call dp_setr (dao, XAIRMASS, clgpsetr (dp, "xairmass"))
	call clgpset (dp, "filter", Memc[str], SZ_FNAME)
	call dp_sets (dao, FILTER, Memc[str])
	call clgpset (dp, "ifilter", Memc[str], SZ_FNAME)
	call dp_rmwhite (Memc[str], Memc[str], SZ_FNAME)
	call dp_sets (dao, IFILTER, Memc[str])
	call clgpset (dp, "obstime", Memc[str], SZ_FNAME)
	call dp_sets (dao, OBSTIME, Memc[str])
	call clgpset (dp, "otime", Memc[str], SZ_FNAME)
	call dp_sets (dao, OTIME, Memc[str])

	# Close the datapars parameter set.
	call clcpset (dp)

	# Open the daopars parameter set.
	dap = clopset ("daopars")

	# Set the psf fitting parameters.
	call clgpset (dap, "function", Memc[tstr], SZ_FNAME)
	if (dp_fctdecode (Memc[tstr], Memc[str], SZ_FNAME) <= 0)
	    call strcpy (",gauss,", Memc[str], SZ_FNAME)
	call dp_sets (dao, FUNCLIST, Memc[str])
	if (dp_strwrd (1, Memc[tstr], SZ_FNAME, Memc[str]) <= 0)
	    call strcpy ("gauss", Memc[tstr], SZ_FNAME)
	call dp_sets (dao, FUNCTION, Memc[tstr])
	call dp_seti (dao, VARORDER, clgpseti (dap, "varorder"))
	#call dp_seti (dao, FEXPAND, btoi (clgpsetb (dap, "fexpand")))
	call dp_seti (dao, FEXPAND, NO)
	call dp_seti (dao, NCLEAN, clgpseti (dap, "nclean"))
	call dp_seti (dao, SATURATED, btoi (clgpsetb (dap, "saturated")))
	psfrad = clgpsetr (dap, "psfrad")
	call dp_setr (dao, RPSFRAD, psfrad)
	call dp_setr (dao, SPSFRAD, psfrad)
	matchrad = clgpsetr (dap, "matchrad")
	call dp_setr (dao, SMATCHRAD, matchrad)

	# Set the fitting parameters.
	fitrad = clgpsetr (dap, "fitrad")
	call dp_setr (dao, SFITRAD, fitrad)
	annulus = clgpsetr (dap, "sannulus")
	call dp_setr (dao, SANNULUS, annulus)
	dannulus = clgpsetr (dap, "wsannulus")
	call dp_setr (dao, SDANNULUS, dannulus)
	call dp_setr (dao, CRITSNRATIO, clgpsetr (dap, "critsnratio"))
	call dp_seti (dao, MAXITER, clgpseti (dap, "maxiter"))
	call dp_seti (dao, MAXGROUP, clgpseti (dap, "maxgroup"))
	call dp_seti (dao, MAXNSTAR, clgpseti (dap, "maxnstar"))
	call dp_seti (dao, RECENTER, btoi (clgpsetb (dap, "recenter")))
	call dp_seti (dao, FITSKY, btoi (clgpsetb (dap, "fitsky")))
	call dp_seti (dao, GROUPSKY, btoi (clgpsetb (dap, "groupsky")))
	call dp_setr (dao, FLATERR, clgpsetr (dap, "flaterr"))
	call dp_setr (dao, PROFERR, clgpsetr (dap, "proferr"))
	call dp_setr (dao, CLIPRANGE, clgpsetr (dap, "cliprange"))
	call dp_seti (dao, CLIPEXP, clgpseti (dap, "clipexp"))
	mergerad = clgpsetr (dap, "mergerad")
	call dp_setr (dao, SMERGERAD, mergerad)

	# Close the daopars pset file.
	call clcpset (dap)

	# Compute the fwhmpsf, psf radius, fitting radius and matching radius
	# in pixels and store.

	call dp_setr (dao, FWHMPSF, fwhmpsf / scale)
	call dp_setr (dao, PSFRAD, psfrad / scale)
	call dp_setr (dao, MATCHRAD, matchrad / scale)
	call dp_setr (dao, FITRAD, fitrad / scale)
	call dp_setr (dao, ANNULUS, annulus / scale)
	call dp_setr (dao, DANNULUS, dannulus / scale)
	if (IS_INDEFR(mergerad))
	    call dp_setr (dao, MERGERAD, INDEFR)
	else
	    call dp_setr (dao, MERGERAD, mergerad / scale)

	call sfree (mp)
end


# DP_FCTDECODE -- Decode and re-encode the list of analytic functions to be
# fit in a from suitable for use by strdic. If no valid psf types are included
# in the list set the dictionary to the gaussian function.

int procedure dp_fctdecode (instr, outstr, maxch)

char	instr[ARB]		# the input list of functions
char	outstr[ARB]		# the output list of functions
int	maxch			# maximum size of the output string

int	ip, op, ntok, tok
pointer	sp, token
int	ctotok(), strdic(), gstrcpy, gstrcat()

begin
	call smark (sp)
	call salloc (token, maxch, TY_CHAR)

	outstr[1] = ','
	outstr[2] = EOS
	op = 2

	ntok = 0
	ip = 1
	while (instr[ip] != EOS) {
	    tok = ctotok (instr, ip, Memc[token], maxch)
	    if (tok != TOK_IDENTIFIER)
		next
	    if (strdic (Memc[token], Memc[token], maxch, FCTN_FTYPES) <= 0)
		next
	    ntok = ntok + 1
	    op = op + gstrcpy (Memc[token], outstr[op], maxch - op + 1)
	    op = op + gstrcat (",", outstr[op], maxch - op + 1)
	}

	call sfree (sp)

	return (ntok)
end


# DP_STRWRD -- Search a dictionary string for a given string index number.
# This is the opposite function of strdic(), that returns the index for
# given string.  The entries in the dictionary string are separated by
# a delimiter character which is the first character of the dictionary
# string.  The index of the string found is returned as the function value.
# Otherwise, if there is no string for that index, a zero is returned.

int procedure dp_strwrd (index, outstr, maxch, dict)

int	index			# String index
char	outstr[ARB]		# Output string as found in dictionary
int	maxch			# Maximum length of output string
char	dict[ARB]		# Dictionary string

int	i, len, start, count

int	strlen()

begin
	# Clear output string
	outstr[1] = EOS

	# Return if the dictionary is not long enough
	if (dict[1] == EOS)
	    return (0)

	# Initialize counters
	count = 1
	len   = strlen (dict)

	# Search the dictionary string. This loop only terminates
	# successfully if the index is found. Otherwise the procedure
	# returns with and error condition.
	for (start = 2; count < index; start = start + 1) {
	    if (dict[start] == dict[1])
		count = count + 1
	    if (start == len)
		return (0)
	}

	# Extract the output string from the dictionary
	for (i = start; dict[i] != EOS && dict[i] != dict[1]; i = i + 1) {
	    if (i - start + 1 > maxch)
		break
	    outstr[i - start + 1] = dict[i]
	}
	outstr[i - start + 1] = EOS

	# Return index for output string
	return (count)
end
