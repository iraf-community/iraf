include "tbtables.h"
include "tblfits.h"		# for FITS_END_OF_FILE

# tbffnd -- find table in FITS file
# This routine finds an existing table in a FITS file.
# The function value will be the extension number (0 = primary header)
# if the table was found; otherwise, EOF will be returned.
#
# The extension may have been specified by extension number, name (EXTNAME),
# version number (EXTVER), or some combination.  Not specifying any extension
# at all is interpreted to mean extension number one (the first after the
# primary header/data unit).
#
# Phil Hodge,  2-Feb-1996  Subroutine created
# Phil Hodge, 20-May-1996  If no extension specified, only check first one.
# Phil Hodge,  1-Jun-1999  Use TB_FILE instead of fd.
# Phil Hodge, 26-Apr-2002  Allow opening any type of extension.

int procedure tbffnd (tp, extname, maxch, extver, hdutype)

pointer tp		# i: pointer to table descriptor
char	extname[maxch]	# o: actual EXTNAME read from header, or ""
int	maxch		# i: size of extname
int	extver		# o: EXTVER from header, or -1
int	hdutype		# o: type of HDU
#--
pointer sp
pointer comment		# for comment for keyword or for error message
pointer t_extname	# for extension name in table header
pointer u_extname	# for user-specified extension name
pointer save		# a copy of EXTNAME from header, to preserve case
int	hdu		# HDU number in user-interface numbering convention
int	t_extver	# extension version number from table header
int	u_extver	# user-specified extension version number
int	t_hdutype	# type of current HDU in file
int	status		# zero is OK
bool	foundit		# true if we have found the table in the file
bool	streq(), strne()
errchk	tbferr

begin
	extname[1] = EOS			# initial values
	extver = -1
	status = 0

	hdu = TB_HDU(tp)

	# Was no extension specified by number, version, or name?
	# If so, assume the first extension was intended.
	if (TB_HDU(tp) < 0 && TB_EXTVER(tp) < 0 && streq (TB_EXTNAME(tp), ""))
	    hdu = 1				# first after primary HDU

	if (hdu == 0) {

	    # Move to the primary HDU (should be there already, actually).
	    call fsmahd (TB_FILE(tp), hdu+1, t_hdutype, status)
	    if (status != 0)
		call tbferr (status)

	    hdutype = t_hdutype			# note:  not a table!

	    return (0)
	}

	call smark (sp)
	call salloc (t_extname, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	if (hdu > 0) {

	    # The extension was specified by number.

	    # Go to the specified HDU number.  (Add one to the HDU number to
	    # conform to the numbering convention in the FITSIO interface.)
	    call fsmahd (TB_FILE(tp), hdu+1, t_hdutype, status)
	    if (status != 0)
		call tbferr (status)

	    # Get EXTNAME from header, if it is present.
	    call fsgkys (TB_FILE(tp), "EXTNAME", Memc[t_extname],
			Memc[comment], status)
	    if (status == 0) {
		call strcpy (Memc[t_extname], extname, maxch)
	    } else {
		status = 0
		call ftcmsg()
	    }

	    # Get EXTVER from header, if present.
	    call fsgkyj (TB_FILE(tp), "EXTVER", t_extver, Memc[comment], status)
	    if (status == 0) {
		extver = t_extver
	    } else {
		status = 0
		call ftcmsg()
	    }

	    hdutype = t_hdutype

	    call sfree (sp)
	    return (hdu)
	}

	# Search for the table that matches whatever was specified.

	call salloc (u_extname, SZ_LINE, TY_CHAR)
	call salloc (save, SZ_LINE, TY_CHAR)

	# User-specified values.
	call strcpy (TB_EXTNAME(tp), Memc[u_extname], SZ_LINE)
	call strlwr (Memc[u_extname])	# for case insensitive comparison
	u_extver = TB_EXTVER(tp)

	# Assign initial values, in case we're not searching on these.
	Memc[save] = EOS
	t_extver = -1

	hdu = 0					# incremented in loop
	foundit = false
	while (!foundit) {

	    # Move forward one HDU.
	    hdu = hdu + 1
	    call fsmahd (TB_FILE(tp), hdu+1, t_hdutype, status)
	    if (status == FITS_END_OF_FILE) {
		call ftcmsg()
		# The table was not found.
		call sfree (sp)
		return (EOF)
	    } else if (status != 0) {		# some other error
		call tbferr (status)
	    }

	    # Is this the right extension?

	    if (Memc[u_extname] != EOS) {

		# EXTNAME was specified.
		call fsgkys (TB_FILE(tp), "EXTNAME", Memc[t_extname],
			Memc[comment], status)
		if (status != 0) {	# EXTNAME not found.
		    status = 0
		    call ftcmsg()
		    next
		} else {
		    # Does EXTNAME match?  (case insensitive comparison)
		    # Save t_extname to preserve case.
		    call strcpy (Memc[t_extname], Memc[save], SZ_LINE)
		    call strlwr (Memc[t_extname])
		    if (strne (Memc[t_extname], Memc[u_extname]))
			next			# EXTNAME does not match
		}
	    }

	    if (u_extver > 0) {

		# EXTVER was specified.
		call fsgkyj (TB_FILE(tp), "EXTVER", t_extver,
			Memc[comment], status)
		if (status != 0) {
		    # EXTVER not found in current header.
		    status = 0
		    call ftcmsg()
		    next
		} else if (t_extver != u_extver) {
		    next		# EXTVER does not match
		}
	    }

	    # We get here only if all tests succeeded.
	    # Save info that we read from the header.
	    call strcpy (Memc[save], extname, maxch)
	    extver = t_extver
	    hdutype = t_hdutype
	    foundit = true
	}

	call sfree (sp)
	return (hdu)		# user-interface numbering convention
end
