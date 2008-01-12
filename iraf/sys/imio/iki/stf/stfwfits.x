# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	<fio.h>
include	"stf.h"

# STF_WFITSHDR -- Update the FITS header file.  This is done by writing an
# entire new header file and then replacing the old header file with the
# new one.  This is necessary since the header file is a text file and text
# files cannot be randomly updated.

procedure stf_wfitshdr (im)

pointer	im			# image descriptor

pointer	sp, fname, lbuf, stf, pp
int	in, out, pn, junk, i, width

bool	fnullfile()
int	stropen(), open(), protect(), strlen() #ditto-dlb
errchk	fmkcopy, open, stropen, fcopyo, fprintf

begin
	if (fnullfile (IM_HDRFILE(im)))
	    return

	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	stf = IM_KDES(im)

	# Open a new header file with a unique, temporary name.  Make a copy
	# of the template file rather than of the old header file.  Since
	# we also block header lines out to 80 chars automatically, this
	# means that we can read any old text file but will always generate
	# a new header file of the standard type when the header is updated.

	call mktemp (IM_HDRFILE(im), Memc[fname], SZ_FNAME)
	call fmkcopy (HDR_TEMPLATE, Memc[fname])
	out = open (Memc[fname], APPEND, TEXT_FILE)

	# Write out the standard, reserved header parameters.

	call fprintf (out, "SIMPLE  =%21s /%81t\n")
	    call pargstr ("F")
	call fprintf (out, "BITPIX  =%21d /%81t\n")
	    call pargi (STF_BITPIX(stf))

	# We want to get the full string length or 8 characters, 
	# whichever is greater--6/25/87, dlb
	
	call fprintf (out, "DATATYPE= '%*.*s'%32t/%81t\n")
	    width = max(8, strlen(STF_DATATYPE(STF)))
	    call pargi (-width) # force left-justified field
	    call pargi (width)
	    call pargstr (STF_DATATYPE(stf))

	call fprintf (out, "NAXIS   =%21d /%81t\n")
	    call pargi (STF_NAXIS(stf))
	do i = 1, STF_NAXIS(stf) {
	    call fprintf (out, "NAXIS%d%9t=%21d /%81t\n")
		call pargi (i)
		call pargi (STF_LENAXIS(stf,i))
	}

	call fprintf (out, "GROUPS  =%21s /%81t\n")
	    if (STF_GROUPS(stf) == YES)
		call pargstr ("T")
	    else
		call pargstr ("F")

	# Changed order of the following three cards to conform
	# to SOGS expectations--dlb, 7/14/87
	# Only write group keywords if STF_GROUPS is YES (BPS 12.06.91)

	if (STF_GROUPS(stf) == YES) {
	    call fprintf (out, "GCOUNT  =%21d /%81t\n")
		call pargi (STF_GCOUNT(stf))
	    call fprintf (out, "PCOUNT  =%21d /%81t\n")
		call pargi (STF_PCOUNT(stf))
	    call fprintf (out, "PSIZE   =%21d /%81t\n")
		call pargi (STF_PSIZE(stf))
	}

	# Add cards defining the fields of the group parameter block.  Each
	# field requires three cards.

	for (pn=1;  pn <= STF_PCOUNT(stf);  pn=pn+1) {
	    pp = STF_PDES(stf,pn)

	    # PTYPE MUST be 8 characters or less.
	    call fprintf (out, "PTYPE%d%9t= '%-8.8s'%32t/%s%81t\n")
		call pargi (pn)
		call pargstr (P_PTYPE(pp))
		call pargstr (P_COMMENT(pp))

	    # Need width for string--6/26/87, dlb
	    call fprintf (out, "PDTYPE%d%9t= '%-*.*s'%32t/%81t\n")
		call pargi (pn)
		width = max (8, strlen(P_PDTYPE(pp)))
		call pargi (-width) # force left-justified field
		call pargi (width)
		call pargstr (P_PDTYPE(pp))

	    call fprintf (out, "PSIZE%d%9t=%21d /%81t\n")
		call pargi (pn)
		call pargi (P_PSIZE(pp))
	}

	# Add the contents of the IMIO user area, excluding the cards used
	# to represent GPB parameters.

	in = stropen (Memc[IM_USERAREA(im)], ARB, READ_ONLY)
	call stf_copyfits (stf, in, NULL, out)
	call close (in)

	# End of FITS header.
	call fprintf (out, "END%81t\n")
	call close (out)

	# Replace the original header file with the new one, even if the
	# original header is a protected file.  Transfer any file protection
	# to the new file.

	if (IM_HFD(im) != NULL)
	    call close (IM_HFD(im))

	if (protect (IM_HDRFILE(im), QUERY_PROTECTION) == YES) {
	    iferr (junk = protect (IM_HDRFILE(im), REMOVE_PROTECTION))
		call erract (EA_ERROR)
	    iferr (junk = protect (Memc[fname], SET_PROTECTION))
		call erract (EA_ERROR)
	}

	iferr (call delete (IM_HDRFILE(im)))
	    call erract (EA_ERROR)
	iferr (call rename (Memc[fname], IM_HDRFILE(im)))
	    call erract (EA_ERROR)

	if (IM_HFD(im) != NULL)
	    IM_HFD(im) = open (IM_HDRFILE(im), READ_ONLY, TEXT_FILE)

	call sfree (sp)
end
