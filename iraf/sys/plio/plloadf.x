# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plset.h>
include	<plio.h>

# PL_LOADF -- Load a mask stored in external format in a binary file.  This
# simple code permits only one mask per file; more sophisticated storage
# facilities are planned; these will probably obsolete this routine.

procedure pl_loadf (pl, mask, title, maxch)

pointer	pl			#I mask descriptor
char	mask[ARB]		#I mask file
char	title[maxch]		#O mask title
int	maxch			#I max chars out

int	fd, nchars
pointer	sp, bp, sv, text, fname, extn
int	open(), read(), miireadc(), miireadi(), fnextn()
errchk	open, read, syserrs
define	err_ 91

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	# Get mask file name.
	call strcpy (mask, Memc[fname], SZ_PATHNAME)
	if (fnextn (mask, Memc[extn], SZ_FNAME) <= 0)
	    call strcat (".pl", Memc[fname], SZ_PATHNAME)

	# Open the mask save file.
	fd = open (Memc[fname], READ_ONLY, BINARY_FILE)

	# Get savefile header.
	call salloc (sv, LEN_SVDES, TY_STRUCT)
	if (miireadi (fd, Memi[sv], LEN_SVDES) != LEN_SVDES)
	    goto err_

	# Verify file type.
	if (SV_MAGIC(sv) != PLIO_SVMAGIC)
	    goto err_

	# Get descriptive text.
	call salloc (text, SV_TITLELEN(sv), TY_CHAR)
	if (miireadc (fd, Memc[text], SV_TITLELEN(sv)) != SV_TITLELEN(sv))
	    goto err_
	else
	    call strcpy (Memc[text], title, maxch)

	# Get encoded mask.
	call salloc (bp, SV_MASKLEN(sv), TY_SHORT)
	iferr (nchars = read (fd, Mems[bp], SV_MASKLEN(sv) * SZ_SHORT))
	    goto err_
	call close (fd)

	iferr (call pl_load (pl, bp))
	    goto err_

	call sfree (sp)
	return
err_
	call close (fd)
	call syserrs (SYS_PLBADSAVEF, Memc[fname])
end
