# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	"fmio.h"

# FM_LFCOPY -- Copy an lfile, either to another lfile within the same
# datafile, or to another datafile.

procedure fm_lfcopy (old, o_lfile, new, n_lfile)

pointer	old, new		#I FMIO descriptors of source and destination
int	o_lfile, n_lfile	#I lfile numbers of source and dest. lfiles

long	offset
int	n_szbpage
pointer	sp, o_ft, n_ft, o_lf, n_lf, o_lfname, n_lfname, buf
int	maxpages, maxbytes, nbytes, type, nleft, status

errchk	fmio_errchk, fmio_bind, syserrs
define	olderr_ 91
define	newerr_ 92

begin
	# Verify input.
	if (o_lfile < 0 || o_lfile > FM_NLFILES(old))
	    goto olderr_
	if (n_lfile < 0 || n_lfile > FM_NLFILES(new))
	    goto newerr_

	# Compute some useful values.
	o_ft = FM_FTABLE(old)
	n_ft = FM_FTABLE(new)
	n_szbpage = FM_SZBPAGE(new)
	maxpages = FM_MAXBUFSIZE(new) / n_szbpage
	if (maxpages <= 0)
	    maxpages = DEF_BIGBUFNP
	maxbytes = n_szbpage * maxpages

	# Copy the lfile.
	o_lf = o_ft + o_lfile * LEN_FTE
	n_lf = n_ft + n_lfile * LEN_FTE

	# Skip empty or deleted lfiles.
	LF_FLAGS(n_lf) = and (LF_FLAGS(o_lf), not(LFF_DELETED))
	if (LF_FSIZE(o_lf) <= 0 || and(LF_FLAGS(o_lf),LFF_DELETED) != 0)
	    return

	# Get lfile type.
	type = BINARY_FILE
	if (and(LF_FLAGS(o_lf),LFF_TEXTFILE) != 0)
	    type = TEXT_FILE

	# Allocate buffers.
	call smark (sp)
	call salloc (o_lfname, SZ_FNAME, TY_CHAR)
	call salloc (n_lfname, SZ_FNAME, TY_CHAR)
	call salloc (buf, maxbytes / SZB_CHAR, TY_CHAR)

	# Open old lfile.
	call fm_lfname (old, o_lfile, type, Memc[o_lfname], SZ_FNAME)
	call strpak (Memc[o_lfname], Memc[o_lfname], SZ_FNAME)
	call fm_lfopen (Memc[o_lfname], READ_ONLY, o_lf)
	if (o_lf == ERR)
	    goto olderr_

	# Open new lfile.
	call fm_lfname (new, n_lfile, type, Memc[n_lfname], SZ_FNAME)
	call strpak (Memc[n_lfname], Memc[n_lfname], SZ_FNAME)
	call fm_lfopen (Memc[n_lfname], NEW_FILE, n_lf)
	if (n_lf == ERR)
	    goto newerr_

	# Copy the lfile data (as a binary file to avoid needless
	# character unpack/pack).

	nleft = LF_FSIZE(o_lf)
	for (offset=1;  nleft > 0;  offset=offset+nbytes) {
	    # Read a block of data.
	    call fm_lfbinread (o_lf, Memc[buf], maxbytes, offset)
	    call fm_lfbinwait (o_lf, nbytes)
	    if (nbytes == ERR)
		goto olderr_
	    else if (nbytes == 0)
		break

	    # Write to the new datafile.
	    call fm_lfbinwait (n_lf, status)
	    if (status == ERR)
		goto newerr_
	    call fm_lfbinwrite (n_lf, Memc[buf], nbytes, offset)
	    nleft = nleft - nbytes
	}

	# Wait for the last write to terminate.
	call fm_lfbinwait (n_lf, status)
	if (status == ERR)
	    goto newerr_

	# Close the lfiles.
	call fm_lfclose (o_lf, status)
	if (status == ERR)
	    goto olderr_
	call fm_lfclose (n_lf, status)
	if (status == ERR)
	    goto newerr_

	call fmio_errchk (old)
	call fmio_errchk (new)

	call sfree (sp)
	return

olderr_
	call syserrs (SYS_FMLFCOPY, FM_DFNAME(old))
newerr_
	call syserrs (SYS_FMLFCOPY, FM_DFNAME(new))
end
