include <imhdr.h>
include <fset.h>
include "../lib/daophotdef.h"
include "../lib/psfdef.h"


# DP_OPPSF -- Open the current psf image and the psf group file.

procedure dp_oppsf (dao, psfim, opst, psfgr)

pointer	dao			# pointer to the daophot structure
pointer	psfim			# pointer to the psf image
int	opst			# the psf star list descriptor
int	psfgr			# the psf group file descriptor

pointer	sp, str
int	open(), dp_stati(), dp_pstati()
pointer	immap(), tbtopn()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Reopen the PSF star and group files.
	call dp_stats (dao, OUTREJFILE, Memc[str], SZ_FNAME)
	if (dp_stati (dao, TEXT) == YES)
	    opst = open (Memc[str], NEW_FILE, TEXT_FILE)
	else
	    opst = tbtopn (Memc[str], NEW_FILE, 0)
	call dp_stats (dao, OUTPHOTFILE, Memc[str], SZ_FNAME)
	if (dp_stati (dao, TEXT) == YES)
	    psfgr = open (Memc[str], NEW_FILE, TEXT_FILE)
	else
	    psfgr = tbtopn (Memc[str], NEW_FILE, 0)

	# Reopen the psf image.
	call dp_stats (dao, PSFIMAGE, Memc[str], SZ_FNAME)
	psfim = immap (Memc[str], NEW_IMAGE, dp_pstati (dao, LENUSERAREA))

	call sfree (sp)
end


# DP_UPDATEPSF -- Update the psf on disk.

bool procedure dp_updatepsf (dao, im, psfim, opst, psfgr, psf_new, psf_current,
	psf_written)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
pointer	psfim			# pointer to the psf image
int	opst			# the psf star list file descriptor
int	psfgr			# the psf group file descriptor
bool	psf_new			# is the psf star list defined ?
bool	psf_current		# is the psf fit uptodate
bool	psf_written		# has the psf been saved on disk

bool	update
pointer	sp, str
int	dp_fitpsf()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	update = false
	if (psfim == NULL) {
	    call printf ("Warning: The PSF image is undefined\n")
	} else if (psfgr == NULL) {
	    call printf ("Warning: The PSF group file is undefined\n")
	} else if (psf_new) {
	    call printf ("Warning: The PSF star list is undefined\n")
	} else if (! psf_current) {
	    if (dp_fitpsf (dao, im, Memc[str], SZ_LINE) == OK) {
		call dp_writepsf (dao, im, psfim)
		call dp_wplist (dao, im, opst)
		call dp_wneistars (dao, im, psfgr)
		update = true
	    } else {
		call printf ("%s\n")
		    call pargstr (Memc[str])
	    }
	} else if (! psf_written) {
	    call dp_writepsf (dao, im, psfim)
	    call dp_wplist (dao, im, opst)
	    call dp_wneistars (dao, im, psfgr)
	    update = true
	}

	if (DP_VERBOSE(dao) == YES && update) {
	    call printf ("\nWriting PSF image %s\n")
		call pargstr (IM_HDRFILE (psfim))
	    call dp_stats (dao, OUTREJFILE, Memc[str], SZ_FNAME)
	    call printf ("Writing output PSF star list %s\n")
		call pargstr (Memc[str])
	    call dp_stats (dao, OUTPHOTFILE, Memc[str], SZ_FNAME)
	    call printf ("Writing output PSF star group file %s\n")
		call pargstr (Memc[str])
	}

	call sfree (sp)

	return (update)
end


# DP_RMPSF -- Close and delete the psf image and the psf group file.

procedure dp_rmpsf (dao, psfim, opst, psfgr)

pointer	dao			# pointer to the daophot structure
pointer	psfim			# pointer to the psf image
int	opst			# the psf star list file descriptor
int	psfgr			# the psf group file descriptor

pointer	sp, temp
int	dp_stati(), access()

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)

	# Delete the psf star file. The access check is necessary because
	# an empty tables file is automatically deleted by the system.
	if (dp_stati (dao, TEXT) == YES) {
	    call fstats (opst, F_FILENAME, Memc[temp], SZ_FNAME)
	    call close (opst)
	} else {
	    call tbtnam (opst, Memc[temp], SZ_FNAME)
	    call tbtclo (opst)
	}
	if (access (Memc[temp], 0, 0) == YES)
	    call delete (Memc[temp])
	opst = NULL

	# Delete the neighbours file. The access check is necessary because
	# an empty tables file is automatically deleted by the system.
	if (dp_stati (dao, TEXT) == YES) {
	    call fstats (psfgr, F_FILENAME, Memc[temp], SZ_FNAME)
	    call close (psfgr)
	} else {
	    call tbtnam (psfgr, Memc[temp], SZ_FNAME)
	    call tbtclo (psfgr)
	}
	if (access (Memc[temp], 0, 0) == YES)
	    call delete (Memc[temp])
	psfgr = NULL

	# Delete the PSF image.
	call strcpy (IM_HDRFILE(psfim), Memc[temp], SZ_FNAME)
	call imunmap (psfim)
	call imdelete (Memc[temp])
	psfim = NULL

	call sfree (sp)
end
