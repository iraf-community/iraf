# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<syserr.h>
include	<error.h>
include	<imhdr.h>
include "fxf.h"

# FXF_DELETE -- Delete a FITS file.  NOTE: it is not possible to delete an
# individual extension at this time.

procedure fxf_delete (kernel, root, extn, status)

int	kernel			#I IKI kernel
char	root[ARB]		#I root filename
char	extn[ARB]		#I header file extension
int	status			#O status value

int	cindx
pointer	sp, fname, im, tmp
pointer	immapz()
bool    streq()

errchk  syserrs
include "fxfcache.com"

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
        call salloc (tmp, SZ_PATHNAME, TY_CHAR)

	call fxf_init()
	status = OK

	# Get the file extension if not given.
        if (extn[1] == EOS) {
	    call fxf_access (kernel, root, extn, READ_ONLY, status)
	    if (status == NO) {
		call sfree (sp)
		status = ERR
		return
	    }
	}

	call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)
	call strcpy (Memc[fname], Memc[tmp], SZ_PATHNAME)
	call strcat ("[0]", Memc[tmp], SZ_PATHNAME)
	iferr (im = immapz (Memc[tmp], READ_ONLY, 0))
	    call syserrs (SYS_FXFDELMEF, Memc[fname])
	else
	    call imunmap (im)

	iferr (call delete (Memc[fname]))
	    call erract (EA_WARN)

	# Remove the image from the FITS cache if found.
	do cindx=1, rf_cachesize {
	    if (rf_fit[cindx] == NULL)
		next
	    if (streq (Memc[fname], rf_fname[1,cindx])) {
		call mfree (rf_pextv[cindx], TY_INT)
		call mfree (rf_pextn[cindx], TY_CHAR)
		call mfree (rf_pixp[cindx], TY_INT)
		call mfree (rf_hdrp[cindx], TY_INT)
		call mfree (rf_fit[cindx], TY_STRUCT)
		call mfree (rf_hdr[cindx], TY_CHAR)
		rf_fit[cindx] = NULL
		rf_lru[cindx] = 0
		rf_fname[1,cindx] = EOS
	    }
	}

	status = OK
	call sfree (sp)
end
