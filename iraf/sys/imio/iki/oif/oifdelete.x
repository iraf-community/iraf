# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<protect.h>
include	<error.h>
include	<imhdr.h>

# OIF_DELETE -- Delete an image.  A special operator is required since the
# image is stored as two files.

procedure oif_delete (kernel, root, extn, status)

int	kernel			#I IKI kernel
char	root[ARB]		#I root filename
char	extn[ARB]		#U extension
int	status

int	junk
pointer	sp, fname, pixfile
int	access(), protect()
pointer	im, immapz()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (pixfile, SZ_PATHNAME, TY_CHAR)

	# Generate filename.
	call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)

	iferr (im = immapz (Memc[fname], READ_ONLY, 0)) {
	    call erract (EA_WARN)

	} else {
	    if (IM_PIXFILE(im) != EOS) {
		call oif_gpixfname (IM_PIXFILE(im), IM_HDRFILE(im),
		    Memc[pixfile], SZ_PATHNAME)
		if (access (Memc[pixfile],0,0) == YES)
		    iferr (call delete (Memc[pixfile]))
			call erract (EA_WARN)
	    }

	    call imunmap (im)

	    # Do not complain if the file is not protected.
	    iferr (junk = protect (Memc[fname], REMOVE_PROTECTION))
		;
	    iferr (call delete (Memc[fname]))
		call erract (EA_WARN)
	}

	call sfree (sp)
	status = OK
end
