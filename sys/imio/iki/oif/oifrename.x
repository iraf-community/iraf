# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	"oif.h"

# OIF_RENAME -- Rename an image.  A special operator is required since the image
# is stored as two files.

procedure oif_rename (kernel, old_root, old_extn, new_root, new_extn, status)

int	kernel			#I IKI kernel
char	old_root[ARB]		# old image root name
char	old_extn[ARB]		# old image extn
char	new_root[ARB]		# new image root name
char	new_extn[ARB]		# old image extn
int	status

pointer	sp, im
bool	heq, peq
pointer	old_hfn, new_hfn
pointer	old_pfn, new_pfn
int	nchars, old_rootoff, new_rootoff, junk

bool	streq()
pointer	immapz()
int	access(), strlen(), strncmp()
errchk	immapz, rename

begin
	call smark (sp)
	call salloc (old_hfn, SZ_PATHNAME, TY_CHAR)
	call salloc (new_hfn, SZ_PATHNAME, TY_CHAR)
	call salloc (old_pfn, SZ_PATHNAME, TY_CHAR)
	call salloc (new_pfn, SZ_PATHNAME, TY_CHAR)

	# Get filenames of old and new images.
	call iki_mkfname (old_root, old_extn, Memc[old_hfn], SZ_PATHNAME)
	call iki_mkfname (new_root, OIF_HDREXTN, Memc[new_hfn], SZ_PATHNAME)
	heq = streq (Memc[old_hfn], Memc[new_hfn])

	# Our task here is nontrivial as the pixel file must be renamed as
	# well as the header file, e.g., since renaming the header file may
	# move it to a different directory, and the PIXFILE field in the
	# image header may indicate that the pixel file is in the same dir
	# as the header.  Must open image, get pixfile name from the header,
	# and generate the new pixfile name.  The CURRENT value of IMDIR is
	# used to generate the new pixfile name.

	im = immapz (Memc[old_hfn], READ_WRITE, 0)

	if (IM_PIXFILE(im) != EOS) {
	    # Get old pixel file filename.
	    call oif_gpixfname (IM_PIXFILE(im), Memc[old_hfn], Memc[old_pfn],
		SZ_PATHNAME)

	    # Get new pixel file filename.
	    call strcpy (Memc[new_hfn], IM_HDRFILE(im), SZ_IMHDRFILE)
	    call oif_mkpixfname (im, Memc[new_pfn], SZ_PATHNAME)

	    # Do not change the pixel file name if the name does not change
	    # other than by the addition of the "aa" style suffix added by
	    # mkpixfname.

	    peq = false
	    call zfnbrk (old_root, old_rootoff, junk)
	    call zfnbrk (new_root, new_rootoff, junk)
	    peq = streq (old_root[old_rootoff], new_root[new_rootoff])

	    if (peq) {
		nchars = strlen (Memc[new_pfn]) - strlen ("aa.imh")
		peq = (strncmp (Memc[old_pfn], Memc[new_pfn], nchars) == 0)
	    }

	    if (peq)
		IM_UPDATE(im) = NO
	    else {
		# If the pixel file rename fails do not rename the header file
		# and do not change the name of the pixel file in the header.

		iferr (call rename (Memc[old_pfn], Memc[new_pfn])) {
		    if (access (Memc[old_pfn], 0, 0) == YES) {
			IM_UPDATE(im) = NO
			call imunmap (im)
			call erract (EA_ERROR)
		    }
		}
	    }

	} else
	    call strcpy (Memc[new_hfn], IM_HDRFILE(im), SZ_IMHDRFILE)

	call strcpy (Memc[old_hfn], IM_HDRFILE(im), SZ_IMHDRFILE)
	call imunmap (im)

	# Rename the header file.
	if (!heq)
	    call rename (Memc[old_hfn], Memc[new_hfn])

	call sfree (sp)
end
