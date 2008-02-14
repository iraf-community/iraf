# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imfort.h"
include	"oif.h"

# IMRNAM -- Rename an image (both the header and pixel files).  It is not an
# error if there is no pixel file.  The rename operator can be used to move
# an image to a different directory.

procedure imrnam (oimage, nimage, ier)

%	character*(*) oimage
%	character*(*) nimage
int	ier

int	status
pointer	sp, im, ip
pointer	root, extn, osfn
pointer	old_hfn, new_hfn
pointer	old_pfn, new_pfn
pointer	o_osfn, n_osfn

bool	strne()
int	stridxs()
define	quit_ 91

begin
	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (old_hfn, SZ_PATHNAME, TY_CHAR)
	call salloc (new_hfn, SZ_PATHNAME, TY_CHAR)
	call salloc (old_pfn, SZ_PATHNAME, TY_CHAR)
	call salloc (new_pfn, SZ_PATHNAME, TY_CHAR)
	call salloc (n_osfn, SZ_PATHNAME, TY_CHAR)
	call salloc (o_osfn, SZ_PATHNAME, TY_CHAR)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)

	ier = OK

	# Construct filename of new image header file.
	call f77upk (nimage, Memc[new_hfn], SZ_PATHNAME)
	call imf_parse (Memc[new_hfn], Memc[root], Memc[extn])
	if (Memc[extn] == EOS)
	    call strcpy (OIF_HDREXTN, Memc[extn], SZ_FNAME)

	call strcpy (Memc[root], Memc[new_hfn], SZ_FNAME)
	call strcat (".", Memc[new_hfn], SZ_FNAME)
	call strcat (Memc[extn], Memc[new_hfn], SZ_FNAME)

	# Open existing image, make sure that it exists.
	call imopen (oimage, RW, im, ier)
	if (ier != OK) {
	    ier = IE_IMRNAMNEXIM
	    goto quit_
	}

	# Perform clobber checking and delete any old image with the new
	# name, if clobber is enabled.

	call f77upk (oimage, Memc[o_osfn], SZ_PATHNAME)
	call f77upk (nimage, Memc[n_osfn], SZ_PATHNAME)
	if (strne (Memc[o_osfn], Memc[n_osfn])) {
	    call strpak (Memc[new_hfn], Memc[osfn], SZ_PATHNAME)
	    call zfacss (Memc[osfn], 0, 0, status)
	    if (status == YES) {
		call strpak ("clobber", Memc[osfn], SZ_FNAME)
		call zgtenv (Memc[osfn], Memc[osfn], SZ_FNAME, status)
		if (status != ERR) {
		    call imdele (nimage, ier)
		    if (ier != OK) {
			ier = IE_IMRENAME
			goto quit_
		    }
		} else {
		    ier = IE_CLOBBER
		    call f77upk (nimage, Memc[osfn], SZ_PATHNAME)
		    call im_seterrop (ier, Memc[osfn])
		    call sfree (sp)
		    return
		}
	    }
	}

	# Our task here is nontrivial as the pixel file must be renamed as
	# well as the header file, e.g., since renaming the header file may
	# move it to a different directory, and the PIXFILE field in the
	# image header may indicate that the pixel file is in the same dir
	# as the header.  Must open image, get pixfile name from the header,
	# and generate the new pixfile name.

	call strcpy (IM_HDRFILE(im), Memc[old_hfn], SZ_PATHNAME)

	if (IM_PIXFILE(im) != EOS) {
	    # Get old pixel file name.
	    call imf_gpixfname (IM_PIXFILE(im), IM_HDRFILE(im),
		Memc[old_pfn], SZ_PATHNAME)
	    ip = old_pfn + stridxs ("!", Memc[old_pfn])
	    call strcpy (Memc[ip], Memc[old_pfn], SZ_PATHNAME)

	    # Construct the new pixel file name.
	    call strcpy (Memc[new_hfn], IM_HDRFILE(im), SZ_PATHNAME)
	    call imf_mkpixfname (im, Memc[new_pfn], SZ_PATHNAME, ier)
	    if (ier != OK)
		goto quit_

	    ip = new_pfn + stridxs ("!", Memc[new_pfn])
	    call strcpy (Memc[ip], Memc[new_pfn], SZ_PATHNAME)

	    # Update the image header (save new pixel file name).
	    IM_UPDATE(im) = YES

	} else {
	    call strcpy (Memc[new_hfn], IM_HDRFILE(im), SZ_PATHNAME)
	    Memc[old_pfn] = EOS
	}

	call imclos (im, ier)
	if (ier != OK)
	    goto quit_

	call strpak (Memc[old_hfn], Memc[old_hfn], SZ_PATHNAME)
	call strpak (Memc[old_pfn], Memc[old_pfn], SZ_PATHNAME)
	call strpak (Memc[new_hfn], Memc[new_hfn], SZ_PATHNAME)
	call strpak (Memc[new_pfn], Memc[new_pfn], SZ_PATHNAME)

	# Rename the header and pixel files.  It is not an error if
	# there is no pixel file.

	call zfrnam (Memc[old_hfn], Memc[new_hfn], status)
	if (status == ERR)
	    ier = IE_IMRENAME
	else if (Memc[old_pfn] != EOS) {
	    call zfrnam (Memc[old_pfn], Memc[new_pfn], status)
	    if (status == ERR)
		ier = IE_IMRENAME
	}

quit_
	call f77upk (oimage, Memc[old_hfn], SZ_PATHNAME)
	call im_seterrop (ier, Memc[old_hfn])
	call sfree (sp)
end
