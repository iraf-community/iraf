include	<imhdr.h>
include	<imio.h>
include	<pkg/gtools.h>
include	"identify.h"

# ID_GDATA -- Get image data.

procedure id_gdata (id)

pointer	id				# ID pointer

int	i, j, vmap, voff, vstep, axis, first, last, nsum1
pointer	im, sp, buf, str1, str2

double	asumd(), imgetd()
int	imaccf()
pointer	immap(), imgl1d(), imgl2d()
errchk	immap, imgl1d, imgl2d

begin
	# Map the image.  Abort if the image is not one dimensional.

	im = immap (Memc[ID_IMAGE(id)], READ_ONLY, 0)

	if (IM_NDIM(im) > 1 && IM_LEN(im,2) > 1) {
	    call imunmap (im)
	    call error (0, "Image is not one dimensional")
	}

	ID_NPTS(id) = IM_LEN(im, 1)
	vmap = IM_VMAP (im, 1)
	voff = IM_VOFF (im, vmap)
	vstep = IM_VSTEP (im, vmap)

	# Since we are not prepared to deal with pixel coordinates which are
	# not the same as data vector coordinates override the offset and
	# step.

	voff = 0
	vstep = 1

	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Set graph title.
	call sprintf (Memc[str1], SZ_LINE, "identify %s\n%s")
	    call pargstr (Memc[ID_IMAGE(id)])
	    call pargstr (IM_TITLE(im))
	call gt_sets (ID_GT(id), GTTITLE, Memc[str1])

	# Get header parameters.
	call sprintf (Memc[str2], SZ_LINE, "crpix%d")
	    call pargi (vmap)
	iferr (ID_CRPIX(id) = imgetd (im, Memc[str2]))
	    ID_CRPIX(id) = 1.
	call sprintf (Memc[str2], SZ_LINE, "crval%d")
	    call pargi (vmap)
	iferr (ID_CRVAL(id) = imgetd (im, Memc[str2]))
	    ID_CRVAL(id) = 1.
	call sprintf (Memc[str2], SZ_LINE, "cdelt%d")
	    call pargi (vmap)
	iferr (ID_CDELT(id) = imgetd (im, Memc[str2])) {
	    call sprintf (Memc[str2], SZ_LINE, "cd%d_%d")
	        call pargi (vmap)
	        call pargi (vmap)
	    iferr (ID_CDELT(id) = imgetd (im, Memc[str2]))
	        ID_CDELT(id) = 1.
	}
	call sprintf (Memc[str2], SZ_LINE, "ctype%d")
	    call pargi (vmap)
	if (imaccf (im, Memc[str2]) == YES) {
	    call imgstr (im, Memc[str2], Memc[str1], SZ_LINE)
	    call gt_sets (ID_GT(id), GTXLABEL, Memc[str1])
	    call ic_pstr (ID_IC(id), "ylabel", Memc[str1])
	}
	call sprintf (Memc[str2], SZ_LINE, "cunit%d")
	    call pargi (vmap)
	if (imaccf (im, Memc[str2]) == YES) {
	    call imgstr (im, Memc[str2], Memc[str1], SZ_LINE)
	    call gt_sets (ID_GT(id), GTXUNITS, Memc[str1])
	    call ic_pstr (ID_IC(id), "yunits", Memc[str1])
	}

	# Free previous vectors and allocate new vectors.

	call mfree (ID_PIXDATA(id), TY_DOUBLE)
	call mfree (ID_IMDATA(id), TY_DOUBLE)

	call malloc (ID_PIXDATA(id), ID_NPTS(id), TY_DOUBLE)
	call malloc (ID_IMDATA(id), ID_NPTS(id), TY_DOUBLE)

	# Set the coordinates.

	do i = 1, ID_NPTS(id)
	    PIXDATA(id,i) = voff + (i - 1) * vstep + 1

	if ((ID_NSUM(id) == 1) || (IM_NPHYSDIM (im) == 1)) {
	    call amovd (Memd[imgl1d(im)], IMDATA(id,1), ID_NPTS(id))

	} else {
	    if (vmap == 1)
		axis = 2
	    else
		axis = 1

	    first = max (1, IM_VOFF (im, axis) + 1 - ID_NSUM(id) / 2)
	    last = min (IM_SVLEN (im, axis), first + ID_NSUM(id) - 1)

	    call imunmap (im)

	    call imgimage (Memc[ID_IMAGE(id)], Memc[str1], SZ_LINE)
	    im = immap (Memc[str1], READ_ONLY, 0)

	    switch (vmap) {
	    case 1:
		call aclrd (IMDATA(id,1), ID_NPTS(id))
		do i = first, last {
		    buf = imgl2d (im, i)
		    if (vstep == 1)
		        call aaddd (Memd[buf+voff], IMDATA(id,1), IMDATA(id,1),
			    ID_NPTS(id))
		    else {
			do j = 1, ID_NPTS(id)
			    IMDATA(id,j) = IMDATA(id,j) +
				Memd[buf+voff+(j-1)*vstep]
		    }
		}
	    case 2:
		nsum1 = last - first + 1
		do i = 1, ID_NPTS(id) {
		    j = voff + (i - 1) * vstep + 1
		    buf = imgl2d (im, j)
		    IMDATA(id,i) = asumd (Memd[buf+first-1], nsum1)
		}
	    }
	}

	call sfree (sp)
	call imunmap (im)
end
