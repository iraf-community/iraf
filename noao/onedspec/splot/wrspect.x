include	<imhdr.h>
include	"../idsmtn.h"

# WRSPECT -- Write out spectrum

procedure wrspect (image, pix, npts, ids, in)

char	image[ARB]
real	pix[ARB]
int	npts
pointer	ids, in

int	i, new, fmt, line
pointer	sp, str, out

int	clgwrd(), clgeti()
bool	clgetb(), streq()
pointer immap(), impl1r(), imgl2r(), impl2r()
errchk	immap

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get new image name
10	call clgstr ("new_image", Memc[str], SZ_FNAME)

	# Write to the current image in place.
	if (streq (image, Memc[str])) {
	    if (!clgetb ("overwrite"))
		goto 10
	    call imunmap (in)
	    out = immap (image, READ_WRITE, 0)
	    call store_keywords (ids, out)
	    if (IM_NDIM(out) == 1)
	        call amovr (pix, Memr[impl1r(out)+NP1(ids)], npts)
	    else
	        call amovr (pix, Memr[impl2r(out,LINE(ids))+NP1(ids)], npts)
	    call imunmap (out)

	    # Reopen the image.
	    in = immap (image, READ_ONLY, 0)

	    return
	}
	    
	# Map the new image.  Allow overwriting.
	new = YES
	iferr (out = immap (Memc[str], NEW_COPY, in)) {
	    if (!clgetb ("overwrite"))
		goto 10
	    out = immap (Memc[str], READ_WRITE, 0)
	    new = NO
	}

	# Set the output format.
	if (IM_NDIM(in) == 1)
	    fmt = 1
	else
	    fmt = clgwrd ("format", Memc[str], SZ_FNAME,
		"|onedspec|echelle|multispec|")

	# If a new image fix the header and possibly copy other lines.
	if (new == YES) {
	    switch (fmt) {
	    case 1:
		if (IM_NDIM(in) > 1) {
		    do i = 1, IM_LEN(in,2) {
		        call sprintf (Memc[str], SZ_FNAME, "APNUM%d")
			    call pargi (i)
		        call imdelf (out, Memc[str])
		    }
		    IM_NDIM(out) = 1
		    call imastr (out, "apformat", "onedspec")
		}
	        call amovr (pix, Memr[impl1r(out)+NP1(ids)], npts)
	    case 2, 3:
	        do i = 1, IM_LEN(in,2)
		    call amovr (Memr[imgl2r(in,i)], Memr[impl2r(out,i)],
		        IM_LEN(in,1))
	        call amovr (pix, Memr[impl2r(out,LINE(ids))+NP1(ids)], npts)
	    }
	} else {
	    switch (fmt) {
	    case 1:
		if (IM_NDIM(out) > 1) {
		    line = clgeti ("line")
	            call amovr (pix, Memr[impl2r(out,line)+NP1(ids)], npts)
		} else
	            call amovr (pix, Memr[impl1r(out)+NP1(ids)], npts)
	    case 2, 3:
		if (IM_NDIM(out) > 1)
	            call amovr (pix, Memr[impl2r(out,LINE(ids))+NP1(ids)], npts)
		else
	            call amovr (pix, Memr[impl1r(out)+NP1(ids)], npts)
	    }
	}

	# Store the new header and unmap the output image.
	call store_keywords (ids, out)
	call imunmap (out)
end
