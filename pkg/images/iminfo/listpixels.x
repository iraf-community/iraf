# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# LISTPIXELS -- Convert image pixels into a text stream, i.e., into a list.
# Each pixel is printed on a separate line, preceded by its coordinates.
# The images or image sections may be of any dimension.

procedure t_listpixels()

bool	verbose
char	image[SZ_FNAME]
int	i, j, npix, ndim
long	v[IM_MAXDIM], last_v[IM_MAXDIM]
pointer	im, line, imlist

bool	clgetb()
int	imgnlr(), imgnlx(), imtgetim()
pointer	imtopenp(), immap()

begin
	imlist = imtopenp ("images")
	verbose = clgetb ("verbose")

	while (imtgetim (imlist, image, SZ_FNAME) != EOF) {

	    # Print banner string.
	    if (verbose) {
		call printf ("\n#Image: %s\n\n")
		    call pargstr (image)
	    }

	    # Open the input image.
	    im = immap (image, READ_ONLY, 0)
	    call amovkl (long (1), v, IM_MAXDIM)
	    call amovl (v, last_v, IM_MAXDIM)
	    npix = IM_LEN(im,1)
	    ndim = IM_NDIM(im)

	    switch (IM_PIXTYPE(im)) {
	    case TY_COMPLEX:
  	        while (imgnlx (im, line, v) != EOF) {
	            do i = 1, npix {
		        call printf ("%3d")		    # X
		            call pargi (i)
		        do j = 2, ndim {		    # Y, Z, etc.
		            call printf (" %3d")
			        call pargl (last_v[j])
		        }
		        call printf ("  %z\n")		    # pixel value
			    call pargx (Memx[line+i-1])
	            }
	            call amovl (v, last_v, IM_MAXDIM)
	        }
	    default:
  	        while (imgnlr (im, line, v) != EOF) {
	            do i = 1, npix {
		        call printf ("%3d")		    # X
		            call pargi (i)
		        do j = 2, ndim {		    # Y, Z, etc.
		            call printf (" %3d")
			        call pargl (last_v[j])
		        }
		        call printf ("  %g\n")		    # pixel value
			    call pargr (Memr[line+i-1])
	            }
	            call amovl (v, last_v, IM_MAXDIM)
	        }
	    }

	    call imunmap (im)
	}

	call imtclose (imlist)
end
