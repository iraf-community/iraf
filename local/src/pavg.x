include <imhdr.h>

# PAVG -- CL callable task to plot the average of the lines of an image or
# image section on the standard graphics output.  This example is taken from
# the IRAF paper.

procedure t_pavg()

char    image[SZ_FNAME]         # name of image to be plotted
char    title[SZ_LINE]
int     npix, nlines
long    v[IM_MAXDIM]
pointer im, lineptr, sumv
pointer immap(), imgnlr()

begin
        # Fetch image name from CL and open the image.  The IMMAP
        # function returns a pointer to the image descriptor structure.

        call clgstr ("image", image, SZ_FNAME)
        im = immap (image, READ_ONLY, 0)

        # Allocate a zeroed buffer of length NPIX for the sum-vector,
        # and set V to point to first image line, i.e., v=[1,1,1,...].

        npix = IM_LEN(im,1)
        call calloc (sumv, npix, TY_REAL)
        call amovkl (long(1), v, IM_MAXDIM)

        # Sum the lines of the image.
        for (nlines=0;  imgnlr(im,lineptr,v) != EOF;  nlines=nlines+1)
            call aaddr (Memr[lineptr], Memr[sumv], Memr[sumv], npix)

        # Normalize the sum-vector to get the average.
        call adivkr (Memr[sumv], real(nlines), Memr[sumv], npix)

        # Format plot title and plot the sum-vector on STDGRAPH.
        call sprintf (title, SZ_LINE, "Line Average of %s")
            call pargstr (image)
        call gplotv (Memr[sumv], npix, 1., real(npix), title)

        # Free storage for sum-vector and close image.
        call mfree (sumv, TY_REAL)
        call imunmap (im)
end
