include <imhdr.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"
include "rvfilter.h"

# WRITE_CCF - Write the CCF image to disk.

procedure write_ccf (rv)

pointer	rv					#I RV struct pointer

pointer	sp, bp
int	imaccess(), access()
int	stat, scan()
bool	streq()

define	err_			99
define	loop_			98

begin
	call smark (sp)
	call salloc (bp, SZ_FNAME, TY_CHAR)

	if (RV_CCFFILE(rv) == NULL) {
	    call malloc (RV_CCFFILE(rv), SZ_FNAME, TY_CHAR)
	    Memc[RV_CCFFILE(rv)] = EOS
	}

	call gargstr (Memc[bp], SZ_FNAME)
	if (Memc[bp] != EOS)
	    call strcpy (Memc[bp], Memc[RV_CCFFILE(rv)], SZ_FNAME)
	else {
loop_	    call printf ("Output filename (%s): ")
		call pargstr (Memc[RV_CCFFILE(rv)])
	    call flush (STDOUT)
	    stat = scan ()
	        call gargstr (Memc[bp], SZ_FNAME)
	    if (Memc[bp] != EOS && Memc[bp] != '\n')
		call strcpy (Memc[bp], Memc[RV_CCFFILE(rv)], SZ_FNAME)
	    else if (streq("",Memc[RV_CCFFILE(rv)]) || 
		 streq(" ",Memc[RV_CCFFILE(rv)]))
		      goto loop_
	}

	if (RV_CCFTYPE(rv) == OUTPUT_TEXT) { 
	    if (access (Memc[RV_CCFFILE(rv)], 0, 0) == YES) { 
	        call printf ("Warning: File `%s' already exists.\n") 
		    call pargstr (Memc[RV_CCFFILE(rv)])
		call flush (STDOUT)
		call tsleep (2)
		goto loop_
	    }
	    call wrt_ccf_text(rv)

	} else if (RV_CCFTYPE(rv) == OUTPUT_IMAGE) {
	    if (imaccess(Memc[RV_CCFFILE(rv)], 0) == YES) {
	        call printf ("Warning: Image `%s' already exists.\n")
		    call pargstr (Memc[RV_CCFFILE(rv)])
		call flush (STDOUT)
		call tsleep (2)
		goto loop_
	    }
	    call wrt_ccf_image(rv)
	} 

err_	call sfree (sp)
end


# WRT_CCF_IMAGE - Write the ccf as an output image with appropriate header
# information.

procedure wrt_ccf_image (rv)

pointer	rv					#I RV struct pointer

pointer	sp, x, im, buf, bp
int	npts

pointer	immap(), impl1r()
double	rv_shift2vel()
errchk	immap, impl1r

define	err_			99

begin
	call smark (sp)
	call salloc (x, RV_CCFNPTS(rv), TY_REAL)
	call salloc (bp, SZ_FNAME, TY_CHAR)

	# Open the image
	iferr (im = immap(Memc[RV_CCFFILE(rv)], NEW_IMAGE, 2880)) {
	    call rv_errmsg ("Error opening ccf output image '%s'.")
		call pargstr (Memc[RV_CCFFILE(rv)])
	    goto err_
	}

	# Set up the image parameters
	IM_PIXTYPE(im) = TY_REAL
	IM_NDIM(im) = 1
	IM_LEN(im,1) = RV_CCFNPTS(rv)
	npts = RV_CCFNPTS(rv)
	call sprintf (IM_TITLE(im), SZ_IMTITLE, "Correlation Function")

	# Now dump the data into the image
	buf = impl1r (im)
	call amovr (WRKPIXY(rv,1), Memr[buf], RV_CCFNPTS(rv))

	# Add image header information
	call imastr (im, "object", IMAGE(rv))
	call imastr (im, "template", RIMAGE(rv))
	call imaddi (im, "npts", RV_CCFNPTS(rv))
	if (RV_DCFLAG(rv) != -1) {
	    #call imaddr (im, "crval1", 
	    #	real(rv_shift2vel(rv,real(-(RV_CCFNPTS(rv)/2)))))
	    #call imaddr (im, "crval1", real (RV_VCOR(rv)))
	    call imaddr (im, "crval1", 
	    	real(rv_shift2vel(rv,real(RV_SHIFT(rv)))))
	    call imaddr (im, "cdelt1", RV_DELTAV(rv))

	} else {
	    call imaddr (im, "crval1", real(-RV_CCFNPTS(rv))/2.)
	    call imaddr (im, "cdelt1", 1.)
	}
	#call imaddi (im, "crpix1", 1)
	call imaddr (im, "crpix1", ((npts/2+1)+RV_SHIFT(rv)))
	call imastr (im, "ctype1", "velocity")
	call imastr (im, "cunit1", "km/s")
	call nam_filtype (rv, Memc[bp])
	call imastr (im, "filtype", Memc[bp])
	call imaddi (im, "cuton", RVF_CUTON(rv))
	call imaddi (im, "cutoff", RVF_CUTOFF(rv))
	call imaddi (im, "fullon", RVF_FULLON(rv))
	call imaddi (im, "fulloff", RVF_FULLOFF(rv))

	call imunmap (im)
err_	call sfree (sp)
end


# WRT_CCF_TEXT - Write out the ccf to a text file.

procedure wrt_ccf_text (rv)

pointer	rv					#I RV struct pointer

pointer	fd, sp, x
int	i, j, npts

pointer	open()
double	rv_shift2vel()
errchk	open

begin
	# Open the text file
	iferr (fd=open(Memc[RV_CCFFILE(rv)], NEW_FILE, TEXT_FILE)) {
	    call rv_errmsg ("Error opening ccf output file `%s'.")
		call pargstr (Memc[RV_CCFFILE(rv)])
	    return
	}

	npts = RV_CCFNPTS(rv)

	call smark (sp)
	call salloc (x, npts, TY_REAL)

	# Set up X-axis
	if (RV_DCFLAG(rv) != -1) {
            do i = 1, npts 
                Memr[x+i-1] = real (rv_shift2vel(rv,WRKPIXX(rv,i)))

	} else {
	    i = - (RV_CCFNPTS(rv) / 2)
	    do j = 1, npts {
		Memr[x+j-1] = real (i)
		i = i + 1
	    }
	}

	# Write it out
	do i = 1, npts {
	    call fprintf (fd, "%.1f %f\n")
	 	call pargr (Memr[x+i-1])
		call pargr (WRKPIXY(rv,i))
	}
	call flush (fd)

	call close (fd)
	call sfree (sp)
end
