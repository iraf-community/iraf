include <imhdr.h>
include <mwset.h>
include	<error.h>
include "igi.h"
include "commands.h"

procedure ig_section (cmd, igs)

# ig_section -- Read an image section as an igi vector.  Implements the
# command [XYEPL]SECTION.  Ignores any use of the DATA command and
# overwrites the appropriate vector.  The command has two arguments:  the
# image name and how to treat multi-dimensional data.  The options for
# multi-dimensional data are to treat it as one-dimensional and just map
# the pixels to the 1-D igi vector or project the extra dimensions to
# a single line by averaging the projected pixels.
#
# 2/4/91 Optionally use the WCS coordinates in the X vector.  ZGL
# 9/29/92 Modified to allow interdependent WCS coordinates. FV

int	cmd		# Command index
pointer	igs		# Parameters structure

int	igps
pointer	sp
pointer	args		# Command arguments
pointer	imgnam		# Image name
int	projax
pointer	im		# Image descriptor pointer
int	naxis		# Dimensionality
int	proj		# Project multi-dimensional data?
long	vs[IM_MAXDIM]	# Image index vector
long	ve[IM_MAXDIM]	# Image index vector
pointer	tbuf
int	bufsiz		# Size of pixel buffer
int	dim
int	nchar
int	ip
real	factor
pointer	xvec		# Temp X vector

define	IMG_VECT	0
define	SUM_VECT	1
define	AVG_VECT	2

int	immap(), imaccess(), ctowrd(), ctoi()
pointer	imggsr()

begin
	call lcmdcat (igs, YES)
	igps = PLOT_PARMS(igs)

	call smark (sp)
	call salloc (args, SZ_FNAME, TY_CHAR)
	call salloc (imgnam, SZ_FNAME, TY_CHAR)

	# Get the argument(s)
	call igstarg (igs, Memc[args], SZ_FNAME)

	ip = args

	# Parse out the image name
	nchar = ctowrd (Memc, ip, Memc[imgnam], SZ_FNAME)

	if (Memc[imgnam] == EOS) {
	    if (DEBUG_OUTPUT(igs) == YES)
		call eprintf ("No arguments ")

	    return
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("image:  %s ")
		call pargstr (Memc[imgnam])
	}

	if (imaccess (Memc[imgnam], READ_ONLY) == NO) {
	    call eprintf ("File %s not found ")
		call pargstr (Memc[imgnam])
	    return
	}

	# Map the image
	im = immap (Memc[imgnam], READ_ONLY, NULL)

	naxis = IM_NDIM(im)
	projax = 0

	if (naxis == 1) {
	    # 1-D, just read the data into the appropriate buffer
	    # Ignore any additional argument
	    proj = IMG_VECT

	} else if (naxis > 1) {
	    # Do something with multi-dimensional data
	    # Get the optional second argument (the projection method).

	    # Parse out the projection option
	    nchar = ctoi (Memc, ip, projax)

	    if (nchar == 0 || projax == 0) {
		# Treat the section as a single vector
		proj = IMG_VECT

	    } else if (projax > 0) {
		# Average to a single vector
		proj = AVG_VECT

		if (DEBUG_OUTPUT(igs) == YES) {
		    call eprintf ("Average lines about axis %d ")
			call pargi (projax)
		}

	    } else {
		# Sum to a single vector
		proj = SUM_VECT

		projax = abs (projax)

		if (DEBUG_OUTPUT(igs) == YES) {
		    call eprintf ("Sum lines about axis %d ")
			call pargi (projax)
		}

		factor = 1
		do dim = 1, IM_NDIM(im)
		    factor = factor * real (IM_LEN(im,dim))

		factor = factor / real (IM_LEN(im,projax))
	    }

	    projax = min (projax, naxis)
	}

	if (proj == SUM_VECT || proj == AVG_VECT) {
	    # Sum or average the lines to form the vector

	    vs[projax] = 1
	    ve[projax] = IM_LEN(im,projax)
	    bufsiz = ve[projax]
	    call calloc (tbuf, bufsiz, TY_REAL)

	    # Do the projection (this is a sgraph procedure)
	    call im_projection (im, Memr[tbuf], bufsiz, projax)

	    if (proj == SUM_VECT)
		# Rescale by the projected dimensions to restore the sum
		call amulkr (Memr[tbuf], factor, Memr[tbuf], bufsiz)

	} else {
	    # Don't sum or average

	    if (DEBUG_OUTPUT(igs) == YES)
		call eprintf ("Read pixels as vector ")

	    call amovkl (long(1), vs, IM_MAXDIM)
	    bufsiz = 1
	    do dim = 1, IM_NDIM(im) {
		ve[dim] = IM_LEN(im,dim)
		bufsiz  = bufsiz * IM_LEN(im,dim)
	    }

	    # Get the whole section
	    tbuf = imggsr (im, vs, ve, IM_NDIM(im))
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf (" %d pixels read ")
		call pargi (bufsiz)
	}

	call igimcol (cmd, igps, Memc[imgnam], Memr[tbuf], bufsiz)

	if (cmd == YSECTION && MG_IMGWCS(igps) == YES) {
	    # Fill the X buffer with the WCS coordinates
	    # This will overwrite anything in the X buffer, of course

	    if (DEBUG_OUTPUT(igs) == YES)
		call eprintf (" Use WCS coordinates ")
	
	    projax = max (1, min (naxis, projax))
	    call malloc (xvec, bufsiz, TY_REAL)
	    call mwxcol (im, proj, projax, vs, ve, Memr[xvec])
	    call igadat (MG_XDATAP(igps), MG_XNPTS(igps),
		Memr[xvec], bufsiz)
	}

	#call lcmdcat (igs, NO)
	call cmdcat  (igs, NO)

	# That's all folks.
	iferr (call imunmap (im))
	    ;
	call sfree (sp)
end


procedure igimcol (cmd, igps, imgnam, data, npts)

# igimcol -- Move the image buffer into the appropriate igi vector

int	cmd		# Command index
pointer	igps		# Plot parameters structure
char	imgnam[ARB]	# Image name
real	data[ARB]	# Pixel data
int	npts		# Number of points

begin
	switch (cmd) {

	case XSECTION:
	    call igadat (MG_XDATAP(igps), MG_XNPTS(igps), data, npts)
	    call strcpy (imgnam, MG_XLABEL(igps), SZ_LINE)

	case YSECTION:
	    call igadat (MG_YDATAP(igps), MG_YNPTS(igps), data, npts)
	    call strcpy (imgnam, MG_YLABEL(igps), SZ_LINE)

	case ESECTION:
	    call igadat (MG_EDATAP(igps), MG_ENPTS(igps), data, npts)

	case PSECTION:
	    call igadat (MG_PDATAP(igps), MG_PNPTS(igps), data, npts)

	case LSECTION:
	    call igadat (MG_LDATAP(igps), MG_LNPTS(igps), data, npts)

	case SSECTION:
	    # Point marker styles
	    call igadat (MG_SDATAP(igps), MG_SNPTS(igps), data, npts)
	}
end


procedure igadat (pdatap, pnpts, ndata, npts)

#  igadat -- Allocate or reallocate an igi vector for the appropriate size
#  and copy the input data into it.

pointer	pdatap		# igi vector pointer
int	pnpts		# Size of vector
real	ndata[ARB]	# New data
int	npts		# Size of new data

begin

 	if (pdatap == NULL) {
	    # No data vector allocated;  allocate it
	    call malloc (pdatap, npts, TY_REAL) 
		#call eprintf("IGADAT: allocated NEW memory block ...\n")
	
	} else if (pnpts < npts) {
	    # Not enough space allocated for data vector;  reallocate it
	    call realloc (pdatap, npts, TY_REAL)
	    #	call eprintf("IGADAT: REALLOCATED memory block...\n")
	}
	pnpts = npts

	# Copy the input data into the igi vector
	call amovr (ndata, Memr[pdatap], npts)
#	call eprintf("IGADAT: copied image data into Z-buffer ...\n")

end


procedure mwxcol (im, proj, projax, vs, ve, xvec)

pointer	im				#I IMIO pointer
int	proj				#I Projection type
int	projax				#I Logical projection axis
long	vs[IM_MAXDIM], ve[IM_MAXDIM]	#I Logical image section
real	xvec[ARB]			#O X vector

int	i, j, ndim, wcsndim, paxis, mw_stati()
pointer	sp, axno, axval, incoord, outcoord
pointer	mw, ct, mw_openim(), mw_sctran()
errchk	mw_openim, mw_gaxmap, mw_sctran, mw_ctranr

begin
	call smark (sp)
	call salloc (axno, IM_MAXDIM, TY_INT)
	call salloc (axval, IM_MAXDIM, TY_INT)
	call salloc (incoord, IM_MAXDIM, TY_REAL)
	call salloc (outcoord, IM_MAXDIM, TY_REAL)

	iferr {
	    # Cancel axis mapping so that we can use MWCS physical dimensions.
	    mw = NULL
	    i = mw_openim (im); mw = i
	    call mw_seti (mw, MW_USEAXMAP, NO)
	    wcsndim = mw_stati (mw, MW_NPHYSDIM)
	    ndim = IM_NDIM(im)

	    # Get the axis map and determine the physical axis being used.
	    call mw_gaxmap (mw, Memi[axno], Memi[axval], wcsndim)
	    paxis = 0
	    do i = 1, wcsndim {
		if (Memi[axno+i-1] == projax)
		    paxis = i
		Memi[axval+i-1] = vs[i]
	    }

	    # Check and for the no axis mapping case.
	    if (paxis == 0) {
		paxis = projax
		do i = 1, wcsndim
		    Memi[axno+i-1] = i
	    }

	    # Set the transformation.
	    ct = mw_sctran (mw, "logical", "world", 0)

	    # Fill in the X buffer.
	    if (proj == IMG_VECT) {
		j = 1
		repeat {
		    do i = 1, wcsndim {
			if (Memi[axno+i-1] == 0)
			    Memr[incoord+i-1] = 1.
			else
			    Memr[incoord+i-1] = Memi[axval+Memi[axno+i-1]-1]
		    }
		    do i = vs[projax], ve[projax] {
			Memr[incoord+paxis-1] = i 
			call mw_ctranr (ct, Memr[incoord], Memr[outcoord],
			    wcsndim)
			xvec[j] = Memr[outcoord+paxis-1]
			j = j + 1
		    }
		    for (i=2; i<=ndim; i=i+1) {
			Memi[axval+i-1] = Memi[axval+i-1] + 1
			if (ve[i] - Memi[axval+i-1] < 0)
			    Memi[axval+i-1] = vs[i]
			else
			    break
		    }
		} until (i > ndim)
	    } else {
		do i = vs[projax], ve[projax] {
		    Memr[incoord+paxis-1] = i 
		    call mw_ctranr (ct, Memr[incoord], Memr[outcoord], wcsndim)
		    xvec[i] = Memr[outcoord+paxis-1]
		}
	    }
	    call mw_close (mw)
	} then {
	    if (mw != NULL)
		call mw_close (mw)
	    call erract (EA_WARN)
	}

	call sfree (sp)
end
