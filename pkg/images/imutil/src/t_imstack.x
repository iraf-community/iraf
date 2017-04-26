# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include <mwset.h>

define	NTYPES	7


# T_IMSTACK -- Stack images into a single image of higher dimension.

procedure t_imstack ()

int	i, j, npix, list, pdim, lmax, lindex
int	axno[IM_MAXDIM], axval[IM_MAXDIM]
long	line_in[IM_MAXDIM], line_out[IM_MAXDIM]
pointer	sp, input, output, in, out, buf_in, buf_out, mwin, mwout

bool	envgetb()
int	imtopenp(), imtgetim(), imtlen()
int	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
int	mw_stati()
pointer	immap(), mw_open(), mw_openim()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	# Get the input images and the output image.
	list = imtopenp ("images")
	call clgstr ("output", Memc[output], SZ_FNAME)

	# Add each input image to the output image.

	i = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {

	    i = i + 1
	    in = immap (Memc[input], READ_ONLY, 0)

	    # For the first input image map the output image as a copy
	    # and increment the dimension.  Set the output line counter.

	    if (i == 1) {
		out = immap (Memc[output], NEW_COPY, in)
		call isk_new_image (out)
		IM_NDIM(out) = IM_NDIM(out) + 1
		IM_LEN(out, IM_NDIM(out)) = imtlen (list)
		npix = IM_LEN(out, 1)
	        call amovkl (long(1), line_out, IM_MAXDIM)
	    }

	    # Check next input image for consistency with the output image.
	    if (IM_NDIM(in) != IM_NDIM(out) - 1)
		call error (0, "Input images not consistent")
	    do j = 1, IM_NDIM(in) {
		if (IM_LEN(in, j) != IM_LEN(out, j))
		    call error (0, "Input images not consistent")
	    }

	    # Copy the input lines from the image to the next lines of
	    # the output image.  Switch on the output data type to optimize
	    # IMIO.

	    call amovkl (long(1), line_in, IM_MAXDIM)
	    switch (IM_PIXTYPE (out)) {
	    case TY_SHORT:
	        while (imgnls (in, buf_in, line_in) != EOF) {
		    if (impnls (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovs (Mems[buf_in], Mems[buf_out], npix)
		}
	    case TY_INT:
	        while (imgnli (in, buf_in, line_in) != EOF) {
		    if (impnli (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovi (Memi[buf_in], Memi[buf_out], npix)
		}
	    case TY_USHORT, TY_LONG:
	        while (imgnll (in, buf_in, line_in) != EOF) {
		    if (impnll (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovl (Meml[buf_in], Meml[buf_out], npix)
		}
	    case TY_REAL:
	        while (imgnlr (in, buf_in, line_in) != EOF) {
		    if (impnlr (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovr (Memr[buf_in], Memr[buf_out], npix)
		}
	    case TY_DOUBLE:
	        while (imgnld (in, buf_in, line_in) != EOF) {
		    if (impnld (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovd (Memd[buf_in], Memd[buf_out], npix)
		}
	    case TY_COMPLEX:
	        while (imgnlx (in, buf_in, line_in) != EOF) {
		    if (impnlx (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovx (Memx[buf_in], Memx[buf_out], npix)
		}
	    default:
	        while (imgnlr (in, buf_in, line_in) != EOF) {
		    if (impnlr (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovr (Memr[buf_in], Memr[buf_out], npix)
		}
	    }

	    # Update the wcs. The output image will inherit the wcs of
	    # the first input image. The new axis will be assigned the
	    # identity transformation if wcsdim of the original image is
	    # less than the number of dimensions in the stacked image.

	    if ((i == 1) && (! envgetb ("nowcs"))) {
		mwin = mw_openim (in)
		pdim = mw_stati (mwin, MW_NPHYSDIM)
		call mw_gaxmap (mwin, axno, axval, pdim)
		lmax = 0
		lindex = 0
		do j = 1, pdim {
		    if (axno[j] <= lmax)
			next
		    lmax = axno[j]
		    lindex = j
		}
		if (lindex < pdim) {
		    axno[pdim] = lmax + 1
		    axval[pdim] = 0
		    call mw_saxmap (mwin, axno, axval, pdim)
		    call mw_saveim (mwin, out)
		} else {
		    mwout = mw_open (NULL, pdim + 1)
		    call isk_wcs (mwin, mwout, IM_NDIM(out))
		    call mw_saveim (mwout, out)
		    call mw_close (mwout)
		}
		call mw_close (mwin)
	    }

	    call imunmap (in)
	}

	# Finish up.
	call imunmap (out)
	call imtclose (list)
	call sfree (sp)
end


# ISK_NEW_IMAGE -- Get a new image title and pixel type.
#
# The strings 'default' or '*' are recognized as defaulting to the original
# title or pixel datatype.

procedure isk_new_image (im)

pointer	im				# image descriptor

pointer	sp, lbuf
int	i, type_codes[NTYPES]
bool	strne()
int	stridx()

string	types "suilrdx"
data	type_codes /TY_SHORT,TY_USHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,
	TY_COMPLEX/

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	call clgstr ("title", Memc[lbuf], SZ_LINE)
	if (strne (Memc[lbuf], "default") && strne (Memc[lbuf], "*"))
	    call strcpy (Memc[lbuf], IM_TITLE(im), SZ_IMTITLE) 

	call clgstr ("pixtype", Memc[lbuf], SZ_LINE)
	if (strne (Memc[lbuf], "default") && strne (Memc[lbuf], "*")) {
	    i = stridx (Memc[lbuf], types)
	    if (i != 0)
	        IM_PIXTYPE(im) = type_codes[i]
	}

	call sfree (sp)
end


# ISK_WCS -- Update the wcs of the stacked image.

procedure isk_wcs (mwin, mwout, ndim)

pointer	mwin			# input wcs descriptor
pointer	mwout			# output wcs descriptor
int	ndim			# the dimension of the output image

int	i, j, nin, nout, szatstr, axno[IM_MAXDIM], axval[IM_MAXDIM]
pointer	sp, wcs, attribute, matin, matout, rin, rout, win, wout, atstr
int	mw_stati(), itoc(), strlen()
errchk	mw_newsystem()

begin
	# Get the sizes of the two wcs.
	nin = mw_stati (mwin, MW_NPHYSDIM)
	nout = mw_stati (mwout, MW_NPHYSDIM)
	szatstr = SZ_LINE

	# Allocate space for the matrices and vectors.
	call smark (sp)
	call salloc (wcs, SZ_FNAME, TY_CHAR)
	call salloc (matin, nin * nin, TY_DOUBLE)
	call salloc (matout, nout * nout, TY_DOUBLE)
	call salloc (rin, nin, TY_DOUBLE)
	call salloc (rout, nout, TY_DOUBLE)
	call salloc (win, nin, TY_DOUBLE)
	call salloc (wout, nout, TY_DOUBLE)
	call salloc (attribute, SZ_FNAME, TY_CHAR)
	call malloc (atstr, szatstr, TY_CHAR)

	# Set the system name.
	call mw_gsystem (mwin, Memc[wcs], SZ_FNAME)
	iferr (call mw_newsystem (mwout, Memc[wcs], nout))
	    call mw_ssystem (mwout, Memc[wcs]) 

	# Set the lterm.
	call mw_gltermd (mwin, Memd[matin], Memd[rin], nin)
	call aclrd (Memd[rout], nout)
	call amovd (Memd[rin], Memd[rout], nin)
	call mw_mkidmd [Memd[matout], nout)
	call isk_mcopy (Memd[matin], nin, Memd[matout], nout)
	call mw_sltermd (mwout, Memd[matout], Memd[rout], nout)

	# Set the wterm.
	call mw_gwtermd (mwin, Memd[rin], Memd[win], Memd[matin], nin)
	call aclrd (Memd[rout], nout)
	call amovd (Memd[rin], Memd[rout], nin)
	call aclrd (Memd[wout], nout)
	call amovd (Memd[win], Memd[wout], nin)
	call mw_mkidmd [Memd[matout], nout)
	call isk_mcopy (Memd[matin], nin, Memd[matout], nout)
	call mw_swtermd (mwout, Memd[rout], Memd[wout], Memd[matout], nout)

	# Set the axis map.
	call mw_gaxmap (mwin, axno, axval, nin)
	do i = nin + 1, nout {
	    axno[i] = ndim
	    axval[i] = 0
	}
	call mw_saxmap (mwout, axno, axval, nout)

	# Get the axis list and copy the old attribute list for each axis.
	do i = 1, nin {
	    iferr (call mw_gwattrs (mwin, i, "wtype", Memc[atstr], szatstr))
		call strcpy ("linear", Memc[atstr], szatstr)
	    call mw_swtype (mwout, i, 1, Memc[atstr], "")
	    for (j = 1; ; j = j + 1) {
		if (itoc (j, Memc[attribute], SZ_FNAME) <= 0)
		    Memc[attribute] = EOS
		repeat {
		    iferr (call mw_gwattrs (mwin, i, Memc[attribute],
		        Memc[atstr], szatstr))
			Memc[atstr] = EOS
		    if (strlen (Memc[atstr]) < szatstr)
			break
		    szatstr = szatstr + SZ_LINE
		    call realloc (atstr, szatstr, TY_CHAR)
		}
		if (Memc[atstr] == EOS)
		    break
		call mw_swattrs (mwout, i, Memc[attribute], Memc[atstr])
	    }
	}

	# Set the default attributes for the new axes.
	do i = nin + 1, nout
	    call mw_swtype (mwout, i, 1, "linear", "")

	call mfree (atstr, TY_CHAR)
	call sfree (sp)
end


# ISK_MCOPY -- Copy a smaller 2d matrix into a larger one.

procedure isk_mcopy (matin, nin, matout, nout)

double	matin[nin,nin]		# the input matrix
int	nin			# size of the input matrix
double	matout[nout,nout]	# the input matrix
int	nout			# size of the output matrix

int	i,j

begin
	do i = 1, nin {
	    do j = 1, nin
		matout[j,i] = matin[j,i]
	}
end
