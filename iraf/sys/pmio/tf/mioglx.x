# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<pmset.h>
include	<plio.h>
include	"../mio.h"

# MIO_GLSEG -- Get a line segment from a masked image.  A line segment is a
# region of the data image for which the corresponding region of the mask has
# the constant nonzero value MVAL.  Line segments are returned for each line in
# the region VS to VE, returning the number of pixels in each line segment as
# the function value, or EOF when the region is exhausted.  Once EOF is
# reached, repeated calls will continue to return EOF until the next call to
# MIO_SETRANGE.  Repeated calls to MIO_SETRANGE may be used to access a series
# of distinct regions in the image.  If a subregion of the image is being
# accessed with MIO_SETRANGE, the vector coordinates V returned below will
# be relative to the defined subregion (if this is not what is desired,
# the range should be set to the full image and a region mask used to mask
# off the subregion to be accessed).

int procedure mio_glsegx (mp, ptr, mval, v, npix)

pointer	mp			#I MIO descriptor
pointer	ptr			#O pointer to a buffer containing the data
int	mval			#O mask value for the output line segment
long	v[IM_MAXDIM]		#U coords of first pixel in output ine segment
int	npix			#O number of pixels in output line segment

int	x1, i
long	ve[IM_MAXDIM]
pointer	pm, im, rl, rp, bp
pointer	imgl2x(), imgl3x(), imggsx()
errchk	imgl2x, imgl3x, imggsx, pm_glri
bool	pm_sectnotempty()
int	plloop()

begin
	pm = M_PM(mp)
	rl = M_RLP(mp)

	# Initialization performed for the first i/o on a new region.
	if (M_ACTIVE(mp) == NO) {
	    call plsslv (pm, M_VS(mp,1), M_VN(mp,1), M_V(mp,1), M_VE(mp,1))
	    call pm_glri (pm,
		M_V(mp,1), Memi[rl], M_DEPTH(mp), M_VN(mp,1), PIX_SRC)
	    M_RLI(mp) = RL_FIRST
	    M_ACTIVE(mp) = YES
	}

	# Get a new mask line?
	while (M_RLI(mp) > RLI_LEN(rl))
	    if (plloop (M_V(mp,1), M_VS(mp,1), M_VE(mp,1),
		M_NDIM(mp)) == LOOP_DONE) {
		return (EOF)
	    } else {
		call amovl (M_V(mp,1), ve, M_NDIM(mp))
		ve[1] = M_VE(mp,1)
		if (pm_sectnotempty (pm, M_V(mp,1), ve, M_NDIM(mp))) {
		    call pm_glri (pm,
			M_V(mp,1), Memi[rl], M_DEPTH(mp), M_VN(mp,1), PIX_SRC)
		    M_RLI(mp) = RL_FIRST
		}
	    }

	# Get a new image line?
	if (M_RLI(mp) == RL_FIRST) {
	    call amovl (M_V(mp,1), v, M_NDIM(mp))
	    im = M_IM(mp)
	
	    if (M_LINEIO(mp) == YES && M_NDIM(mp) == 2)
		bp = imgl2x (im, v[2])
	    else if (M_LINEIO(mp) == YES && M_NDIM(mp) == 3)
		bp = imgl3x (im, v[2], v[3])
	    else {
		call amovl (v, ve, M_NDIM(mp));  ve[1] = M_VE(mp,1)
		bp = imggsx (im, v, ve, M_NDIM(mp))
	    }

	    M_LBP(mp) = bp
	} else
	    bp = M_LBP(mp)

	# Return the next line segment.
	rp = rl + (M_RLI(mp) - 1) * RL_LENELEM
	M_RLI(mp) = M_RLI(mp) + 1

	x1   = Memi[rp+RL_XOFF]
	npix = Memi[rp+RL_NOFF]
	mval = Memi[rp+RL_VOFF]
	ptr  = bp + x1 - M_VS(mp,1)

	if (M_REGCOORDS(mp) == NO) {
	    v[1] = x1
	    do i = 2, M_NDIM(mp)
		v[i] = M_V(mp,i)
	} else {
	    v[1] = x1 - M_VS(mp,1) + 1
	    do i = 2, M_NDIM(mp)
		v[i] = M_V(mp,i) - M_VS(mp,i) + 1
	}

	return (npix)
end
