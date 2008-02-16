# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<pmset.h>


# SET_FP -- Set the fixpix mask.
#
# This routine transforms the input mask values into the output mask
# values.  It allows the input mask to have two classes of bad pixels;
# those which are interpolated and those which are not.

procedure set_fp (im, fp)

pointer	im			#I Input mask image pointer
pointer	fp			#O FIXPIX interpolation pointer

int	i, j, nc, nl
long	v[2]
pointer	data1, data2, pm, pmi

int	imstati(), pm_newcopy()
pointer	yt_fpinit()
errchk	malloc, yt_fpinit

begin
	# Set the image size and data buffers.
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	call malloc (data1, nc, TY_SHORT)
	call malloc (data2, nc, TY_SHORT)

	# Get the pixel mask from the image.
	pm = imstati (im, IM_PMDES)

	# Extract the pixels to be interpolated.
	pmi = pm_newcopy (pm)
	v[1] = 1
	do j = 1, nl {
	    v[2] = j
	    call pmglps (pm, v, Mems[data1], 0, nc, PIX_SRC)
	    do i = 0, nc-1 {
	        if (Mems[data1+i] > 1)
		    Mems[data1+i] = 0
	    }
	    call pmplps (pmi, v, Mems[data1], 0, nc, PIX_SRC)
	}

	# Set the interpolation.
	fp = yt_fpinit (pmi, 2, 3)

	# Merge back the bad pixels which are not interpolated.
	v[1] = 1
	do j = 1, nl {
	    v[2] = j
	    call pmglps (pm, v, Mems[data1], 0, nc, PIX_SRC)
	    call pmglps (pmi, v, Mems[data2], 0, nc, PIX_SRC)
	    do i = 0, nc-1 {
	        if (Mems[data2+i] != 0)
		    Mems[data1+i] = Mems[data2+i]
		else if (Mems[data1+i] > 1)
		    Mems[data1+i] = 6
	    }
	    call pmplps (pm, v, Mems[data1], 0, nc, PIX_SRC)
	}

	# Finish up.
	call mfree (data1, TY_SHORT)
	call mfree (data2, TY_SHORT)
	#call pm_close (pmi)
end
