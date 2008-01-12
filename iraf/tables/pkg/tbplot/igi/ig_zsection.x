include <imhdr.h>
include "igi.h"
include "commands.h"

define	AVG	1
define	SUM	2

procedure ig_zsection (cmd, igs)

#  IG_ZSECTION -- Implements ZSECTION.  Read an image section as the igi
#  Z vector.  This is distinct from the other vectors in that it is a 2-D
#  raster so it is a really different command.  Ignores any use of the
#  DATA command and overwrites the appropriate vector.  The command has
#  one arguments, the image name (including section syntax, of course).

## 6/19/92  ZGL
## 7/10/92  Save the image name in the structure.  ZGL
## 7/27/92  Assign MG_ZNPTS.  ZGL
## 4/08/97  Adding 'imunmap' to close image after it has been 
##              processed. WJH
## 11/19/97 Added new parameters to perform on-the-fly block averaging/summing
##		and +/-90 degree rotations. WJH
## 2/20/98  Moved the call to 'imunmap' to insure image gets closed, even 
##	    upon exiting ZSECTION early due to errors.  WJH

int	cmd		# Command index
pointer	igs		# Parameters structure

int	igps
pointer	sp
pointer	args		# Command arguments
pointer	imgnam		# Image name
int	projax		# Axis to project multi-dimensional data
pointer	im		# Image descriptor pointer
int	naxis		# Dimensionality
int	nx, ny		# Raster size
int	fill		# Fill viewport with raster?
pointer	tbuf
int	bufsiz		# Size of pixel buffer
int	nchar
int	ip
int	blkfac[5]	# blocking factor for each dimension
int	option		# block operation (average or sum)
pointer rotbuf		# temporary storage of rotated image

int	immap(), imaccess(), ctowrd(), ctoi()
pointer	imgl1r(), imgs2r()

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
		call eprintf ("No arguments \n")

	    return
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("image:  %s \n")
		call pargstr (Memc[imgnam])
	}

	if (imaccess (Memc[imgnam], READ_ONLY) == NO) {
	    call eprintf ("File %s not found in Zsection\n")
		call pargstr (Memc[imgnam])
	    return
	}
	

	# Parse out the fill option
	nchar = ctoi (Memc, ip, fill)
	nchar = ctoi (Memc, ip, projax)
	if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf("fill = %d, projax = %d\n")
			call pargi(fill)
			call pargi(projax)
	}

	# Initialize pointer to IGI data buffer
	# This avoided a problem with calling ZSECTION
	# multiple times in a script and corrupting the memory
	# when using the block average parameter
	#	20 Nov 1997 - WJH
	MG_ZDATAP(igps) = NULL
	MG_ZNPTSX(igps) = 0
        MG_ZNPTSY(igps) = 0
        MG_ZNPTS(igps)  = 0

	if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf("Initialized data buffers...\n")
	}

	# Map the image
	im = immap (Memc[imgnam], READ_ONLY, NULL)

	naxis = IM_NDIM(im)
	if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf("Opened image...\n")
	}
	
	if (naxis == 1) {
	    # 1-D, just read the data into the appropriate buffer
	    # Ignore any additional argument

	    if (DEBUG_OUTPUT(igs) == YES)
		call eprintf ("Read 1-D image section \n")

	    nx = IM_LEN(im,1)
	    MG_ZNPTSX(igps) = nx
	    ny = 1
	    MG_ZNPTSY(igps) = ny
	    tbuf = imgl1r (im)
	    bufsiz = nx

	    if (abs(fill) > 0 ) {
		blkfac[1] = abs(fill)
		blkfac[2] = 1
		blkfac[3] = 1
		blkfac[4] = nx
		blkfac[5] = 1

		if (fill > 0) {
			option = AVG
		} else {
			option = SUM
		}

		call ig_blkavr (tbuf, tbuf, blkfac, option)
		
		nx = blkfac[4]
		ny = blkfac[5]
		bufsiz = int(nx)
		
	        MG_ZNPTSX(igps) = nx
	        MG_ZNPTSY(igps) = ny
		MG_ZNPTS(igps) = bufsiz
	    }


	} else if (naxis == 2) {
	    # 2-D raster
	    if (DEBUG_OUTPUT(igs) == YES)
		call eprintf ("Read 2-D image section ")

	    nx = IM_LEN(im,1)
	    MG_ZNPTSX(igps) = nx
	    ny = IM_LEN(im,2)
	    MG_ZNPTSY(igps) = ny
	    bufsiz = nx * ny
		
	    tbuf = imgs2r (im, 1, nx, 1, ny)

	    if (abs(fill) > 0) {

		blkfac[1] = abs(fill)
		blkfac[2] = abs(fill)
		blkfac[3] = 2
		blkfac[4] = nx
		blkfac[5] = ny

		if (fill > 0) {
			option = AVG
		} else {
			option = SUM
		}

		call ig_blkavr (tbuf, tbuf, blkfac, option)

		nx = blkfac[4]
		ny = blkfac[5]
		bufsiz = int(nx * ny)

	    }
	        MG_ZNPTSX(igps) = nx
	        MG_ZNPTSY(igps) = ny
		MG_ZNPTS(igps) = bufsiz
	    

	} else {
	    call eprintf ("We're really dealing with >2 axes now...NOT! ")
		call imunmap(im)
		call cmdcat(igs,NO)
		call sfree(sp)
	    return
	}


	# Rotate image if called for...
	if (abs(projax) == 90) {
	    call calloc (rotbuf, bufsiz, TY_REAL)
	    if (projax > 0) 
		# Rotate +90 degrees
		call ig_rotvec (Memr[tbuf], Memr[rotbuf], nx, ny, 1)
	    else
		# Rotate -90 degrees 
		call ig_rotvec (Memr[tbuf], Memr[rotbuf], nx, ny, -1)

	    # Copy rotated vector back into output buffer
	    call amovr(Memr[rotbuf], Memr[tbuf], bufsiz)

		# Swap the size of X and Y to match the rotated image
		# JC.Hsu 20 Feb 1998 (WJH)
	        MG_ZNPTSX(igps) = nx
	        MG_ZNPTSY(igps) = ny

	    call mfree(rotbuf,TY_REAL)


	# Finished rotating image
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf (" %d x %d \n")
		call pargi (nx)
		call pargi (ny)
	}
	

	#  Fill the igi Z buffer
	iferr(call igadat (MG_ZDATAP(igps), MG_ZNPTS(igps), Memr[tbuf], bufsiz) ) {
		call eprintf("Error in filling data array with igadat...\n")
		# Close image here upon an error... WJH 20 Feb 1998
		call imunmap(im)
		call sfree(sp)
		call cmdcat (igs,NO)
		return
	}

	#  Save the image name
	MG_DATASRC(igps) = IMAGE_DATA
	call strcpy (Memc[imgnam], MG_FILE_NAME(igps), SZ_FNAME)

#	call lcmdcat (igs, NO)
	call cmdcat  (igs, NO)

	call sfree (sp)
	# Close the image file now, to prevent problems with file I/O
	iferr(call imunmap(im)) 
		call eprintf("Error in closing %s in ZSECTION...\n")
		call pargstr(Memc[imgnam])
	;

end

# IG_ROTVEC -- Rotates a vector +/- 90 degrees
#	    -- Based on Generic Transpose function IMTR2 from IMTRANSPOSE
#	18 Nov 1997 - WJH

procedure ig_rotvec (a, b, nx, ny, dir)

real	a[nx, ny], b[ny, nx]
int	nx, ny, x, y, limx, limy
int	dir
int 	tmp

begin

	do x = 1, nx {
	    limx = nx - x + 1

	    do y = 1, ny {
		limy = ny - y + 1


		if (dir > 0) {
			b[y, x] = a[x,limy]
		 } else
			b[y, x] = a[limx,y]

	    }
	}

	# Pass back altered dimensions of new array
	tmp = nx
	nx = ny
	ny = tmp

end
