include <imhdr.h>
include <mach.h>
include "i2sun.h"


# CNV_IMAGE -- Read each line of the input image, apply ztransform, convert
# to rasterfile form, and write to rasterfile.

procedure cnv_image (im, slice, tr, uptr, rfd)
pointer	im		# input image
int	slice		# current slice if n>2 image
pointer	tr		# spatial & greyscale transform structure
pointer	uptr		# pointer to user-specified transfer lut
pointer rfd		# output rasterfile

real	z1, z2, rz1, rz2
int	ztrans, row, xblk, yblk, ocols, olines
real	px1, px2, py1, py2	# image coords in fractional image pixels
pointer	sp, si, bufptr, out, rtemp, packed
short	z1_s, z2_s, rz1_s, rz2_s
bool	unitary_greyscale_transformation

bool	fp_equalr()
pointer	siglns(), siglnr(), sigln_setup()
errchk	siglns(), siglnr(), sigln_setup()

begin
	# Set up for scaled image input.
	px1 = 1
	px2 = IM_LEN(im,COL)
	py1 = 1
	py2 = IM_LEN(im,LINE)
	ocols = TR_XE(tr) - TR_XS(tr) + 1
	olines = TR_YE(tr) - TR_YS(tr) + 1

	# Round odd-numbered numbers of columns up due to restrictions on
	# IRAF binary byte i/o (number of bytes of image data must match
	# that specified in rasterfile header).
	if (mod (ocols, 2) == 1)
	    ocols = ocols + 1

	xblk = INDEFI
	yblk = INDEFI
	si = sigln_setup (im, px1,px2,ocols,xblk, py1,py2,olines,yblk, 
	    TR_ORDER(tr))

	# Type short pixels are treated as a special case to minimize vector
	# operations for such images (which are common).  If unity mapping is
	# employed the data is simply copied, i.e., floor ceiling constraints
	# are not applied.  This is very fast and will produce a contoured
	# image on the display which will be adequate for some applications.

	z1 = TR_Z1(tr)
	z2 = TR_Z2(tr)
	ztrans = TR_ZTRANS(tr)
	rz1 = COLORSTART
	rz2 = COLOREND
	if (ztrans == Z_UNITARY) {
	    unitary_greyscale_transformation = true
	} else if (ztrans == Z_LINEAR) {
	    unitary_greyscale_transformation =
		((fp_equalr(z1,rz1) && fp_equalr(z2,rz2)) || fp_equalr(z1,z2))
	} else
	    unitary_greyscale_transformation = false

	if (IM_PIXTYPE(im) == TY_SHORT && ztrans != Z_LOG) {

	    call smark (sp)
	    call salloc (out, ocols, TY_SHORT)
	    call salloc (packed, ocols, TY_CHAR)
	    z1_s = z1;  z2_s = z2

	    for (row=olines;  row >= 1;  row=row-1) {
		bufptr = siglns (si, row, TR_SLICEAXIS(tr), slice)

		if (unitary_greyscale_transformation) {
		    call amovs (Mems[bufptr], Mems[out], ocols)
		} else if (ztrans == Z_USER) {
		    rz1_s = U_Z1; rz2_s = U_Z2
		    call amaps (Mems[bufptr], Mems[out], ocols, z1_s, z2_s,
			rz1_s, rz2_s)
		    call aluts (Mems[out], Mems[out], ocols, Mems[uptr])
		} else {
		    rz1_s = rz1; rz2_s = rz2
		    call amaps (Mems[bufptr], Mems[out], ocols, z1_s, z2_s,
			rz1_s, rz2_s)
		}

		# Pack to unsigned byte and write to file.
		call achtsc (Mems[out], Memc[packed], ocols)
		call chrpak (Memc[packed], 1, Memc[packed], 1, ocols)
		call write (rfd, Memc[packed], ocols / SZB_CHAR)
	    }
	    call sfree (sp)

	} else if (ztrans == Z_USER) {
	    call smark (sp)
	    call salloc (rtemp, ocols, TY_REAL)
	    call salloc (out, ocols, TY_SHORT)
	    call salloc (packed, ocols, TY_CHAR)

	    for (row=olines;  row >= 1;  row=row-1) {
		bufptr = siglnr (si, row, TR_SLICEAXIS(tr), slice)
		call amapr (Memr[bufptr], Memr[rtemp], ocols, z1, z2, 
		    real(U_Z1), real(U_Z2))
		call achtrs (Memr[rtemp], Mems[out], ocols)
		call aluts (Mems[out], Mems[out], ocols, Mems[uptr])
		call achtsc (Mems[out], Memc[packed], ocols)
		call chrpak (Memc[packed], 1, Memc[packed], 1, ocols)
		call write (rfd, Memc[packed], ocols / SZB_CHAR)
	    }
	    call sfree (sp)

	} else {
	    call smark (sp)
	    call salloc (rtemp, ocols, TY_REAL)
	    call salloc (packed, ocols, TY_CHAR)

	    for (row=olines;  row >= 1;  row=row-1) {
		bufptr = siglnr (si, row, TR_SLICEAXIS(tr), slice)

		if (unitary_greyscale_transformation)  {
		    call amovr (Memr[bufptr], Memr[rtemp], ocols)
		} else if (ztrans == Z_LOG) {
		    call amapr (Memr[bufptr], Memr[rtemp], ocols,
			z1, z2, 1.0, 10.0 ** MAXLOG)
		    call alogr (Memr[rtemp], Memr[rtemp], ocols)
		    call amapr (Memr[rtemp], Memr[rtemp], ocols,
			1.0, real(MAXLOG), rz1, rz2)
		} else
		    call amapr (Memr[bufptr], Memr[rtemp], ocols, z1, z2,
			rz1, rz2)
		call achtrc (Memr[rtemp], Memc[packed], ocols)
		call chrpak (Memc[packed], 1, Memc[packed], 1, ocols)
		call write (rfd, Memc[packed], ocols / SZB_CHAR)
	    }
	    call sfree (sp)
	}

	call sigln_free (si)
end

