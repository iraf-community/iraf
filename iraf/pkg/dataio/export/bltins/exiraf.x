include <imhdr.h>
include <mach.h>
include <evvexpr.h>
include "../export.h"


# EX_IRAF - Write the evaluated expressions back out as an IRAF image.

procedure ex_iraf (ex)

pointer	ex					#i task struct pointer

pointer	sp, imname
pointer	im, op, out
int	i, j, flags
int	line, percent, orow, type

pointer	ex_evaluate(), ex_chtype()
pointer	immap()
pointer	impl2s(), impl2i(), impl2l(), impl2r(), impl2d()
int	fnroot()

errchk	immap

begin
	# Check to see that we have the correct number of expressions.
	flags = EX_OUTFLAGS(ex)
	if (EX_NEXPR(ex) != 1 && !bitset(flags, OF_BAND))
	        call error (7, "Invalid number of expressions for IRAF image.")
	if (bitset(flags, OF_LINE) || bitset (flags, LINE_STORAGE))
	    call error (7, "Line storage illegal for IRAF image.")
	if (EX_OUTTYPE(ex) == TY_UBYTE)
	    call ex_do_outtype (ex, "u2")

	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call aclrc (Memc[imname], SZ_FNAME)

	# Since we're writing an image, close the output file descriptor
	# and instead use an image pointer.
	call close (EX_FD(ex))
	call delete (BFNAME(ex))
	EX_FD(ex) = NULL

	# Generate the image name and map it for processing.
	if (fnroot (BFNAME(ex), Memc[imname], SZ_FNAME) == 0)
	    call error (0, "Error making image name.")
	iferr (im = immap (Memc[imname], NEW_IMAGE, 0))
	    call error (0, "Error mapping output image.")

	# Set the minimal header values.
	IM_LEN(im,1) = EX_OCOLS(ex)
	IM_LEN(im,2) = EX_OROWS(ex)
	IM_NDIM(im) = 2
	IM_PIXTYPE(im) = EX_OUTTYPE(ex)

	# Finally, evaluate the expressions and write the image.
	type = EX_OUTTYPE(ex)
	percent = 0
	orow = 1
	do i = 1, EX_NEXPR(ex) {

	    # Process each line in the image.
	    do j = 1, O_HEIGHT(ex,i) {

		# See if we're flipping the image.
		if (bitset (EX_OUTFLAGS(ex), OF_FLIPY))
		    line = EX_NLINES(ex) - j + 1
		else
		    line = j

		# Get pixels from image(s).
		call ex_getpix (ex, line)

		# Evaluate expression.
		op = ex_evaluate (ex, O_EXPR(ex,i))

		# Convert to the output pixel type.
		out = ex_chtype (ex, op, type)

		# Write evaluated pixels.
		switch (type) {
		case TY_USHORT, TY_SHORT:
		    call amovs (Mems[out], Mems[impl2s(im,orow)], O_LEN(op))
		case TY_INT:
		    call amovi (Memi[out], Memi[impl2i(im,orow)], O_LEN(op))
		case TY_LONG:
		    call amovl (Meml[out], Meml[impl2l(im,orow)], O_LEN(op))
		case TY_REAL:
		    call amovr (Memr[out], Memr[impl2r(im,orow)], O_LEN(op))
		case TY_DOUBLE:
		    call amovd (Memd[out], Memd[impl2d(im,orow)], O_LEN(op))
		default:
		    call error (0, "Illegal output image type.")
		}

		# Clean up the pointers.
		call mfree (out, type)
	 	call evvfree (op)

                # Print percent done if being verbose
	 	orow = orow + 1
                #if (EX_VERBOSE(ex) == YES)
		    call ex_pstat (ex, orow, percent)
	    }
	}

	call imunmap (im)
	call sfree (sp)
end
