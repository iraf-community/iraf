include <mach.h>
include "../export.h"
include "../exbltins.h"


define	IMAGIC		0732B		# SGI magic number
define	BPPMASK		00FFX
define	ITYPE_VERBATIM	0001X
define	ITYPE_RLE	0100X


# EX_RGB - Write the output image to an SGI RGB format file.

procedure ex_rgb (ex)

pointer	ex				#i task struct pointer

size_t	sz_val
int	i, fd
short	imagic, type, dim		# stuff saved on disk
short	xsize, ysize, zsize, pad
int	min, max
char	name[80]

begin
        # Check to see that we have the correct number of expressions to
        # write this format.
        if (EX_NEXPR(ex) != 3)
            call error (7, "Invalid number of expressions for SGI RGB.")

	# Fix up the number of output rows.
	EX_OROWS(ex) = EX_NLINES(ex) * EX_NEXPR(ex)

	# Load the image header values
	imagic = IMAGIC
	type = ITYPE_VERBATIM
	if (EX_NEXPR(ex) >= 3 && !bitset (EX_OUTFLAGS(ex),OF_BAND)) {
	    dim = 3
	    zsize = 3
	} else {
	    dim = 2
	    zsize = 1
	}
	if ( MAX_SHORT < EX_OCOLS(ex) )
	    call error (0, "Too large OCOLS for SGI RGB.")
	if ( MAX_SHORT < EX_NLINES(ex) )
	    call error (0, "Too large NLINES for SGI RGB.")
	xsize = EX_OCOLS(ex)
	ysize = EX_NLINES(ex)
	min = 0
	max = 255
	sz_val = 80
	call aclrc (name, sz_val)
	call strcpy ("no name", name, 80)
	# arg2: incompatible pointer
	call achtcb (name, name, sz_val)

	# Write the header values to the output file.
	fd = EX_FD(ex)
	sz_val = SZ_SHORT / SZ_CHAR
	call write (fd, imagic, sz_val)
	call write (fd, type, sz_val)
	call write (fd, dim, sz_val)
	call write (fd, xsize, sz_val)
	call write (fd, ysize, sz_val)
	call write (fd, zsize, sz_val)
	sz_val = SZ_INT / SZ_CHAR
	# arg2: incompatible pointer
	call write (fd, min, sz_val)
	# arg2: incompatible pointer
	call write (fd, max, sz_val)
	# arg2: incompatible pointer
	call write (fd, 0, sz_val)
	sz_val = 8 / SZB_CHAR
	call write (fd, name, sz_val)

	# Pad to a 512 byte header.
	pad = 0
	sz_val = SZ_SHORT / SZ_CHAR
	do i = 1, 240
	    call write (fd, pad, sz_val)

	# Fix the output parameters.
	call ex_do_outtype (ex, "b1")

	# Write it out.
	call ex_no_interleave (ex)
end
