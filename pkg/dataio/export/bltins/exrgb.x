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

int	i, fd
short	imagic, type, dim		# stuff saved on disk
short	xsize, ysize, zsize, pad
long	min, max
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
	xsize = EX_OCOLS(ex)
	ysize = EX_NLINES(ex)
	min = 0
	max = 255
	call aclrc (name, 80)
	call strcpy ("no name", name, 80)
	call achtcb (name, name, 80)

	# Write the header values to the output file.
	fd = EX_FD(ex)
	call write (fd, imagic, SZ_SHORT / SZ_CHAR)
	call write (fd, type, SZ_SHORT / SZ_CHAR)
	call write (fd, dim, SZ_SHORT / SZ_CHAR)
	call write (fd, xsize, SZ_SHORT / SZ_CHAR)
	call write (fd, ysize, SZ_SHORT / SZ_CHAR)
	call write (fd, zsize, SZ_SHORT / SZ_CHAR)
	call write (fd, min, SZ_LONG / SZ_CHAR)
	call write (fd, max, SZ_LONG / SZ_CHAR)
	call write (fd, 0, SZ_LONG / SZ_CHAR)
	call write (fd, name, 8 / SZB_CHAR)

	# Pad to a 512 byte header.
	pad = 0
	do i = 1, 240
	    call write (fd, pad, SZ_SHORT / SZ_CHAR)

	# Fix the output parameters.
	call ex_do_outtype (ex, "b1")

	# Write it out.
	call ex_no_interleave (ex)
end
