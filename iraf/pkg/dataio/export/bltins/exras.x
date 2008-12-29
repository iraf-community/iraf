include <mach.h>
include "../export.h"


# EXRAS.X - Source file for the EXPORT task rasterfile builtin format.

define	SZ_RASHDR	8
define	RAS_MAGIC	1	# Magic number
define	RAS_WIDTH	2	# Image width (pixels per line)
define	RAS_HEIGHT	3	# Image height (number of lines)
define	RAS_DEPTH	4	# Image depth (bits per pixel)
define	RAS_LENGTH	5	# Image length (bytes)
define	RAS_TYPE	6	# File type
define	RAS_MAPTYPE	7	# Colormap type
define	RAS_MAPLENGTH	8	# Colormap length (bytes)

# Rasterfile magic number
define	RAS_MAGIC_NUM	59A66A95X
define	RAS_RLE		80X

# Sun supported ras_types
define	RT_OLD		0	# Raw pixrect image in 68000 byte order
define	RT_STANDARD	1	# Raw pixrect image in 68000 byte order
define	RT_BYTE_ENCODED	2	# Run-length compression of bytes
define	RT_FORMAT_RGB	3	# XRGB or RGB instead of XBGR or BGR
define	RT_FORMAT_TIFF	4	# tiff <-> standard rasterfile
define	RT_FORMAT_IFF	5	# iff (TAAC format) <-> standard rasterfile
define	RT_EXPERIMENTAL	65535	# Reserved for testing

# Sun supported ras_maptypes
define	RMT_NONE	0	# ras_maplength is expected to be 0
define	RMT_EQUAL_RGB	1	# red[ras_maplength/3],green[],blue[]
define	RMT_RAW		2



# EX_RAS - Write the evaluated expressions as a Sun Rasterfile.

procedure ex_ras (ex)

pointer	ex					#i task struct pointer

size_t	sz_val
size_t	c_1
long	l_val
pointer	sp, cmap
int	header[SZ_RASHDR]
int	i, flags

long	lmod()

begin
	c_1 = 1
	# Check to see that we have the correct number of expressions to
	# write this format.
	flags = EX_OUTFLAGS(ex)
	if (EX_NEXPR(ex) != 1 && EX_NEXPR(ex) != 3 && EX_NEXPR(ex) != 4) {
	    if (!bitset(flags, OF_BAND))
	        call error (7, "Invalid number of expressions for rasterfile.")
	}
	if (bitset(flags, OF_LINE) || bitset (flags, LINE_STORAGE))
	    call error (7, "Line storage illegal for rasterfile.")

	# Fix the output pixel type to single bytes.
	call ex_do_outtype (ex, "b1")
	EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPY)

	# Make sure the output is padded to the nearest 16-bits.
	l_val = 2
	if (lmod(O_WIDTH(ex,1),l_val) != 0) {
	    do i = 1, EX_NEXPR(ex) {
		call strcat ("//repl(0,1)", O_EXPR(ex,i), SZ_EXPSTR)
		O_WIDTH(ex,i) = O_WIDTH(ex,i) + 1
	    }
	    EX_OCOLS(ex) = EX_OCOLS(ex) + 1
	}

	if ( MAX_INT < EX_OCOLS(ex) )
	    call error (0, "Too large OCOLS for Sun Rasterfile.")
	if ( MAX_INT < EX_OROWS(ex) )
	    call error (0, "Too large OROWS for Sun Rasterfile.")

	# Set the header values.
	header[RAS_MAGIC] = RAS_MAGIC_NUM
	header[RAS_WIDTH] = EX_OCOLS(ex)
	header[RAS_HEIGHT] = EX_OROWS(ex)
	header[RAS_TYPE] = RT_STANDARD
	if (EX_NEXPR(ex) == 1 || bitset (flags, OF_BAND)) {
	    if ( MAX_INT < EX_OCOLS(ex) * EX_OROWS(ex) )
		call error (0, "Too large data for Sun Rasterfile.")
	    header[RAS_LENGTH] = header[RAS_WIDTH] * header[RAS_HEIGHT]
	    header[RAS_DEPTH] = 8
	} else {
	    if ( MAX_INT < EX_OCOLS(ex) * EX_OROWS(ex) * 3 )
		call error (0, "Too large data for Sun Rasterfile.")
	    header[RAS_LENGTH] = header[RAS_WIDTH] * header[RAS_HEIGHT] * 3
	    header[RAS_DEPTH] = 24
	    header[RAS_TYPE] = RT_FORMAT_RGB
	}
	if (bitset(flags, OF_CMAP)) {
	    header[RAS_MAPTYPE] = RMT_EQUAL_RGB
	    header[RAS_MAPLENGTH] = 3*CMAP_SIZE
	} else {
	    header[RAS_MAPTYPE] = RMT_NONE
	    header[RAS_MAPLENGTH] = 0
	}

        # Write the header to the file.  First swap it to Sun byte order if
        # needed (although the format doesn't require this), then swap it
        # if requested by the user.
	sz_val = SZ_RASHDR * SZ_INT * SZB_CHAR
	if (BYTE_SWAP4 == YES) 
	    call bswap4 (header, c_1, header, c_1, sz_val)
	if (EX_BSWAP(ex) == S_I4) 
	    call bswap4 (header, c_1, header, c_1, sz_val)

	sz_val = SZ_RASHDR * SZ_INT
	# arg2: incompatible pointer
        call write (EX_FD(ex), header, sz_val)

	# If we have a colormap write that out now.
	if (bitset(flags, OF_CMAP)) {
	    call smark (sp)
	    sz_val = 3*CMAP_SIZE
	    call salloc (cmap, sz_val, TY_CHAR)
	    # arg2: incompatible pointer
	    call achtcb (Memc[EX_CMAP(ex)], Memc[cmap], sz_val)
	    sz_val = (3 * CMAP_SIZE) / SZB_CHAR
	    call write (EX_FD(ex), Memc[cmap], sz_val)

	    call sfree (sp)
	}

	# Finally, evaluate the expressions and write the image.
	if (EX_NEXPR(ex) == 1 || bitset (flags, OF_BAND))
	    call ex_no_interleave (ex)
	else if (EX_NEXPR(ex) == 3 || EX_NEXPR(ex) == 4)
	    call ex_px_interleave (ex)
end
