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

pointer	sp, cmap
long	header[SZ_RASHDR]
int	i, flags

begin
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
	if (mod (O_WIDTH(ex,1),2) != 0) {
	    do i = 1, EX_NEXPR(ex) {
		call strcat ("//repl(0,1)", O_EXPR(ex,i), SZ_EXPSTR)
		O_WIDTH(ex,i) = O_WIDTH(ex,i) + 1
	    }
	    EX_OCOLS(ex) = EX_OCOLS(ex) + 1
	}

	# Set the header values.
	header[RAS_MAGIC] = RAS_MAGIC_NUM
	header[RAS_WIDTH] = EX_OCOLS(ex)
	header[RAS_HEIGHT] = EX_OROWS(ex)
	header[RAS_TYPE] = RT_STANDARD
	if (EX_NEXPR(ex) == 1 || bitset (flags, OF_BAND)) {
	    header[RAS_LENGTH] = header[RAS_WIDTH] * header[RAS_HEIGHT]
	    header[RAS_DEPTH] = long (8)
	} else {
	    header[RAS_LENGTH] = header[RAS_WIDTH] * header[RAS_HEIGHT] * 3
	    header[RAS_DEPTH] = long (24)
	    header[RAS_TYPE] = RT_FORMAT_RGB
	}
	if (bitset(flags, OF_CMAP)) {
	    header[RAS_MAPTYPE] = RMT_EQUAL_RGB
	    header[RAS_MAPLENGTH] = long (3*CMAP_SIZE)
	} else {
	    header[RAS_MAPTYPE] = RMT_NONE
	    header[RAS_MAPLENGTH] = long (0)
	}

        # Write the header to the file.  First swap it to Sun byte order if
        # needed (although the format doesn't require this), then swap it
        # if requested by the user.
        if (BYTE_SWAP4 == YES) 
            call bswap4 (header, 1, header, 1, (SZ_RASHDR * SZ_LONG * SZB_CHAR))
        if (EX_BSWAP(ex) == S_I4) 
            call bswap4 (header, 1, header, 1, (SZ_RASHDR * SZ_LONG * SZB_CHAR))
        call write (EX_FD(ex), header, (SZ_RASHDR * SZ_LONG))

	# If we have a colormap write that out now.
	if (bitset(flags, OF_CMAP)) {
	    call smark (sp)
	    call salloc (cmap, 3*CMAP_SIZE, TY_CHAR)

	    call achtcb (Memc[EX_CMAP(ex)], Memc[cmap], (3 * CMAP_SIZE))
	    call write (EX_FD(ex), Memc[cmap], ((3 * CMAP_SIZE) / SZB_CHAR))

	    call sfree (sp)
	}

	# Finally, evaluate the expressions and write the image.
	if (EX_NEXPR(ex) == 1 || bitset (flags, OF_BAND))
	    call ex_no_interleave (ex)
	else if (EX_NEXPR(ex) == 3 || EX_NEXPR(ex) == 4)
	    call ex_px_interleave (ex)
end
