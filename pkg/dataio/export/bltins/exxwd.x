include <mach.h>
include "../export.h"
include "../exbltins.h"


define 	X11WD_FILE_VERSION 	7	# XWD version
define 	SZ_XWD 			25	# number of header elements
define 	SZ_XWDHEADER 		100	# size of header record (bytes)

# Define the header structure.
define	X_HEADER_SIZE 	     Meml[$1]  		# Size of the header (bytes)
define	X_FILE_VERSION 	     Meml[$1+1]		# XWD_FILE_VERSION 
define	X_PIXMAP_FORMAT      Meml[$1+2]		# Pixmap format 
define	X_PIXMAP_DEPTH 	     Meml[$1+3]		# Pixmap depth 
define	X_PIXMAP_WIDTH 	     Meml[$1+4]		# Pixmap width 
define	X_PIXMAP_HEIGHT      Meml[$1+5]		# Pixmap height 
define	X_XOFFSET 	     Meml[$1+6]		# Bitmap x offset 
define	X_BYTE_ORDER 	     Meml[$1+7]		# MSBFirst, LSBFirst 
define	X_BITMAP_UNIT 	     Meml[$1+8]		# Bitmap unit 
define	X_BITMAP_BIT_ORDER   Meml[$1+9]		# MSBFirst, LSBFirst 
define	X_BITMAP_PAD 	     Meml[$1+10]	# Bitmap scanline pad 
define	X_BITS_PER_PIXEL     Meml[$1+11]	# Bits per pixel 
define	X_BYTES_PER_LINE     Meml[$1+12]	# Bytes per scanline 
define	X_VISUAL_CLASS 	     Meml[$1+13]	# Class of colormap 
define	X_RED_MASK 	     Meml[$1+14]	# Z red mask 
define	X_GREEN_MASK 	     Meml[$1+15]	# Z green mask 
define	X_BLUE_MASK 	     Meml[$1+16]	# Z blue mask 
define	X_BITS_PER_RGB 	     Meml[$1+17]	# Log2 of distinct color values 
define	X_COLORMAP_ENTRIES   Meml[$1+18]	# Number of entries in colormap 
define	X_NCOLORS 	     Meml[$1+19]	# Number of Color structures 
define	X_WINDOW_WIDTH 	     Meml[$1+20]	# Window width 
define	X_WINDOW_HEIGHT      Meml[$1+21]	# Window height 
define	X_WINDOW_X 	     Meml[$1+22]	# Window upper left X coordinate
define	X_WINDOW_Y 	     Meml[$1+23]	# Window upper left Y coordinate
define	X_WINDOW_BDRWIDTH    Meml[$1+24]	# Window border width 

define 	LSBFirst        	0		# Byte order flags
define 	MSBFirst        	1
	
define 	XYBitmap        	0		# Pixmap types
define 	XYPixmap        	1
define 	ZPixmap         	2

define 	StaticGray      	0		# Recognized visuals
define 	GrayScale       	1
define 	StaticColor     	2
define 	PseudoColor     	3
define 	TrueColor       	4
define 	DirectColor     	5

define	DEBUG			false


# EX_XWD - Write the output image to an X11 Window Dump file.

procedure ex_xwd (ex)

pointer	ex				#i task struct pointer

pointer	xwd, cmap
char	cflags, fname[SZ_FNAME]
int	i, fd, flags
long	pixel
short	r, g, b, val

int	strlen()

begin
        # Check to see that we have the correct number of expressions to
        # write this format.
        flags = EX_OUTFLAGS(ex)
	fd = EX_FD(ex)
        if (EX_NEXPR(ex) != 1 && EX_NEXPR(ex) != 3 && EX_NEXPR(ex) != 4) {
            if (!bitset(flags, OF_BAND))
                call error (7, "Invalid number of expressions for XWD.")
        }
        if (bitset(flags, OF_LINE) || bitset (flags, LINE_STORAGE))
            call error (7, "Line storage illegal for XWD.")

        # Fix the output pixel type to single bytes.
        call ex_do_outtype (ex, "b1")
        EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPY)

	# Allocate space for the header.
	iferr (call calloc (xwd, SZ_XWD, TY_STRUCT))
	    call error (0, "Error allocate XWD structure.")

	# Set up the header values.
	flags = EX_OUTFLAGS(ex)
	X_HEADER_SIZE(xwd) = SZ_XWDHEADER + strlen ("xwddump") + 1
	X_FILE_VERSION(xwd) = X11WD_FILE_VERSION
	X_PIXMAP_FORMAT(xwd) = ZPixmap
	X_PIXMAP_WIDTH(xwd) = EX_OCOLS(ex)
	X_PIXMAP_HEIGHT(xwd) = EX_OROWS(ex)
	X_XOFFSET(xwd) = 0
	X_BYTE_ORDER(xwd) = MSBFirst
	X_BITMAP_BIT_ORDER(xwd) = MSBFirst
	X_WINDOW_WIDTH(xwd) = EX_OCOLS(ex)
	X_WINDOW_HEIGHT(xwd) = EX_OROWS(ex)
	X_WINDOW_X(xwd) = 0
	X_WINDOW_Y(xwd) = 0
	X_WINDOW_BDRWIDTH(xwd) = 0

    	if (EX_NEXPR(ex) >= 3) {
	    if (DEBUG)  call eprintf ("We think this is a DirectColor image.\n")
            X_PIXMAP_DEPTH(xwd) = 24
            X_BITMAP_UNIT(xwd) = 32
            X_BITMAP_PAD(xwd) = 32
            X_BITS_PER_PIXEL(xwd) = 32
            X_VISUAL_CLASS(xwd) = DirectColor
            X_COLORMAP_ENTRIES(xwd) = 256
            X_NCOLORS(xwd) = 0
            X_RED_MASK(xwd) = 0FF0000X
            X_GREEN_MASK(xwd) = 0FF00X
            X_BLUE_MASK(xwd) = 0FFX
            X_BYTES_PER_LINE(xwd) = EX_OCOLS(ex) * 4
        } else if (bitset (flags, OF_CMAP)) {
	    if (DEBUG)  call eprintf ("We think this has a colormap.\n")
            X_PIXMAP_DEPTH(xwd) = 8
            X_BITS_PER_PIXEL(xwd) = 8
            X_COLORMAP_ENTRIES(xwd) = EX_NCOLORS(ex)
            X_NCOLORS(xwd) = EX_NCOLORS(ex)
            X_BYTES_PER_LINE(xwd) = EX_OCOLS(ex)

            X_BITMAP_UNIT(xwd) = 8
            X_BITMAP_PAD(xwd) = 8
            X_VISUAL_CLASS(xwd) = StaticGray
            X_RED_MASK(xwd) = 0
            X_GREEN_MASK(xwd) = 0
            X_BLUE_MASK(xwd) = 0
        } else {
	    if (DEBUG)  call eprintf ("Pseudocolor.\n")
            X_PIXMAP_DEPTH(xwd) = 8
            X_BITS_PER_PIXEL(xwd) = 8
            X_VISUAL_CLASS(xwd) = PseudoColor
            X_COLORMAP_ENTRIES(xwd) = 255 + 1
            X_NCOLORS(xwd) = EX_NCOLORS(ex)
            X_RED_MASK(xwd) = 0
            X_GREEN_MASK(xwd) = 0
            X_BLUE_MASK(xwd) = 0
            X_BYTES_PER_LINE(xwd) = EX_OCOLS(ex)
            X_BITMAP_UNIT(xwd) = 8
            X_BITMAP_PAD(xwd) = 8
        }
        X_BITS_PER_RGB(xwd) = X_PIXMAP_DEPTH(xwd)

	# See if we need to byte swap in order to get MSB byte ordering.
	if (BYTE_SWAP4 == YES) 
	    call bswap4 (Meml[xwd], 1, Meml[xwd], 1, SZ_XWDHEADER)
	if (EX_BSWAP(ex) == S_I4) 
	    call bswap4 (Meml[xwd], 1, Meml[xwd], 1, SZ_XWDHEADER)
	call write (fd, Meml[xwd], SZ_XWDHEADER/SZB_CHAR)
	call strpak ("xwddump\0", fname, 8)
	call write (fd, fname, 4)

	# If we have a colormap set up the structure and write it out.
	if (bitset (flags, OF_CMAP)) {
	    cmap = EX_CMAP(ex)
	    cflags = 0
	    do i = 1, EX_NCOLORS(ex) {
		pixel = i - 1
		r = CMAP(cmap,EX_RED,i) * 65535 / 256
		g = CMAP(cmap,EX_GREEN,i) * 65535 / 256
		b = CMAP(cmap,EX_BLUE,i) * 65535 / 256

		call xwd_putlong (ex, fd, pixel)
		call xwd_putword (ex, fd, r)
		call xwd_putword (ex, fd, g)
		call xwd_putword (ex, fd, b)
		call xwd_putword (ex, fd, cflags)
	    }
	} else if (EX_NEXPR(ex) < 3) {
	    do i = 0, 255 {
		val = i * 65535 / 256
		call xwd_putlong (ex, fd, long(i))
		call xwd_putword (ex, fd, val)
		call xwd_putword (ex, fd, val)
		call xwd_putword (ex, fd, val)
		val = 0 #shifti (7, 8)
		call xwd_putword (ex, fd, val)
	    }
	}

        # Finally, evaluate the expressions and write the image.
        if (EX_NEXPR(ex) == 1 || bitset (flags, OF_BAND))
            call ex_no_interleave (ex)
        else if (EX_NEXPR(ex) == 3) {
	    # If all they gave were the RGB values we need to patch the
	    # outbands expressions to stick in an alpha channel.  Patch it
	    # up here.

	    call ex_alloc_outbands (OBANDS(ex,4))
	    do i = 4, 2, -1 {
		call strcpy (O_EXPR(ex,i-1), O_EXPR(ex,i), SZ_EXPSTR)
		O_WIDTH(ex,i) = O_WIDTH(ex,i-1)
		O_HEIGHT(ex,i) = O_HEIGHT(ex,i-1)
	    }
	    call strcpy ("0", O_EXPR(ex,1), SZ_EXPSTR)
	    EX_NEXPR(ex) = 4
            call ex_px_interleave (ex)

        } else if (EX_NEXPR(ex) >= 3) 
            call ex_px_interleave (ex)

	# Clean up.
	call mfree (xwd, TY_STRUCT)
end


# XWD_PUTWORD - Writes a 16-bit integer in XWD order (MSB first).

procedure xwd_putword (ex, fd, w)

pointer	ex				#i task struct pointer
int	fd
short	w

short 	val

begin
	# If this is a MSB-first machine swap the bytes before output.
	if (BYTE_SWAP2 == YES)
	    call bswap2 (w, 1, val, 1, (SZ_SHORT * SZB_CHAR))
	else
	    val = w
	if (EX_BSWAP(ex) == S_I2)
	    call bswap2 (val, 1, val, 1, (SZ_SHORT * SZB_CHAR))

	call write (fd, val, SZ_SHORT/SZ_CHAR)
end


# XWD_PUTLONG - Writes a 32-bit integer in XWD order (MSB first).

procedure xwd_putlong (ex, fd, w)

pointer	ex				#i task struct pointer
int	fd
long	w

long 	val

begin
	# If this is a MSB-first machine swap the bytes before output.
	if (BYTE_SWAP4 == YES)
	    call bswap4 (w, 1, val, 1, (SZ_LONG * SZB_CHAR))
	else
	    val = w
	if (EX_BSWAP(ex) == S_I4)
	    call bswap4 (val, 1, val, 1, (SZ_LONG * SZB_CHAR))

	call write (fd, val, SZ_LONG/SZ_CHAR)
end
