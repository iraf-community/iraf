include <mach.h>
include <fset.h>
include <evvexpr.h>
include "../export.h"
include "../exbltins.h"


define	SZ_GIFSTRUCT	30

define	GIF_INIT_BITS	Memi[$1]	# initial number of bits
define	GIF_MAXCODE	Memi[$1+1]	# max output code
define	GIF_FREE_ENT	Memi[$1+2]	# first unused entry
define	GIF_OFFSET	Memi[$1+3]	# offset into output buffer
define	GIF_IN_COUNT	Memi[$1+4]	# length of input
define	GIF_CUR_BITS	Memi[$1+5]	# current no. bits in code
define	GIF_N_BITS	Memi[$1+6]	# no. of max bits
define	GIF_CUR_ACCUM	Memi[$1+7]	# current accumulator
define	GIF_A_COUNT	Memi[$1+8]	# no. of chars in 'packet'
define	GIF_CLEAR_CODE	Memi[$1+9]	# clear hash table code
define	GIF_EOF_CODE	Memi[$1+10]	# EOF code
define	GIF_CLEAR_FLAG	Memi[$1+11]	# hash table has been cleared?
define	GIF_CURX	Memi[$1+12]	# current 'x' position in image
define	GIF_CURY	Memi[$1+13]	# current 'y' position in image
define	GIF_PASS	Memi[$1+14]	# interlacing pass number
define	GIF_WIDTH	Memi[$1+15]	# width of output image
define	GIF_HEIGHT	Memi[$1+16]	# height of output image
define	GIF_EXPNUM	Memi[$1+17]	# expression we're evaluating
define	GIF_LNUM	Memi[$1+18]	# line w/in that expression
define	GIF_NPIX	Memi[$1+19]	# no. of pixels to process
define	GIF_PERCENT	Memi[$1+20]	# percent of file completed

define	GIF_CDPTR	Memi[$1+25]	# compressed data (ptr)
define	GIF_HPTR	Memi[$1+26]	# hash table (ptr)
define	GIF_APTR	Memi[$1+27]	# packet accumulator (ptr)
define	GIF_DPTR	Memi[$1+28]	# expression data (ptr)
define	GIF_CPTR	Memi[$1+29]	# code table (ptr)

define	ACCUM		Mems[GIF_APTR($1)+$2]
define	HTAB		Memi[GIF_HPTR($1)+$2]
define	CODETAB		Memi[GIF_CPTR($1)+$2]
define	DATA		Mems[GIF_DPTR($1)+$2-1]
define	CDATA		Mems[GIF_CDPTR($1)+$2]

define	HSIZE		5003		# 80% occupancy
define	USE_INTERLACE	true		# Write interlaced GIF files?

#----------------------------------------------------------------------------
define	INTERLACE	040X		# Image descriptor flags
define	GLOBAL_COLORMAP	080X
define	LOCAL_COLORMAP	080X		# (currently unused)

# Define the flags for the GIF89a extension blocks (currently unused).
define	GE_PLAINTEXT	001X		# Plain Text Extension
define	GE_APPLICATION	0FFX		# Application Extension
define	GE_COMMENT	0FEX		# Comment Extension
define	GE_GCONTROL	0F9X		# Graphics Control Extension


# EX_GIF - Write the output image to a GIF 87a file.

procedure ex_gif (ex)

pointer	ex				#i task struct pointer

pointer	gif
int	nbytes, flags

char	ch[2]
int	or()

begin
        # Check to see that we have the correct number of expressions to
        # write this format.
        flags = EX_OUTFLAGS(ex)
        if (EX_NEXPR(ex) != 1 && !bitset(flags, OF_BAND))
            call error (7, "Invalid number of expressions for GIF file.")
        if (bitset(flags, OF_LINE) || bitset (flags, LINE_STORAGE))
            call error (7, "Line storage illegal for GIF file.")

        # Fix the output pixel type to single bytes.
        call ex_do_outtype (ex, "b1")
        EX_OUTFLAGS(ex) = or (EX_OUTFLAGS(ex), OF_FLIPY)

	# Allocate the gif structure.
	iferr {
	    call calloc (gif, SZ_GIFSTRUCT, TY_STRUCT)
	    call calloc (GIF_APTR(gif), 257, TY_SHORT)
	    call calloc (GIF_HPTR(gif), HSIZE, TY_INT)
	    call calloc (GIF_CPTR(gif), HSIZE, TY_INT)
	    call calloc (GIF_DPTR(gif), max(256,EX_OCOLS(ex)), TY_SHORT)
	    call calloc (GIF_CDPTR(gif), (2*EX_OROWS(ex)*EX_OCOLS(ex)),TY_SHORT)
	} then
	    call error (0, "Error allocating gif structure.")

	GIF_WIDTH(gif) = EX_OCOLS(ex)
	GIF_HEIGHT(gif) = EX_OROWS(ex)
	GIF_NPIX(gif) = EX_OROWS(ex) * EX_OCOLS(ex)
	GIF_CURX(gif) = 1
	GIF_CURY(gif) = 0
	GIF_PASS(gif) = 1
	GIF_EXPNUM(gif) = EX_NEXPR(ex)
	GIF_LNUM(gif) = GIF_HEIGHT(gif)

	# Write the header information.
	call gif_wheader (ex, EX_FD(ex))

	# Start processing the expressions and write compressed image data.
	call gif_compress (ex, gif, EX_FD(ex))

	# Write the GIF file terminator and dump the whole thing to disk.
	if (mod(GIF_OFFSET(gif),2) == 1) {
	    CDATA(gif,GIF_OFFSET(gif)) = '\0'
 	    GIF_OFFSET(gif) = GIF_OFFSET(gif) + 1
	    ch[1] = ';'
	    ch[2] = ';'
	    nbytes = (GIF_OFFSET(gif) + 1) / SZB_CHAR
	} else {
	    ch[1] = '\0'
	    ch[2] = ';'
	    nbytes = GIF_OFFSET(gif) / SZB_CHAR
	}
	call achtsb (CDATA(gif,0), CDATA(gif,0), GIF_OFFSET(gif))
	call write (EX_FD(ex), CDATA(gif,0), nbytes)
	call achtsb (ch, ch, 2)
	call write (EX_FD(ex), ch, 1)

	# Clean up the pointers.
	call mfree (GIF_APTR(gif), TY_SHORT)
	call mfree (GIF_DPTR(gif), TY_SHORT)
	call mfree (GIF_CDPTR(gif), TY_SHORT)
	call mfree (GIF_HPTR(gif), TY_INT)
	call mfree (GIF_CPTR(gif), TY_INT)
	call mfree (gif, TY_STRUCT)
end


# GIF_WHEADER - Write the GIF header information.  This covers not only the
# global file header but all the preliminary stuff up until the actual image
# data

procedure gif_wheader (ex, fd)

pointer	ex				#i tast struct pointer
int	fd				#i output file descriptor

char	sig[7]				# GIF signature
char	lsd[772]			# Screen and Color Map information
short	SWidth, SHeight			# Screen width and height

short	stmp
int	i, j

int	shifti(), ori()

define	GIF_SIGNATURE	"GIF87a"

begin
	fd = EX_FD(ex)

	# Write the GIF signature.  This is technically the "header", following
	# this are the scene/color/image descriptors.
	call strcpy (GIF_SIGNATURE, sig, 7)
	call strpak (sig, sig, 7)
	call write (fd, sig, 7/SZB_CHAR)

	# Logical Screen Descriptor.
	SWidth = EX_OCOLS(ex)
	SHeight = EX_OROWS(ex)
	call gif_putword (fd, SWidth)
	call gif_putword (fd, SHeight)

	# Set the 'packed' flags and write it out
	i = 0
	i = ori (i, GLOBAL_COLORMAP) 	# indicate a colormap
	i = ori (i, (shifti(7, 4)))     # color resolution
	i = ori (i, (8-1)) 		# bits per pixel
	lsd[1] = i			# packed flags
	lsd[2] = 0			# background color
	lsd[3] = 0			# aspect ratio
	lsd[4] = 0			# filler expansion byte

	# Write out the colormap.
	if (EX_CMAP(ex) != NULL) {
	    j = 1
	    for (i=4 ; i <= 772; i=i+3) {
	        lsd[i  ] = CMAP(EX_CMAP(ex), EX_RED,   j)
	        lsd[i+1] = CMAP(EX_CMAP(ex), EX_GREEN, j)
	        lsd[i+2] = CMAP(EX_CMAP(ex), EX_BLUE,  j)
	        j = j + 1
	    }
	} else {
	    j = 0
	    for (i=4 ; i <= 772; i=i+3) {
	        lsd[i  ] = j
	        lsd[i+1] = j
	        lsd[i+2] = j
	        j = j + 1
	    }
	}
	lsd[772] = ','
	call achtcb (lsd, lsd, 772)
	call write (fd, lsd, 772/SZB_CHAR)

	# Write the image header.
	stmp = 0
	call gif_putword (fd, stmp)
	call gif_putword (fd, stmp)
	call gif_putword (fd, SWidth)
	call gif_putword (fd, SHeight)

	# Next set the interlace flag and the initial code size in the next
	# two bytes.
	if (USE_INTERLACE)
	    stmp = ori (shifti(INTERLACE,8), 8)
	else
	    stmp = 8
        if (BYTE_SWAP2 == YES)
            call bswap2 (stmp, 1, stmp, 1, 2)
	call write (fd, stmp, 1)
end


# GIF_COMPRESS - Compress the image data using a modified LZW.

procedure gif_compress (ex, gif, fd)

pointer	ex				#i tast struct pointer
pointer	gif				#i gif struct pointer
int	fd				#i output file descriptor

long	fcode
int	i, c, ent, disp
int	hsize_reg, hshift

short	gif_next_pixel()
int	xori(), shifti()

define	probe_		99
define	nomatch_	98

begin
	GIF_INIT_BITS(gif)  = 9		# initialize
	GIF_N_BITS(gif)     = 9
	GIF_OFFSET(gif)     = 0
	GIF_CLEAR_FLAG(gif) = NO
	GIF_IN_COUNT(gif)   = 1
	GIF_MAXCODE(gif)    = 511
	GIF_CLEAR_CODE(gif) = 256
	GIF_EOF_CODE(gif)   = GIF_CLEAR_CODE(gif) + 1
	GIF_FREE_ENT(gif)   = GIF_CLEAR_CODE(gif) + 2
	GIF_A_COUNT(gif)    = 0

	ent = gif_next_pixel (ex, gif)
	hshift = 0
	for (fcode = HSIZE; fcode < 65536 ; fcode = fcode * 2)
	    hshift = hshift + 1
	hshift = 8-hshift		# set hash code range bound

	hsize_reg = HSIZE		# clear the hash table
	call amovki (-1, HTAB(gif,0), HSIZE)

	call gif_output (fd, gif, GIF_CLEAR_CODE(gif))

	# Now loop over the pixels.
	repeat {
	    c = gif_next_pixel (ex, gif)
	    if (c == EOF)
		break
	    GIF_IN_COUNT(gif) = GIF_IN_COUNT(gif) + 1

	    fcode = shifti (c, 12) + ent
	    i = xori (shifti (c, hshift), ent)

	    if (HTAB(gif,i) == fcode) {
		ent = CODETAB(gif,i)
		next
	    } else if (HTAB(gif,i) < 0)		# empty slot
		goto nomatch_
	    disp = hsize_reg - i	# secondary hash (after G. Knott)
	    if (i == 0)
		disp = 1

probe_	    i = i - disp
	    if (i < 0)
		i = i + hsize_reg

	    if (HTAB(gif,i) == fcode) {
		ent = CODETAB(gif,i)
		next
	    }
	    if (HTAB(gif,i) >= 0)
		goto probe_

nomatch_    call gif_output (fd, gif, ent)
	    ent = c
	    if (GIF_FREE_ENT(gif) < 4096) {
		CODETAB(gif,i) = GIF_FREE_ENT(gif)
		GIF_FREE_ENT(gif) = GIF_FREE_ENT(gif) + 1
		HTAB(gif,i) = fcode
	    } else {
		# Clear out the hash table.
		call amovki (-1, HTAB(gif,0), HSIZE)
		GIF_FREE_ENT(gif) = GIF_CLEAR_CODE(gif) + 2
		GIF_CLEAR_FLAG(gif) = YES
		call gif_output (fd, gif, GIF_CLEAR_CODE(gif))
	    }
	}

	# Write out the final code.
	call gif_output (fd, gif, ent)
	call gif_output (fd, gif, GIF_EOF_CODE(gif))
end


# GIF_NEXT_PIXEL - Writes a 16-bit integer in GIF order (LSB first).

short procedure gif_next_pixel (ex, gif)

pointer	ex				#i tast struct pointer
pointer	gif				#i gif struct pointer

short	pix
pointer	op, out
pointer	ex_chtype(), ex_evaluate()

begin
	if (GIF_NPIX(gif) == 0)
	    return (EOF)

	# If the current X position is at the start of a line get the new
	# data, otherwise just return what we already know.
	pix = 1
	if (GIF_CURX(gif) == 1) {
	    call ex_getpix (ex, GIF_LNUM(gif))
	    op = ex_evaluate (ex, O_EXPR(ex,GIF_EXPNUM(gif)))
	    out = ex_chtype (ex, op, TY_UBYTE)
	    call aclrs (DATA(gif,1), O_LEN(op))
	    call achtbu (Memc[out], DATA(gif,1), O_LEN(op))
	    call mfree (out, TY_CHAR)
	    call evvfree (op)
	}
	pix = DATA(gif,GIF_CURX(gif))

	# Increment the position.
	if (GIF_CURY(gif) == EX_OROWS(ex)) {
	    GIF_CURX(gif) = min (EX_OCOLS(ex), GIF_CURX(gif) + 1)
	} else
	    call gif_bump_pixel (ex, gif)

	GIF_NPIX(gif) = GIF_NPIX(gif) - 1
	return (pix)
end


# GIF_BUMP_PIXEL - Update the current x and y values for interlacing.

procedure gif_bump_pixel (ex, gif)

pointer	ex				#i tast struct pointer
pointer gif                             #i gif struct pointer

int	i, row, sum

begin
	GIF_CURX(gif) = GIF_CURX(gif) + 1

        # If we are at the end of a scan line, set curx back to the beginning
        # Since we are interlaced, bump the cury to the appropriate spot.

        if (GIF_CURX(gif) > GIF_WIDTH(gif)) {
            GIF_CURX(gif) = 1

	    if (USE_INTERLACE) {
                switch (GIF_PASS(gif)) {
                case 1:
                    GIF_CURY(gif) = GIF_CURY(gif) + 8
                    if (GIF_CURY(gif) >= GIF_HEIGHT(gif)) {
                        GIF_PASS(gif) = GIF_PASS(gif) + 1
                        GIF_CURY(gif) = 4
                    }
                case 2:
                    GIF_CURY(gif) = GIF_CURY(gif) + 8
                    if (GIF_CURY(gif) >= GIF_HEIGHT(gif)) {
                        GIF_PASS(gif) = GIF_PASS(gif) + 1
                        GIF_CURY(gif) = 2
                    }
                case 3:
                    GIF_CURY(gif) = GIF_CURY(gif) + 4
                    if (GIF_CURY(gif) >= GIF_HEIGHT(gif)) {
                        GIF_PASS(gif) = GIF_PASS(gif) + 1
                        GIF_CURY(gif) = 1
                    }
                case 4:
                    GIF_CURY(gif) = GIF_CURY(gif) + 2
                    if (GIF_CURY(gif) >= GIF_HEIGHT(gif)) {
		        GIF_EXPNUM(gif) = EX_NEXPR(ex)
		        GIF_LNUM(gif) = EX_OROWS(ex)
                        GIF_CURY(gif) = GIF_HEIGHT(gif)
		        return
		    }
                }

	        # Now figure out where we are in the expressions.
	        i = EX_NEXPR(ex)
	        sum = GIF_HEIGHT(gif)
	        while (sum >= GIF_CURY(gif)) {
		    sum = sum - O_HEIGHT(ex,i)
		    i = i - 1
	        }
	        GIF_EXPNUM(gif) = i + 1
	        GIF_LNUM(gif)   = (sum + O_HEIGHT(ex,i+1)) - GIF_CURY(gif) + 1

		row = ((EX_OROWS(ex) * EX_OCOLS(ex)) - GIF_NPIX(gif)) / 
		    EX_OCOLS(ex)
	        #if (EX_VERBOSE(ex) == YES)
	            call ex_pstat (ex, row, GIF_PERCENT(gif))

            } else {
		GIF_CURY(gif) = GIF_CURY(gif) + 1

	        # Now figure out where we are in the expressions.
	        i = EX_NEXPR(ex)
	        sum = GIF_HEIGHT(gif)
	        while (sum >= GIF_CURY(gif)) {
		    sum = sum - O_HEIGHT(ex,i)
		    i = i - 1
	        }

	        if ((i+1) == GIF_EXPNUM(gif)) {
	            GIF_LNUM(gif)   = GIF_LNUM(gif) - 1
	        } else {
	            GIF_EXPNUM(gif) = i + 1
	            GIF_LNUM(gif)   = O_HEIGHT(ex,i+1)
	        }

	        #if (EX_VERBOSE(ex) == YES)
	            call ex_pstat (ex, GIF_CURY(gif), GIF_PERCENT(gif))
            }
        }
end


# GIF_OUTPUT - Output the given code.

procedure gif_output (fd, gif, code)

int	fd				#i output file descriptor
pointer	gif				#i gif struct pointer
int	code				#i code to output

long	masks[17]
int	i

int	ori(), andi(), shifti()

data 	(masks(i), i=1,5)    /00000X, 00001X, 00003X, 00007X, 0000FX/
data	(masks(i), i=6,9)            /0001FX, 0003FX, 0007FX, 000FFX/
data	(masks(i), i=10,13)          /001FFX, 003FFX, 007FFX, 00FFFX/
data	(masks(i), i=14,17)          /01FFFX, 03FFFX, 07FFFX, 0FFFFX/

begin
	GIF_CUR_ACCUM(gif) = andi(GIF_CUR_ACCUM(gif),masks[GIF_CUR_BITS(gif)+1])

	if (GIF_CUR_BITS(gif) > 0)
	    GIF_CUR_ACCUM(gif) = ori (GIF_CUR_ACCUM(gif), 
		shifti (code, GIF_CUR_BITS(gif)))
	else
	    GIF_CUR_ACCUM(gif) = code
	GIF_CUR_BITS(gif) = GIF_CUR_BITS(gif) + GIF_N_BITS(gif)

	while (GIF_CUR_BITS(gif) >= 8) {
	    call char_out (fd, gif, andi (GIF_CUR_ACCUM(gif), 0FFX))
	    GIF_CUR_ACCUM(gif) = shifti (GIF_CUR_ACCUM(gif), -8)
	    GIF_CUR_BITS(gif) = GIF_CUR_BITS(gif) - 8
	}

	# If the next entry is going to be too big for the code size then
	# increase it if possible.
	if (GIF_FREE_ENT(gif) > GIF_MAXCODE(gif) || GIF_CLEAR_FLAG(gif)==YES) {
	    if (GIF_CLEAR_FLAG(gif) == YES) {
		GIF_MAXCODE(gif) = 511
		GIF_N_BITS(gif) = 9
		GIF_CLEAR_FLAG(gif) = NO
	    } else {
		GIF_N_BITS(gif) = GIF_N_BITS(gif) + 1
	  	if (GIF_N_BITS(gif) == 12)
		    GIF_MAXCODE(gif) = 4096
		else
		    GIF_MAXCODE(gif) = shifti (1, GIF_N_BITS(gif)) - 1
	    }
	}

	if (code == GIF_EOF_CODE(gif)) {
	    # At EOF, write the rest of the buffer.
	    while (GIF_CUR_BITS(gif) >= 8) {
	        call char_out (fd, gif, andi (GIF_CUR_ACCUM(gif), 0FFX))
	        GIF_CUR_ACCUM(gif) = shifti (GIF_CUR_ACCUM(gif), -8)
	        GIF_CUR_BITS(gif) = GIF_CUR_BITS(gif) - 8
	    }

	    call flush_char (gif)
	    call flush (fd)
	}
end


# GIF_PUTWORD - Writes a 16-bit integer in GIF order (LSB first).

procedure gif_putword (fd, w)

int	fd
short	w

short 	val
int	tmp, shifti()

begin
	# If this is a MSB-first machine swap the bytes before output.
	if (BYTE_SWAP2 == NO) {
            call bitpak (int(w), tmp, 9, 8)
            call bitpak (shifti(int(w),-8), tmp, 1, 8)
	    val = tmp
	} else
	    val = w

	call write (fd, val, SZ_SHORT/SZ_CHAR)
end


procedure char_out (fd, gif, c)

int	fd				#i output file descriptor
pointer	gif				#i gif struct pointer
int	c				#i char to output

begin
	ACCUM(gif,GIF_A_COUNT(gif)) = c
	GIF_A_COUNT(gif) = GIF_A_COUNT(gif) + 1
	if (GIF_A_COUNT(gif) >= 254)
	    call flush_char (gif)
end


procedure flush_char (gif)

pointer	gif				#i gif struct pointer

begin
	if (GIF_A_COUNT(gif) > 0) {
	    CDATA(gif,GIF_OFFSET(gif)) = GIF_A_COUNT(gif)
	    GIF_OFFSET(gif) = GIF_OFFSET(gif) + 1
	    call amovs (ACCUM(gif,0), CDATA(gif,GIF_OFFSET(gif)),
		GIF_A_COUNT(gif))
	    GIF_OFFSET(gif) = GIF_OFFSET(gif) + GIF_A_COUNT(gif)
	    GIF_A_COUNT(gif) = 0
	}
end
