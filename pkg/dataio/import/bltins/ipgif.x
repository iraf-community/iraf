include "../import.h"


# IPGIF.X - Source file for the GIF builtin format converter.


# Define the GIF data structure
define	MAX_CODE_ENTRIES	4096		# because LZW has 12 bit max
define	SZ_GIFSTRUCT		35
define	SZ_GIFCODE		280
define	SZ_GIFEXTN		256
define	SZ_GIFSTACK		(2*MAX_CODE_ENTRIES+2)
define	SZ_GIFCTAB		(2*MAX_CODE_ENTRIES+2)

define	GIF_FD			Memi[$1]	# GIF file descriptor
define	GIF_WIDTH		Memi[$1+1]	# Screen width
define	GIF_HEIGHT		Memi[$1+2]	# Screen height
define	GIF_CP			Memi[$1+3]	# Colormap pointer
define	GIF_BITPIX		Memi[$1+4]	# Bits per pixel
define	GIF_COLRES		Memi[$1+5]	# Color resolution
define	GIF_BACKGROUND		Memi[$1+6]	# background color (unused?)
define	GIF_ASPECT		Memi[$1+7]	# Aspect ratio
define	GIF_IMNUM		Memi[$1+8]	# Image number
define	GIF_CMAP		Memi[$1+9]	# Global colormap (ptr)

define	GIF_EXTBP		Memi[$1+10]	# Extension buffer (ptr)
define	GIF_CODEP		Memi[$1+11]	# Code table buffer (ptr)
define	GIF_CTABP		Memi[$1+12]	# Code table (ptr)
define	GIF_STACKP		Memi[$1+13]	# Stack (ptr)
define	GIF_CURBIT		Memi[$1+14]	# Decoder var
define	GIF_LASTBIT		Memi[$1+15]	# Decoder var
define	GIF_DONE		Memi[$1+16]	# Decoder var
define	GIF_LASTBYTE		Memi[$1+17]	# Decoder var
define	GIF_ZERO_DATABLOCK	Memi[$1+18]	# Decoder var
define	GIF_SP			Memi[$1+19]	# stack pointer

define	GIF_CLEAR_CODE		Memi[$1+20]	# LZW clear code
define	GIF_END_CODE		Memi[$1+21]	# LZW end code
define	GIF_FIRST_CODE		Memi[$1+22]	# LZW decoder var
define	GIF_OLD_CODE		Memi[$1+23]	# LZW decoder var
define	GIF_MAX_CODE		Memi[$1+24]	# LZW free code
define	GIF_MAX_CODE_SIZE	Memi[$1+25]	# LZW upper limit
define	GIF_CODE_SIZE		Memi[$1+26]	# LZW current code size
define	GIF_SET_CODE_SIZE	Memi[$1+27]	# LZW input code size
define	GIF_FRESH		Memi[$1+28]	# LZW init var

# The following are used for GIF89a only.
define	GIF_TRANSPARENT		Memi[$1+30]	# Transparent Color Index 
define	GIF_DELAYTIME		Memi[$1+31]	# Delay time
define	GIF_INPUTFLAG		Memi[$1+32]	# User input flag
define	GIF_DISPOSAL		Memi[$1+33]	# Disposal Method

# Array macros.
define	CODEBUF			Memc[GIF_CODEP($1)+$2]
define	EXTBUF			Memc[GIF_EXTBP($1)+$2]
define	CODETAB			Memc[GIF_CTABP($1)+($2*MAX_CODE_ENTRIES)+$3]
define	STACK			Memc[GIF_STACKP($1)+$2]

#---------------------------------------------------------------------------

define	INTERLACE	040X		# Image descriptor flags
define	LOCAL_COLORMAP	080X

# Define the flags for the GIF89a extension blocks.
define	GE_PLAINTEXT	001X		# Plain Text Extension
define	GE_APPLICATION	0FFX		# Application Extension
define	GE_COMMENT	0FEX		# Comment Extension
define	GE_GCONTROL	0F9X		# Graphics Control Extension

define	DEBUG		false
define	VDEBUG		false


# IP_GIF - Read and process a GIF format file into an IRAF image.

procedure ip_gif (ip, fname, info_only, verbose)

pointer	ip					#i import struct pointer
char	fname[ARB]				#i file name
int	info_only				#i print out image info only?
int	verbose					#i verbosity flag

pointer	gif
int	fd
int	bitpix, use_global_cmap, interlace
int	width, height, version
char	ch
short	sig[7], screen[12]

pointer	gif_open()
int	btoi(), strncmp(), gif_rdbyte(), gif_getbytes()
int	shifti()

long    filepos
common  /gifcom/ filepos

begin
	# Allocate the gif struct pointer.
	gif = gif_open()
	GIF_FD(gif) = IP_FD(ip)
	fd = GIF_FD(gif)

	# The GIF signature is verified in the database file but check it
	# here anyway.
	filepos = 1
	call ip_lseek (fd, BOF)
	if (gif_getbytes(fd, sig, 6) != OK)
	    call error (0, "Error reading GIF magic number.")
	if (strncmp(sig[4],"87a",3) == 0)
	    version = 87
	else if (strncmp(sig[4],"89a",3) == 0)
	    version = 89
	else
	    call error (0, "Bad version: File is not a GIF 87a or 89A")

	# Now read the screen descriptor.
	if (gif_getbytes(fd, screen, 7) != OK)
	    call error (0, "Error reading screen descriptor.")

	GIF_WIDTH(gif)      = screen[1] + (256 * screen[2])
	GIF_HEIGHT(gif)     = screen[3] + (256 * screen[4])
	GIF_BITPIX(gif)     = shifti (2, and(int(screen[5]),07X))
	GIF_COLRES(gif)     = shifti (and(int(screen[5]), 070X), -3) + 1
	GIF_BACKGROUND(gif) = screen[6]
	GIF_ASPECT(gif)     = screen[7]
	if (DEBUG) {
	    call eprintf ("w:%d h:%d bpix:%d ncol:%d bkg:%d asp:%d\n")
		call pargi(GIF_WIDTH(gif)); call pargi(GIF_HEIGHT(gif))
		call pargi(GIF_BITPIX(gif)); call pargi(GIF_COLRES(gif))
		call pargi(GIF_BACKGROUND(gif)); call pargi(GIF_ASPECT(gif))
		call flush (STDERR)
	}

	# We'll set the buffer size to the full image to speed processing.
	IP_SZBUF(ip) = GIF_HEIGHT(gif)

	# See if we have a global colormap.
	if (and (int(screen[5]), LOCAL_COLORMAP) > 0)
	    call gif_rdcmap (gif, GIF_BITPIX(gif), GIF_CMAP(gif))
	IP_CMAP(ip) = GIF_CMAP(gif)

	# Now process the rest of the image blocks.
	GIF_IMNUM(gif) = 0
	repeat {
	    if (gif_rdbyte(fd, ch) != OK) {
		call error (0, "Bad data read.")
	    }

	    if (ch == ';') {			# GIF terminator
		break
	    }

	    if (ch == '!') {			# Extension block
		# Read the extension function code.
	        if (gif_rdbyte(fd, ch) != OK)
		    call error (0, "Bad data read.")
		call gif_extension (gif, ch, IP_VERBOSE(ip))
		next
	    }

	    if (ch != ',') {			# not a valid start character
	        if (ch != '\0') {		# quietly allow a NULL block
		    call eprintf ("Ignoring bogus start char 0x%02x.")
		        call pargc (ch)
		}
		next
	    }

	    # Read the current image descriptor block.  There may be more
	    # than one image in a file so we'll just copy each image into 
	    # a separate band of the output image (should be rare).
            GIF_IMNUM(gif) = GIF_IMNUM(gif) + 1
	    if (gif_getbytes (fd, screen, 9) != OK)
		call error (0, "Bad scene descriptor")

	    # See if this image has a local colormap.  There supposedly aren't
	    # a lot of files that use this (GIF89a only) but we'll read it
	    # anyway so we don't get stung on file positioning.
	    if (and (int(screen[9]), LOCAL_COLORMAP) == LOCAL_COLORMAP)
                use_global_cmap = NO
	    else
                use_global_cmap = YES

	    # Unpack the image descriptor into useful things.
	    bitpix = shifti (1, (and (int(screen[9]), 07X) + 1))
	    interlace = btoi (and (int(screen[9]), INTERLACE) == INTERLACE)
	    width  = screen[5] + (screen[6] * 256) 
	    height = screen[7] + (screen[8] * 256)
	    if (DEBUG) {
		call eprintf ("global_cmap:%d bitpix:%d  ")
		    call pargi(use_global_cmap); call pargi(bitpix)
		call eprintf ("interlace:%d w:%d h:%d\n")
		    call pargi(interlace); call pargi(width); call pargi(height)
	    }

	    if (info_only == NO) {
                if (use_global_cmap == NO) {
		    # Process the image with a local colormap.
	    	    call gif_rdcmap (gif, bitpix, GIF_CMAP(gif))
                    call gif_read_image (ip, gif, width, height,
		        GIF_CMAP(gif), interlace)
                } else {
		    # Process the image with the global colormap.
                    call gif_read_image (ip, gif, width, height,
		        GIF_CMAP(gif), interlace)
                }
	    } else {
		call ip_gif_info (ip, fname, version, width, height, 
		    GIF_BITPIX(gif), use_global_cmap, interlace, verbose)
		break
	    }
	}

	# Clean up.
	call gif_close (gif)
	IP_CMAP(ip) = NULL
end


# IP_GIF_INFO - Print information about the GIF file.

procedure ip_gif_info (ip, fname, version, width, height, colres, global, 
    interlace, verbose)

pointer	ip					#i task struct pointer
char	fname[ARB]				#i file name
int	version					#i GIF version
int	width, height				#i image dimensions
int	colres					#i number of colormap entries
int	global					#i image has global colormap
int	interlace				#i image is interlaced
int	verbose					#i verbosity flag

begin
	# If not verbose print a one-liner.
	if (verbose == NO) {
#            call printf ("Input file:\n\t")
            call printf ("%s: %20t%d x %d   \t\tCompuServe GIF %da format file\n")
                call pargstr (fname)
                call pargi (width)
                call pargi (height)
            	call pargi (version)

            # Print out the format comment if any.
#            if (IP_COMPTR(ip) != NULL) {
#                if (COMMENT(ip) != '\0') {
#                    call printf ("%s\n")
#                        call pargstr (COMMENT(ip))
#                }
#                call strcpy ("\0", COMMENT(ip), SZ_LINE)
#            }
	    return
	}

        # Print a more verbose description.
        call printf ("%s: %20tCompuServe GIF %da Format File\n")
            call pargstr (fname)
            call pargi (version)

        # Print out the format comment if any.
        if (IP_COMPTR(ip) != NULL) {
            if (COMMENT(ip) != '\0') {
                call printf ("%s\n")
                    call pargstr (COMMENT(ip))
            }
            call strcpy ("\0", COMMENT(ip), SZ_LINE)
        }

        call printf ("%20tResolution:%38t%d x %d\n")
	    call pargi (width)
	    call pargi (height)

        call printf ("%20tPixel storage: %38t%s\n")
	    if (interlace == YES)
            	call pargstr ("Interlaced order")
	    else
            	call pargstr ("Sequential order")

        call printf ("%20tByte Order: %38t%s\n")
	    call pargstr ("LSB first")

        call printf ("%20tType: %38t%s\n")
	    call pargstr ("8-bit Color indexed")

        call printf ("%20t%s Colormap: %38t%d entries\n")
	    if (global == YES)
            	call pargstr ("Global")
	    else
            	call pargstr ("Local")
	    call pargi (colres)

        call printf ("%20tCompression: %38t%s\n")
	    call pargstr ("Lempel-Ziv and Welch (LZW)")
end


# GIF_OPEN - Open the GIF structure descriptor.

pointer procedure gif_open ()

pointer	gif

begin
	iferr (call calloc (gif, SZ_GIFSTRUCT, TY_STRUCT))
	    call error (0, "Error allocating GIF structure.")

	# Allocate the extension and code buffers.
	iferr (call calloc (GIF_CODEP(gif), SZ_GIFCODE, TY_CHAR))
	    call error (0, "Error allocating GIF code buffer pointer.")
	iferr (call calloc (GIF_EXTBP(gif), SZ_GIFEXTN, TY_CHAR))
	    call error (0, "Error allocating GIF extension pointer.")
	iferr (call calloc (GIF_CTABP(gif), SZ_GIFCTAB, TY_CHAR))
	    call error (0, "Error allocating code table pointer.")
	iferr (call calloc (GIF_STACKP(gif), SZ_GIFSTACK, TY_CHAR))
	    call error (0, "Error allocating GIF stack pointer.")

	# Initialize some of the variables to non-zero values.
	GIF_ZERO_DATABLOCK(gif) = NO
	GIF_TRANSPARENT(gif) = -1
	GIF_DELAYTIME(gif) = -1
	GIF_INPUTFLAG(gif) = -1

	return (gif)
end


# GIF_CLOSE - Close the GIF structure descriptor.

procedure gif_close (gif)

pointer	gif					#i GIF struct pointer

begin
	call mfree (GIF_STACKP(gif), TY_CHAR)
	call mfree (GIF_CTABP(gif),  TY_CHAR)
	call mfree (GIF_EXTBP(gif),  TY_CHAR)
	call mfree (GIF_CODEP(gif),  TY_CHAR)

	if (GIF_CMAP(gif) != NULL)
	    call mfree (GIF_CMAP(gif), TY_CHAR)
	call mfree (gif, TY_STRUCT)
end


# GIF_READ_IMAGE - Read the image raster from the file.  Decompress the
# LZW compressed data stream into 8-bit pixels.

procedure gif_read_image (ip, gif, width, height, cmap, interlace)

pointer	ip					#i task struct pointer
pointer	gif					#i GIF struct pointer
int	width, height				#i image dimensions
pointer	cmap					#i colormap pointer
int	interlace				#i interlace flag

pointer	im, op, out, data
char	csize, pix, val
int	i, v, xpos, ypos, pass
int	nlines, line, percent

pointer	ip_evaluate()
int	gif_rdbyte(), gif_lzw_rdbyte()
short	ip_gcmap_val()

begin
	# Get the initial code_size for the compression routines.
	if (gif_rdbyte(GIF_FD(gif), csize) != OK)
	    call error (0, "EOF or read error on image data.")
	call gif_lzw_init (gif, csize)

        # Patch up the pixtype param if needed.
        call ip_fix_pixtype (ip)

        # See if we need to create any outbands operands if the user didn't.
        if (IP_NBANDS(ip) == ERR)
            call ip_fix_outbands (ip)

	im = IP_IM(ip)
	op = PTYPE(ip,GIF_IMNUM(gif))
	call malloc (data, width, TY_CHAR)
	IO_DATA(op) = data
	IO_NPIX(op) = width

	# Get the pixels.
	xpos = 0
	ypos = 0
	pass = 0
	nlines = 0
	percent = 0
	repeat {
	    v = gif_lzw_rdbyte (gif)
	    if (v < 0)
		break				# at the EOF
	    else {
		if (cmap != NULL && IP_USE_CMAP(ip) == YES) {
		    # Apply the colormap since this is just an index.
		    val = v + 1
		    pix = ip_gcmap_val (val, cmap)
		} else
		    pix = char (v)
		Memc[data+xpos] = pix  		# assign the pixel
	    }

	    xpos = xpos + 1
	    if (xpos == width) {
		xpos = 0
		nlines = nlines + 1

                # Evaluate outbands expression.
		do i = 1, IP_NBANDS(ip) {
                    out = ip_evaluate (ip, O_EXPR(ip,i))

                    # Write bands to output image
                    if (IP_OUTPUT(ip) != IP_NONE) {
		        line = ypos + 1
                        call ip_wrline (ip, im, out, GIF_WIDTH(gif), line, 
			    (GIF_IMNUM(gif)-1)*IP_NBANDS(ip)+i)
		    }
                    call evvfree (out)
		}

                # Print percent done if being verbose
                if (IP_VERBOSE(ip) == YES) {
                    if (nlines * 100 / height >= percent + 10) {
                        percent = percent + 10
                        call printf ("    Status: %2d%% complete\r")
                            call pargi (percent)
                        call flush (STDOUT)
                    }
                }

		# if the image is interlaced adjust the line number accordingly,
		# otherwise just increment it.
		if (interlace == YES) {
		    switch (pass) {
		    case 0, 1:
			ypos = ypos + 8
		    case 2:
			ypos = ypos + 4
		    case 3:
			ypos = ypos + 2
		    }

		    if (ypos >= height) {
	    	        pass = pass + 1
		        switch (pass) {
		        case 1:
			    ypos = 4
		        case 2:
			    ypos = 2
		        case 3:
			    ypos = 1
		        }
		    }
		} else {
		    # Non-interlaced GIF so just increment the line number.
	    	    ypos = ypos + 1
		}
	    }
	}

        if (IP_VERBOSE(ip) == YES) {
            call printf ("    Status: Done          \n")
            call flush (STDOUT)
	}

	# Clean up the data pointer.
	call mfree (data, TY_CHAR)
end


# GIF_RDCMAP - Read a colormap (local or global) from the GIF file.

procedure gif_rdcmap (gif, ncolors, cmap)

pointer	gif					#i GIF struct pointer
int	ncolors					#i number of colors to read
pointer	cmap					#u local or global colormap ptr

int	i
char	rgb[3]
int	gif_getbytes()

begin
	if (cmap == NULL)
	    iferr (call calloc (cmap, 3*CMAP_SIZE, TY_CHAR))
		call error (0, "Error allocating color map.")

	do i = 1, ncolors {
	    # Read RGB colors.
	    if (gif_getbytes (GIF_FD(gif), rgb, 3) != OK)
		call error (0, "Bad GIF colormap - not enough colors.")

	    # Load the colormap.
	    CMAP(cmap,IP_RED,i) = rgb[1]
	    CMAP(cmap,IP_GREEN,i) = rgb[2]
	    CMAP(cmap,IP_BLUE,i) = rgb[3]
	}
end


# GIF_EXTENSION - Process a GIF extension block.  For now we'll just ignore
# these when converting the image but read the data blocks anyway.  We should
# still be able to read the image but won't take advantage of the GIF89a 
# extensions.

procedure gif_extension (gif, label, verbose)

pointer	gif					#i Gif struct pointer
char	label					#i GIF extension label
int	verbose					#i print verbose info?

pointer	sp, buf
int	val
int	and(), gif_get_data_block()

begin
	call smark (sp)
	call salloc (buf, SZ_GIFCODE, TY_CHAR)

	switch (label) {
	case GE_PLAINTEXT:			# Plain Text Extension
            if (verbose == YES) {
	        call eprintf ("Warning: Ignoring a Plain Text Extension.\n")
	        call flush (STDERR)
	    }
	case GE_APPLICATION:			# Application Extension
            if (verbose == YES) {
	        call eprintf ("Warning: Ignoring an Application Extension.\n")
	        call flush (STDERR)
	    }
	case GE_COMMENT:			# Comment Extension
	    # Simply print out the comment.
	    while (gif_get_data_block (gif, Memc[buf]) != 0) {
                if (verbose == YES) {
		    call printf ("Comment: %s\n")
		        call pargstr (Memc[buf])
	        }
	    }
	    call sfree (sp)
	    return
	case GE_GCONTROL:			# Graphic Control Extension
	    # Process the graphic control block.
	    val = gif_get_data_block (gif, Memc[buf])
	    GIF_DISPOSAL(gif) = and (int(Memc[buf]/4), 07X)
	    GIF_INPUTFLAG(gif) = and (int(Memc[buf]/2), 01X)
	    GIF_DELAYTIME(gif) = Memc[buf+1] + (256 * Memc[buf+2])
	    if (and(int(Memc[buf]),01X) == 1)
	        GIF_TRANSPARENT(gif) = Memc[buf+3]

	    while (gif_get_data_block (gif, Memc[buf]) != 0)
	        ;

	    call sfree (sp)
	    return
	default:
	    call eprintf ("Warning: Unknown extension label (0x%02x).\n")
		call pargc (label)
	    call flush (STDERR)
	}

	# If we get here then we've ignored an extension but still need to
	# eat the data blocks.
	while (gif_get_data_block (gif, Memc[buf]) != 0)
	    ;

	call sfree (sp)
end


# GIF_LZW_INIT - Initialize the LZW decompression variables.

procedure gif_lzw_init (gif, input_code_size)

pointer	gif					#i GIF struct pointer
char	input_code_size				#i input code size

int     i, shifti()

begin
        GIF_SET_CODE_SIZE(gif) = input_code_size
        GIF_CODE_SIZE(gif)     = GIF_SET_CODE_SIZE(gif) + 1
        GIF_CLEAR_CODE(gif)    = shifti (1, GIF_SET_CODE_SIZE(gif))
        GIF_END_CODE(gif)      = GIF_CLEAR_CODE(gif) + 1
        GIF_MAX_CODE_SIZE(gif) = 2 * GIF_CLEAR_CODE(gif)
        GIF_MAX_CODE(gif)      = GIF_CLEAR_CODE(gif) + 2

	GIF_CURBIT(gif) = 0		# initialize the code vars
	GIF_LASTBIT(gif) = 0
	GIF_DONE(gif) = NO
                
        GIF_FRESH(gif) = YES

	# Initialize the code table.
        for (i = 0; i < GIF_CLEAR_CODE(gif); i=i+1) {
            CODETAB(gif,0,i) = 0
            CODETAB(gif,1,i) = i
        }
        for (; i < MAX_CODE_ENTRIES; i=i+1) {
            CODETAB(gif,0,i) = 0
	    CODETAB(gif,1,0) = 0
	}

	GIF_SP(gif) = 0
end



# GIF_LZW_RDBYTE -

int procedure gif_lzw_rdbyte (gif)

pointer	gif					#i GIF struct pointer

pointer	sp, buf
int     i, count
int	code, incode

int	gif_get_code(), gif_get_data_block()

begin
	if (GIF_FRESH(gif) == YES) {
            GIF_FRESH(gif) = NO
            repeat {
                GIF_OLD_CODE(gif) = gif_get_code (gif, GIF_CODE_SIZE(gif))
                GIF_FIRST_CODE(gif) = GIF_OLD_CODE(gif)
            } until (GIF_FIRST_CODE(gif) != GIF_CLEAR_CODE(gif))
            return (GIF_FIRST_CODE(gif))
        }

	if (GIF_SP(gif) > 0) {
	    GIF_SP(gif) = GIF_SP(gif) - 1
	    return (STACK(gif,GIF_SP(gif)))
	}

        code = gif_get_code (gif, GIF_CODE_SIZE(gif))
        while (code >= 0) {

	    # The Clear Code sets everything back to its initial value, then
	    # reads the immediately subsequent code as uncompressed data.
            if (code == GIF_CLEAR_CODE(gif)) {
                for (i = 0; i < GIF_CLEAR_CODE(gif); i=i+1) {
                    CODETAB(gif,0,i) = 0
                    CODETAB(gif,1,i) = i
                }
                for ( ; i < MAX_CODE_ENTRIES; i=i+1) {
                    CODETAB(gif,0,i) = 0
		    CODETAB(gif,1,i) = 0
		}
                GIF_CODE_SIZE(gif) = GIF_SET_CODE_SIZE(gif) + 1
                GIF_MAX_CODE_SIZE(gif) = 2 * GIF_CLEAR_CODE(gif)
                GIF_MAX_CODE(gif) = GIF_CLEAR_CODE(gif) + 2
		GIF_SP(gif) = 0
		GIF_OLD_CODE(gif) = gif_get_code (gif, GIF_CODE_SIZE(gif))
                GIF_FIRST_CODE(gif) = GIF_OLD_CODE(gif)
                return (GIF_FIRST_CODE(gif))

	    # If this is the End Code we'll clean up a little before returning.
            } else if (code == GIF_END_CODE(gif)) {
                if (GIF_ZERO_DATABLOCK(gif) == YES)
                    return (ERR)

		call smark (sp)
		call salloc (buf, 260, TY_CHAR)

		repeat {
                    count = gif_get_data_block (gif, Memc[buf])
                } until (count <= 0)

                if (count != 0) {
                    call eprintf (
			"Missing EOD in data stream (common occurance)")
		}
		call sfree (sp)
                return (ERR)
            }

	    # Must be data so save it in incode.
            incode = code

	    # If it's greater or equal than the Free Code it's not in the hash
	    # table yet, repeat the last character decoded.
            if (code >= GIF_MAX_CODE(gif)) {
		STACK(gif, GIF_SP(gif)) = GIF_FIRST_CODE(gif)
		GIF_SP(gif) = GIF_SP(gif) + 1
                code = GIF_OLD_CODE(gif)
            }

            while (code >= GIF_CLEAR_CODE(gif)) {
		STACK(gif, GIF_SP(gif)) = CODETAB(gif,1,code)
		GIF_SP(gif) = GIF_SP(gif) + 1
                if (code == CODETAB(gif,0,code))
                    call error (0, "Circular GIF code table entry.")
                code = CODETAB(gif,0,code)
            }

            GIF_FIRST_CODE(gif) = CODETAB(gif,1,code)
	    STACK(gif, GIF_SP(gif)) = GIF_FIRST_CODE(gif)
	    GIF_SP(gif) = GIF_SP(gif) + 1

	    if (VDEBUG) {
		call eprintf("code=%d gmax=%d gmaxsz=%d 4096 old:%d frst:%d\n")
		    call pargi(code) ; call pargi(GIF_MAX_CODE(gif))
		    call pargi(GIF_MAX_CODE_SIZE(gif))
		    call pargi(GIF_OLD_CODE(gif))
		    call pargi(GIF_FIRST_CODE(gif))
	    }

	    # Point to the next slot in the table.  If we exceed the current
	    # MaxCode value, increment the code size unless it's already 12. 
	    # If it is, do nothing: the next code decompressed better be CLEAR

            code = GIF_MAX_CODE(gif)
            if (code < MAX_CODE_ENTRIES) {
                CODETAB(gif,0,code) = GIF_OLD_CODE(gif)
                CODETAB(gif,1,code) = GIF_FIRST_CODE(gif)
                GIF_MAX_CODE(gif) = GIF_MAX_CODE(gif) + 1
                if ((GIF_MAX_CODE(gif) >= GIF_MAX_CODE_SIZE(gif)) &&
                    (GIF_MAX_CODE_SIZE(gif) < MAX_CODE_ENTRIES)) {
                        GIF_MAX_CODE_SIZE(gif) = GIF_MAX_CODE_SIZE(gif) * 2
                        GIF_CODE_SIZE(gif) = GIF_CODE_SIZE(gif) + 1
                }
            }

            GIF_OLD_CODE(gif) = incode

	    if (GIF_SP(gif) > 0) {
		GIF_SP(gif) = GIF_SP(gif) - 1
	        return (STACK(gif,GIF_SP(gif)))
	    }
        
	    code = gif_get_code (gif, GIF_CODE_SIZE(gif))
        }
        return code
end


# GIF_GET_CODE - Fetch the next code from the raster data stream.  The codes 
# can be any length from 3 to 12 bits, packed into 8-bit bytes, so we have to
# maintain our location in the Raster array as a BIT Offset.  We compute the 
# byte Offset into the raster array by dividing this by 8, pick up three 
# bytes, compute the bit Offset into our 24-bit chunk, shift to bring the 
# desired code to the bottom, then mask it off and return it.  Simple.

int procedure gif_get_code (gif, code_size)

pointer	gif					#i GIF struct pointer
int	code_size				#i op code size

int	i, j, count, ret
int	val1, val2
int	btoi(), and(), shifti(), ori ()
int	gif_get_data_block()

begin
	# See if processing the next code will overflow our buffer.  If so
	# we get the next control block from the stream.
	if ( (GIF_CURBIT(gif) + code_size) >= GIF_LASTBIT(gif)) {
	    if (GIF_DONE(gif) == YES) {
		if (GIF_CURBIT(gif) >= GIF_LASTBIT(gif)) {
		    call error (0, "GIF_GET_CODE: Ran out of bits.\n")
		    return (ERR)
		}
	    }

	    CODEBUF(gif,0) = CODEBUF(gif,GIF_LASTBYTE(gif)-2)
            CODEBUF(gif,1) = CODEBUF(gif,GIF_LASTBYTE(gif)-1)

            count = gif_get_data_block (gif, CODEBUF(gif,2))
            if (count == 0)
                GIF_DONE(gif) = YES

            GIF_LASTBYTE(gif) = 2 + count
            GIF_CURBIT(gif) = (GIF_CURBIT(gif) - GIF_LASTBIT(gif)) + 16
            GIF_LASTBIT(gif) = (2 + count) * 8
        }

        #  for (i = GIF_CURBIT(gif), j = 0; j < code_size; ++i, ++j)
        #      ret |= ((buf[ i / 8 ] & (1 << (i % 8))) != 0) << j;

	i = GIF_CURBIT(gif)
	j = 0
        ret = 0
	while (j < code_size) {
	    val1 = btoi ( and (int(CODEBUF(gif,i/8)), shifti(1,mod(i,8))) != 0 )
            val2 = shifti (val1, j)
            ret = ori (ret, val2)
	    i = i + 1
	    j = j + 1
	}

        GIF_CURBIT(gif) = GIF_CURBIT(gif) + code_size
	if (VDEBUG) {
	    call eprintf (": returning %d\n");call pargi(ret);call flush(STDERR)
	}

        return (ret)
end


# GIF_GET_DATA_BLOCK - Get the next block of GIF data from the data stream so
# it can be converted to raster data.

int procedure gif_get_data_block (gif, buf)

pointer	gif					#i GIF struct pointer
char	buf[ARB]				#o data block

char	count
int	nb, btoi()
int	gif_rdbyte(), gif_getbytes()

begin
        if (gif_rdbyte (GIF_FD(gif), count) != OK) {
            call error (0, "error in getting DataBlock size")
            return (ERR)
        }

        GIF_ZERO_DATABLOCK(gif) = btoi (count == 0)
	if (VDEBUG) {
	    call eprintf ("getDataBlock: count = %d "); call pargs(count) }
	nb = count
        if ((count != 0) && (gif_getbytes(GIF_FD(gif), buf, nb) != OK)) {
            call error (0, "error in reading DataBlock")
            return (ERR)
        }
        return count
end



# Byte I/O routines.  We use the normal IMPORT procedures but localize the code
# here to make it easier to keep track of the current file position (in bytes).

# GIF_RDBYTE - Read a single byte at the current offset from the file.

int procedure gif_rdbyte (fd, val)

int	fd					#i file descriptor
char	val					#o byte read

short	ip_getb()

long    filepos
common  /gifcom/ filepos

begin
	iferr (val = ip_getb (fd, filepos))
	    return (ERR)

	filepos = filepos + 1
	call ip_lseek (fd, filepos)

	return (OK)
end


# GIF_GETBYTES - Read an array of bytes from the file at the current offset.

int procedure gif_getbytes (fd, buffer, len)

int	fd					#i file descriptor
char	buffer[ARB]				#o output buffer
int	len					#i no. of bytes to read

pointer	sp, bp

long    filepos
common  /gifcom/ filepos

begin
	call smark (sp)
	call salloc (bp, len+1, TY_CHAR)
	call aclrc (Memc[bp], len+1)

	call ip_agetb (fd, bp, len)		# read the bytes
	call amovc (Memc[bp], buffer, len)	# copy to output buffer
	filepos = filepos + len
	call ip_lseek (fd, filepos)

	call sfree (sp)
	return (OK)
end
