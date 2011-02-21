include <mach.h>
include "../import.h"


# IPRAS.X - Source file for the IMPORT task rasterfile builtin format.


define	SZ_RASHDR	13
define	RAS_MAGIC	Memi[$1]	# Magic number
define	RAS_WIDTH	Memi[$1+1]	# Image width (pixels per line)
define	RAS_HEIGHT	Memi[$1+2]	# Image height (number of lines)
define	RAS_DEPTH	Memi[$1+3]	# Image depth (bits per pixel)
define	RAS_LENGTH	Memi[$1+4]	# Image length (bytes)
define	RAS_TYPE	Memi[$1+5]	# File type
define	RAS_MAPTYPE	Memi[$1+6]	# Colormap type
define	RAS_MAPLENGTH	Memi[$1+7]	# Colormap length (bytes)

define	RAS_CMAP	Memi[$1+10]	# Colormap (ptr)
define	RAS_COUNT	Memi[$1+11]	# RLE decoding var
define	RAS_CH		Memi[$1+12]	# RLE decoding var

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



# IP_RAS - Read and process a Sun Rasterfile into an IRAF image.

procedure ip_ras (ip, fname, info_only, verbose)

pointer	ip					#i import struct pointer
char	fname[ARB]				#i file name
int	info_only				#i print out image info only?
int	verbose					#i verbosity flag

pointer	ras
int	fd, w, nchars
pointer	ras_open()

long    filepos
common  /rascom/ filepos

begin
        # Allocate the ras struct pointer.
        ras = ras_open ()
        fd = IP_FD(ip)

        # Initialize the file position.
        filepos = 1
        call ip_lseek (fd, BOF)

	# Read in the rasterfile header, dump it directly to the task struct.
	call ip_ageti (fd, ras, 8)
	filepos = filepos + SZ_INT32 * SZB_CHAR * 8
	call ip_lseek (fd, filepos)

	# Now do some sanity checking on the values.
	if (RAS_MAGIC(ras) != RAS_MAGIC_NUM)
	    call error (0, "Not a Sun rasterfile.")
        if (RAS_TYPE(ras) == RT_OLD && RAS_LENGTH(ras) == 0)
            RAS_LENGTH(ras) = RAS_WIDTH(ras) * RAS_HEIGHT(ras) *
		RAS_DEPTH(ras) / 8

	# See if we really want to convert this thing.
	if (info_only == YES) {
	    call ip_ras_info (ip, ras, fname, verbose)
	    call ras_close (ras)
	    return
	}

	# Get the colormap (if any).
	call ras_rdcmap (fd, ras, RAS_CMAP(ras))
	IP_CMAP(ip) = RAS_CMAP(ras)

        # Round up to account for 16 bit line blocking.
	w = RAS_WIDTH(ras) * (RAS_DEPTH(ras) / 8)
        nchars = w + mod (w, SZB_CHAR)


        # Patch up the pixtype param if needed.
        call ip_fix_pixtype (ip)

        # See if we need to create any outbands operands if the user didn't.
        if (IP_NBANDS(ip) == ERR)
            call ip_fix_outbands (ip)

	# Now process the image.
	switch (RAS_DEPTH(ras)) {
	case 1:
	    call eprintf ("Bitmap rasterfiles aren not supported.")
	    call flush (STDERR)

	case 8:
	    # Standard or byte encoded 8-bit rasterfile.
	    if (RAS_TYPE(ras) == RT_OLD || RAS_TYPE(ras) == RT_STANDARD) {
		call ip_prband (ip, fd, IP_IM(ip), RAS_CMAP(ras))

            } else if (RAS_TYPE(ras) == RT_BYTE_ENCODED) {
	        call ras_rle8 (ip, ras, fd, nchars)

            } else {
	        call eprintf ("Unsupported 8-bit RAS_TYPE: %d\n")
		    call pargi (RAS_TYPE(ras))
	        call flush (STDERR)
	    }

	case 24, 32:
	    # 24 or 32-bit rasterfiles have no colormap (at least they 
	    # shouldn't) and are pixel-interleaved.  We already know how to 
	    # do this so just call the right routines for processing.

	    if (RAS_TYPE(ras) == RT_BYTE_ENCODED) {
	        call ip_fix_pixtype (ip)
	        call ras_rle24 (ip, ras, fd, nchars)
	    } else {
	        call ip_fix_pixtype (ip)
	        call ip_prpix (ip, fd, IP_IM(ip), NULL)
	    }

	default:
	    call eprintf ("Invalid pixel size.")
	    call flush (STDERR)
	}

	# Clean up.
	call ras_close (ras)
	IP_CMAP(ip) = NULL
end


# IP_RAS_INFO - Print information about the raster file.

procedure ip_ras_info (ip, ras, fname, verbose)

pointer	ip					#i ip struct pointer
pointer	ras					#i ras struct pointer
char	fname[ARB]				#i file name
int	verbose					#i verbosity flag

begin
	# If not verbose print a one-liner.
	if (verbose == NO) {
#            call printf ("Input file:\n\t")
            call printf ("%s: %20t%d x %d   \t\t%d-bit Sun Rasterfile\n")
                call pargstr (fname)
                call pargi (RAS_WIDTH(ras))
                call pargi (RAS_HEIGHT(ras))
                call pargi (RAS_DEPTH(ras))

            # Print out the format comment if any.
#            if (IP_COMPTR(ip) != NULL) {
#                if (COMMENT(ip) != '\0') {
#                    call printf ("%s\n")
#                        call pargstr (COMMENT(ip))
#                }
#                call strcpy ("\0", COMMENT(ip), SZ_LINE)
#            }
#	    if (RAS_DEPTH(ras) > 8) {
#	        if (RAS_TYPE(ras) != RT_FORMAT_RGB && RAS_TYPE(ras) != RT_OLD) {
#		    call eprintf ("\tNote: %d-bit rasterfile is stored as %s\n")
#		        call pargi (RAS_DEPTH(ras))
#			call pargstr ("ABGR and not ARGB")
#	        }
#	    }
	    return
	}

        # Print a more verbose description.
        call printf ("%s: %20tSun Rasterfile\n")
            call pargstr (fname)

        # Print out the format comment if any.
        if (IP_COMPTR(ip) != NULL) {
            if (COMMENT(ip) != '\0') {
                call printf ("%s\n")
                    call pargstr (COMMENT(ip))
            }
            call strcpy ("\0", COMMENT(ip), SZ_LINE)
        }
	if (RAS_DEPTH(ras) > 8) {
	    if (RAS_TYPE(ras) != RT_FORMAT_RGB && RAS_TYPE(ras) != RT_OLD) {
		call eprintf ("\tNote: %d-bit rasterfile is stored as %s\n")
		    call pargi (RAS_DEPTH(ras))
		    call pargstr ("ABGR and not ARGB")
	    }
	}

        call printf ("%20tByte Order:%38t%s\n")
	    if (IP_SWAP(ip) == S_NONE && BYTE_SWAP2 == NO )
                call pargstr ("Most Significant Byte First")
	    else
                call pargstr ("Least Significant Byte First")

        call printf ("%20tResolution:%38t%d x %d\n")
            call pargi (RAS_WIDTH(ras))
            call pargi (RAS_HEIGHT(ras))

        call printf ("%20tType: %38t%d-bit %s %s\n")
	    call pargi (RAS_DEPTH(ras))
            switch (RAS_TYPE(ras)) {
            case RT_OLD:
                call pargstr ("Old")
            case RT_STANDARD:
                call pargstr ("Standard")
            case RT_BYTE_ENCODED:
                call pargstr ("Byte Encoded")
            case RT_FORMAT_RGB:
                call pargstr ("RGB")
            case RT_FORMAT_TIFF:
                call pargstr ("TIFF")
            case RT_FORMAT_IFF:
                call pargstr ("IFF")
            default:
                call pargstr ("Experimental (or unknown)")
            }
	    if (RAS_MAPLENGTH(ras) > 0)
                call pargstr ("Color Index")
	    else
                call pargstr ("")

	if (RAS_MAPLENGTH(ras) > 0) {
            call printf ("%20tColormap:%38t%d entries\n")
	        if (RAS_MAPTYPE(ras) == RMT_EQUAL_RGB)
                    call pargi (RAS_MAPLENGTH(ras)/3)
	        else
                    call pargi (RAS_MAPLENGTH(ras))
	} else
            call printf ("%20tColormap:%38tnone\n")

        call printf ("%20tCompression: %38t%s\n")
            if (RAS_TYPE(ras) == RT_BYTE_ENCODED)
	        call pargstr ("Run Length Encoded")
	    else 
		call pargstr ("None")

        call printf ("%20tAlpha Channel: %38t%s\n")
	    if (RAS_DEPTH(ras) == 32)
		call pargstr ("yes")
	    else
		call pargstr ("none")
end


# RAS_OPEN - Open the RAS structure descriptor.

pointer procedure ras_open ()

pointer	ras

begin
	iferr (call calloc (ras, SZ_RASHDR, TY_STRUCT))
	    call error (0, "Error allocating RAS structure.")
	RAS_CMAP(ras) = NULL

	return (ras)
end


# RAS_CLOSE - Close the RAS structure descriptor.

procedure ras_close (ras)

pointer	ras					#i RAS struct pointer

begin
	if (RAS_CMAP(ras) != NULL)
	    call mfree (RAS_CMAP(ras), TY_CHAR)
	call mfree (ras, TY_STRUCT)
end


# RAS_RDCMAP - Read the colormap from the image if necessary.

procedure ras_rdcmap (fd, ras, cmap)

int	fd					#i file descriptor
pointer	ras					#i RAS struct pointer
pointer	cmap					#i colormap array ptr

int	ncolors

long    filepos
common  /rascom/ filepos

begin
	# Now read the colormap, allocate the pointer if we need to.
	ncolors = RAS_MAPLENGTH(ras)
	if (RAS_MAPTYPE(ras) == RMT_EQUAL_RGB && ncolors > 0) {
	    if (cmap == NULL)
	        call calloc (cmap, ncolors*3, TY_CHAR)
	    call ip_agetb (fd, cmap, ncolors)

	} else if (RAS_MAPTYPE(ras) == RMT_RAW) {
	    call eprintf ("Warning: Can't handle RMT_RAW maptype - ignoring.\n")
	    call flush (STDERR)

	    # Skip over the bytes anyway.
	    filepos = filepos + ncolors
	    call ip_lseek (fd, filepos)
	    return
	}

	filepos = filepos + ncolors
	call ip_lseek (fd, filepos)
end


# RAS_RLE8 - Process an 8-bit rasterfile into an IRAF image.  This
# procedure handles both standard and RLE files.

procedure ras_rle8 (ip, ras, fd, nchars)

pointer	ip					#i ip struct pointer
pointer	ras					#i ras struct pointer
int	fd					#i input file descriptor
int	nchars					#i line size

pointer	im, data, op
int	i, percent

long    filepos
common  /rascom/ filepos

begin
        im = IP_IM(ip)
        op = PTYPE(ip,1)
        call malloc (data, nchars, TY_CHAR)
        IO_DATA(op) = data
        IO_NPIX(op) = RAS_WIDTH(ras)

        percent = 0
        do i = 1, RAS_HEIGHT(ras) {
	    call ras_read_rle (ras, fd, Memc[data], nchars)

            # Apply the colormap since this is just an index.
            if (RAS_MAPLENGTH(ras) != 0 && IP_USE_CMAP(ip) == YES)
                call ip_gray_cmap (Memc[data], RAS_WIDTH(ras),
                    RAS_CMAP(ras))

            # Evaluate and write the outbands expressions.
            call ip_probexpr (ip, im, RAS_WIDTH(ras), i)

            # Print percent done if being verbose
            if (IP_VERBOSE(ip) == YES) {
                if (i * 100 / RAS_HEIGHT(ras) >= percent + 10) {
                    percent = percent + 10
                    call printf ("    Status: %2d%% complete\r")
                        call pargi (percent)
                    call flush (STDOUT)
                }
            }

        }

        if (IP_VERBOSE(ip) == YES) {
            call printf ("    Status: Done          \n")
            call flush (STDOUT)
        }
end


# RAS_RLE24 - Process an 24-bit rasterfile into an IRAF image.  This
# procedure handles both standard and RLE files.

procedure ras_rle24 (ip, ras, fd, nchars)

pointer	ip					#i ip struct pointer
pointer	ras					#i ras struct pointer
int	fd					#i input file descriptor
int	nchars					#i line size

pointer	im, data, op
int	i, percent, npix

long    filepos
common  /rascom/ filepos

begin
        im = IP_IM(ip)
        op = PTYPE(ip,1)
        call malloc (data, nchars, TY_SHORT)
        IO_DATA(op) = data
        IO_NPIX(op) = RAS_WIDTH(ras)

        # See if we need to create any outbands operands if the user didn't.
        if (IP_NBANDS(ip) == ERR)
            call ip_fix_outbands (ip)

        # Allocate the pixtype data pointers.
        npix = RAS_WIDTH(ras)
        do i = 1, IP_NPIXT(ip) {
            op = PTYPE(ip,i)
            IO_NPIX(op) = npix
            call calloc (IO_DATA(op), npix, TY_SHORT)
        }

        percent = 0
        do i = 1, RAS_HEIGHT(ras) {
	    call ras_read_rle (ras, fd, Memc[data], nchars)

            # Separate pixels into different vectors.
            call ip_upkpix (ip, data, npix)

            # Evaluate and write the outbands expressions.
            call ip_probexpr (ip, im, npix, i)

            # Print percent done if being verbose
            if (IP_VERBOSE(ip) == YES) {
                if (i * 100 / RAS_HEIGHT(ras) >= percent + 10) {
                    percent = percent + 10
                    call printf ("    Status: %2d%% complete\r")
                        call pargi (percent)
                    call flush (STDOUT)
                }
            }

        }
        if (IP_VERBOSE(ip) == YES) {
            call printf ("    Status: Done          \n")
            call flush (STDOUT)
        }
end


# RAS_READ_RLE - Read a line of RLE encoded data from the file.

procedure ras_read_rle (ras, fd, data, nchars)

pointer	ras					#i ras struct pointer
int	fd					#i file descriptor
char	data[ARB]				#u output pixels
int	nchars					#i number of pixels to read

int	i
short	pix, ras_rdbyte()

long    filepos
common  /rascom/ filepos

begin
	i = 1
	while (i <= nchars) {
	    if (RAS_COUNT(ras) > 0) {
		data[i] = RAS_CH(ras)
		i = i + 1
		RAS_COUNT(ras) = RAS_COUNT(ras) - 1

	    } else {
		pix = ras_rdbyte (fd)
		if (pix == RAS_RLE) {
		    RAS_COUNT(ras) = ras_rdbyte (fd)
		    if (RAS_COUNT(ras) == 0) {
			data[i] = pix
			i = i + 1
		    } else {
		        RAS_CH(ras) = ras_rdbyte (fd)
			data[i] = RAS_CH(ras)
			i = i + 1
		    }
		} else {
		    data[i] = pix
		    i = i + 1
		}
	    }
	}
end


# RAS_RDBYTE - Read a single byte at the current offset from the file.

short procedure ras_rdbyte (fd)

int     fd                                      #i file descriptor

short   val
short   ip_getb()

long    filepos
common  /rascom/ filepos

begin
        iferr (val = ip_getb (fd, filepos))
            return (ERR)

        filepos = filepos + 1
        call ip_lseek (fd, filepos)

        return (val)
end
