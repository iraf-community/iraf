# IPXWD.X - Source file for the IMPORT task X Window Dump builtin format.

include <mach.h>
include "../import.h"


# IP_XWD - Read and process an X Window Dump into an IRAF image.

procedure ip_xwd (ip, fname, info_only, verbose)

pointer	ip					#i import struct pointer
char	fname[ARB]				#i file name
int	info_only				#i print out image info only?
int	verbose					#i verbosity flag

int	fd
pointer im, cmap
int     nchars
long	depth, cmap_entries, hdr_size
long	hskip, lpad, width,height

long	ip_getl()

begin
        # Get the input file descriptor and initialize the file position.
        fd = IP_FD(ip)
        im = IP_IM(ip)
        call ip_lseek (fd, BOF)

	# Get some information from the header we'll need for processing.
	hdr_size = ip_getl (fd, 1)
	width = IP_AXLEN(ip,1)
	height = IP_AXLEN(ip,2)
	depth = ip_getl (fd, 45)
	hskip = IP_HSKIP(ip)
	lpad = IP_LPAD(ip)
	cmap_entries = ip_getl (fd, 73)
	nchars = width + lpad

	# See if we really want to convert this thing.
	if (info_only == YES) {
	    call ip_xwd_info (ip, fname, depth, cmap_entries, verbose)
	    return
	}

	# Now process the image.  For 24-bit or 32-bit files we have an RGB
	# image and can process normally, if this is an 8-bit image see if
	# we have a colormap we need to use.

	if (depth > 8) {
	    call ip_prpix (ip, fd, im, NULL)
	} else {
	    cmap = NULL
	    if (cmap_entries > 0)
		call xwd_rdcmap (ip, fd, hdr_size, cmap_entries, cmap)
	    call ip_prband (ip, fd, im, cmap)
        }
	IP_CMAP(ip) = NULL
end


# IP_XWD_INFO - Print information about the xwd file.

procedure ip_xwd_info (ip, fname, depth, ncolors, verbose)

pointer	ip					#i ip struct pointer
char	fname[ARB]				#i file name
int	depth					#i bits per pixel
int	ncolors					#i number of colors
int	verbose					#i verbosity flag

begin
	# If not verbose print a one-liner.
	if (verbose == NO) {
            #call printf ("Input file:\n\t")
            call printf ("%s: %20t%d x %d   \t%d-bit X11 Window Dump\n")
                call pargstr (fname)
                call pargi (IP_AXLEN(ip,1))
                call pargi (IP_AXLEN(ip,2))
                call pargi (depth)

            # Print out the format comment if any.
            if (IP_COMPTR(ip) != NULL) {
                if (COMMENT(ip) != '\0') {
                    call printf ("%s\n")
                        call pargstr (COMMENT(ip))
                }
                call strcpy ("\0", COMMENT(ip), SZ_LINE)
            }
	    return
	}

        # Print a more verbose description.
        call printf ("%s: %20tX11 Window Dump\n")
            call pargstr (fname)

        # Print out the format comment if any.
        if (IP_COMPTR(ip) != NULL) {
            if (COMMENT(ip) != '\0') {
                call printf ("%s\n")
                    call pargstr (COMMENT(ip))
            }
            call strcpy ("\0", COMMENT(ip), SZ_LINE)
        }

        call printf ("%20tByte Order:%38t%s\n")
	    if (IP_SWAP(ip) == S_NONE && BYTE_SWAP2 == NO )
                call pargstr ("Most Significant Byte First")
	    else
                call pargstr ("Least Significant Byte First")

        call printf ("%20tResolution:%38t%d x %d\n")
            call pargi (IP_AXLEN(ip,1))
            call pargi (IP_AXLEN(ip,2))

        call printf ("%20tType: %38t%d-bit %s\n")
	    call pargi (depth)
    	    if (ncolors > 0)
                call pargstr ("Color Index")
	    else
                call pargstr ("")

        call printf ("%20tHeader size:%38t%d bytes\n")
            call pargi (IP_HSKIP(ip))

	if (ncolors > 0) {
           call printf ("%20tColormap:%38t%d entries\n")
                call pargi (ncolors)
	} else
            call printf ("%20tColormap:%38tnone\n")

        call printf ("%20tAlpha Channel: %38t%s\n")
	    if (depth == 32)
		call pargstr ("8-bit")
	    else
		call pargstr ("none")
end


# XWD_RDCMAP - Read colormap from an X11 Window Dump file and return a 
# pointer to it.

procedure xwd_rdcmap (ip, fd, hdr_size, ncolors, cmap)

pointer	ip					#i task struct pointer
int	fd					#i file descriptor
int	hdr_size				#i header size
int	ncolors					#i number of colormap entries
pointer	cmap					#i colormap pointer

int	i
long	filepos, pixel
int	r, g, b
char	flags, pad

short	ip_getb()
int	ip_getu()
long	ip_getl()

define	SZ_X11_CSTRUCT	12

begin
        # Now read the colormap, allocate the pointer if we need to.
	cmap = NULL
        if (ncolors == 0)
	    return
	else
            call calloc (cmap, CMAP_SIZE*3, TY_CHAR)

	filepos = hdr_size + 3
	call ip_lseek (fd, filepos)
	do i = 1, ncolors {
	    pixel = ip_getl (fd, filepos)
	    r = ip_getu (fd, filepos+4)
	    g = ip_getu (fd, filepos+6)
	    b = ip_getu (fd, filepos+8)
	    flags = ip_getb (fd, filepos+10)
	    pad = ip_getb (fd, filepos+11)

	    CMAP(cmap,IP_RED,i)   = r * 255 / 65535
	    CMAP(cmap,IP_GREEN,i) = g * 255 / 65535
	    CMAP(cmap,IP_BLUE,i)  = b * 255 / 65535

	    filepos = filepos + SZ_X11_CSTRUCT
	    call ip_lseek (fd, filepos)
	}
	IP_CMAP(ip) = cmap
end
