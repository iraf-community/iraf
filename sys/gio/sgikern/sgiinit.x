# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<gki.h>
include	"sgi.h"

# SGI_INIT -- Initialize the gkt data structures from the graphcap entry
# for the device.  Called once, at OPENWS time, with the TTY pointer already
# set in the common.  The companion routine SGI_RESET initializes the attribute
# packets when the frame is flushed.

procedure sgi_init (tty, devname)

pointer	tty			# graphcap descriptor
char	devname[ARB]		# device name

pointer	nextch
int	maxch, i
real	char_height, char_width, char_size

bool	ttygetb()
real	ttygetr()
int	ttygeti(), btoi(), gstrcpy()
include	"sgi.com"

begin
	# Allocate the gkt descriptor and the string buffer.
	if (g_kt == NULL) {
	    call calloc (g_kt, LEN_SGI, TY_STRUCT)
	    call malloc (SGI_SBUF(g_kt), SZ_SBUF, TY_CHAR)
	}

	# Get the maximum frame count and the flags controlling frame advance
	# at start and end of metafile.

	g_maxframes = ttygeti (tty, "MF")
	if (g_maxframes == 0)
	    g_maxframes = DEF_MAXFRAMES
	SGI_STARTFRAME(g_kt) = btoi (ttygetb (tty, "FS"))
	SGI_ENDFRAME(g_kt)   = btoi (ttygetb (tty, "FE"))

	# Init string buffer parameters.  The first char of the string buffer
	# is reserved as a null string, used for graphcap control strings
	# omitted from the graphcap entry for the device.

	SGI_SZSBUF(g_kt) = SZ_SBUF
	SGI_NEXTCH(g_kt) = SGI_SBUF(g_kt) + 1
	Memc[SGI_SBUF(g_kt)] = EOS

	# Get the device resolution from the graphcap entry.

	g_xres = ttygeti (tty, "xr")
	if (g_xres <= 0)
	    g_xres = 1024
	g_yres = ttygeti (tty, "yr")
	if (g_yres <= 0)
	    g_yres = 1024

	# Initialize the character scaling parameters, required for text
	# generation.  The heights are given in NDC units in the graphcap
	# file, which we convert to GKI units.  Estimated values are
	# supplied if the parameters are missing in the graphcap entry.

	char_height = ttygetr (tty, "ch")
	if (char_height < EPSILON)
	    char_height = 1.0 / 35.0
	char_height = char_height * GKI_MAXNDC

	char_width = ttygetr (tty, "cw")
	if (char_width < EPSILON)
	    char_width = 1.0 / 80.0
	char_width = char_width * GKI_MAXNDC

	# If the device has a set of discreet character sizes, get the
	# size of each by fetching the parameter "tN", where the N is
	# a digit specifying the text size index.  Compute the height and
	# width of each size character from the "ch" and "cw" parameters
	# and the relative scale of character size I.

	SGI_NCHARSIZES(g_kt) = min (MAX_CHARSIZES, ttygeti (tty, "th"))
	nextch = SGI_NEXTCH(g_kt)

	if (SGI_NCHARSIZES(g_kt) <= 0) {
	    SGI_NCHARSIZES(g_kt) = 1
	    SGI_CHARSIZE(g_kt,1) = 1.0
	    SGI_CHARHEIGHT(g_kt,1) = char_height
	    SGI_CHARWIDTH(g_kt,1)  = char_width
	} else {
	    Memc[nextch+2] = EOS
	    for (i=1;  i <= SGI_NCHARSIZES(g_kt);  i=i+1) {
		Memc[nextch] = 't'
		Memc[nextch+1] = TO_DIGIT(i)
		char_size = ttygetr (tty, Memc[nextch])
		SGI_CHARSIZE(g_kt,i)   = char_size
		SGI_CHARHEIGHT(g_kt,i) = char_height * char_size
		SGI_CHARWIDTH(g_kt,i)  = char_width  * char_size
	    }
	}

	# Initialize the output parameters.  All boolean parameters are stored
	# as integer flags.  All string valued parameters are stored in the
	# string buffer, saving a pointer to the string in the gkt
	# descriptor.  If the capability does not exist the pointer is set to
	# point to the null string at the beginning of the string buffer.

	SGI_POLYLINE(g_kt)   = btoi (ttygetb (tty, "pl"))
	SGI_POLYMARKER(g_kt) = btoi (ttygetb (tty, "pm"))
	SGI_FILLAREA(g_kt)   = btoi (ttygetb (tty, "fa"))
	SGI_FILLSTYLE(g_kt)  =       ttygeti (tty, "fs")
	SGI_ROAM(g_kt)	     = btoi (ttygetb (tty, "ro"))
	SGI_ZOOM(g_kt)	     = btoi (ttygetb (tty, "zo"))
	SGI_XRES(g_kt)	     =       ttygeti (tty, "xr")
	SGI_YRES(g_kt)	     =       ttygeti (tty, "yr")
	SGI_ZRES(g_kt)	     =       ttygeti (tty, "zr")
	SGI_CELLARRAY(g_kt)  = btoi (ttygetb (tty, "ca"))
	SGI_SELERASE(g_kt)   = btoi (ttygetb (tty, "se"))
	SGI_PIXREP(g_kt)     = btoi (ttygetb (tty, "pr"))

	# Initialize the input parameters.

	SGI_CURSOR(g_kt)     = 1

	# Save the device string in the descriptor.
	nextch = SGI_NEXTCH(g_kt)
	SGI_DEVNAME(g_kt) = nextch
	maxch = SGI_SBUF(g_kt) + SZ_SBUF - nextch + 1
	nextch = nextch + gstrcpy (devname, Memc[nextch], maxch) + 1
	SGI_NEXTCH(g_kt) = nextch
end


# SGI_GSTRING -- Get a string value parameter from the graphcap table,
# placing the string at the end of the string buffer.  If the device does
# not have the named capability return a pointer to the null string,
# otherwise return a pointer to the string.  Since pointers are used,
# rather than indices, the string buffer is fixed in size.  The additional
# degree of indirection required with an index was not considered worthwhile
# in this application since the graphcap entries are never very large.

pointer procedure sgi_gstring (cap)

char	cap[ARB]		# device capability to be fetched
pointer	strp, nextch
int	maxch, nchars
int	ttygets()
include	"sgi.com"

begin
	nextch = SGI_NEXTCH(g_kt)
	maxch = SGI_SBUF(g_kt) + SZ_SBUF - nextch + 1

	nchars = ttygets (g_tty, cap, Memc[nextch], maxch)
	if (nchars > 0) {
	    strp = nextch
	    nextch = nextch + nchars + 1
	} else
	    strp = SGI_SBUF(g_kt)

	SGI_NEXTCH(g_kt) = nextch
	return (strp)
end
