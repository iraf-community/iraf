# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<gki.h>
include	<gset.h>
include	"stdgraph.h"

# STG_INIT -- Initialize the stdgraph data structures from the graphcap entry
# for the device.  Called once, at OPENWS time, with the TTY pointer already
# set in the common.  The companion routine STG_RESET initializes the attribute
# packets when the screen is cleared.

procedure stg_init (tty, devname)

pointer	tty			# graphcap descriptor
char	devname[ARB]		# device name

pointer	nextch
bool	first_time
int	maxch, i, junk
real	char_height, char_width, char_size

bool	ttygetb()
real	ttygetr()
pointer	stg_gstring()
int	ttygets(), ttygeti(), btoi(), stg_encode(), gstrcpy()
include	"stdgraph.com"
data	first_time /true/

begin
	# One time initialization.
	if (first_time) {
	    # Initialize the Tek 4012 coordinate encoding lookup tables.
	    do i = 1, TEK_XRES {
		g_hixy[i] = (i-1) / 40B + 40B
		g_lox[i]  = mod ((i-1), 40B) + 100B
	    }
	    do i = 1, TEK_YRES
		g_loy[i]  = mod ((i-1), 40B) + 140B

	    first_time = false
	}

	# Allocate the stdgraph descriptor and the string buffer.
	call calloc (g_sg, LEN_SG, TY_STRUCT)
	call malloc (SG_SBUF(g_sg), SZ_SBUF, TY_CHAR)

	# Init string buffer parameters.  The first char of the string buffer
	# is reserved as a null string, used for graphcap control strings
	# omitted from the graphcap entry for the device.

	SG_SZSBUF(g_sg) = SZ_SBUF
	SG_NEXTCH(g_sg) = SG_SBUF(g_sg) + 1
	Memc[SG_SBUF(g_sg)] = EOS

	# Set the software device resolution and the coordinate transformations
	# to the resolution space and from GKI to device coords.  The values
	# g_[xy]res were initialized when the kernel was opened by the main
	# program.

	call stg_resolution (g_xres, g_yres)

	# Initialize the encoder.  The graphcap parameter LR contains encoder
	# instructions to perform any device dependent initialization required.

	call aclri (g_reg, NREGISTERS)
	nextch = SG_NEXTCH(g_sg)

	g_reg[E_IOP] = 1
	g_reg[E_TOP] = SZ_MEMORY
	if (ttygets (tty, "LR", Memc[nextch], SZ_SBUF-1) > 0)
	    junk = stg_encode (Memc[nextch], g_mem, g_reg)

	# If the device does not support hardware character generation, set
	# txquality to high to get software character generation.

	if (!ttygetb (tty, "tx"))
	    g_hardchar = GT_HIGH

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

	# If the device has a set of discrete character sizes, get the
	# size of each by fetching the parameter "tN", where the N is
	# a digit specifying the text size index.  Compute the height and
	# width of each size character from the "ch" and "cw" parameters
	# and the relative scale of character size I.

	SG_NCHARSIZES(g_sg) = min (MAX_CHARSIZES, ttygeti (tty, "th"))
	nextch = SG_NEXTCH(g_sg)

	if (SG_NCHARSIZES(g_sg) <= 0) {
	    SG_NCHARSIZES(g_sg) = 1
	    SG_CHARSIZE(g_sg,1) = 1.0
	} else {
	    Memc[nextch+2] = EOS
	    for (i=1;  i <= SG_NCHARSIZES(g_sg);  i=i+1) {
		Memc[nextch] = 't'
		Memc[nextch+1] = TO_DIGIT(i)
		char_size = ttygetr (tty, Memc[nextch])
		SG_CHARSIZE(g_sg,i)   = char_size
		SG_CHARHEIGHT(g_sg,i) = char_height * char_size
		SG_CHARWIDTH(g_sg,i)  = char_width  * char_size
	    }
	}

	# Initialize the output parameters.  All boolean parameters are stored
	# as integer flags.  All string valued parameters are stored in the
	# string buffer, saving a pointer to the string in the stdgraph
	# descriptor.  If the capability does not exist the pointer is set to
	# point to the null string at the beginning of the string buffer.

	SG_POLYLINE(g_sg)   = btoi (ttygetb (tty, "PL"))
	SG_POLYMARKER(g_sg) = btoi (ttygetb (tty, "pm"))
	SG_FILLAREA(g_sg)   = btoi (ttygetb (tty, "fa"))

	SG_ENCODEXY(g_sg)   = stg_gstring ("XY")
	g_xy = SG_ENCODEXY(g_sg)

	SG_STARTDRAW(g_sg)  = stg_gstring ("DS")
	SG_ENDDRAW(g_sg)    = stg_gstring ("DE")
	SG_STARTMOVE(g_sg)  = stg_gstring ("VS")
	SG_ENDMOVE(g_sg)    = stg_gstring ("VE")
	SG_STARTMARK(g_sg)  = stg_gstring ("MS")
	SG_ENDMARK(g_sg)    = stg_gstring ("ME")
	SG_STARTFILL(g_sg)  = stg_gstring ("FS")
	SG_ENDFILL(g_sg)    = stg_gstring ("FE")
	SG_STARTTEXT(g_sg)  = stg_gstring ("TS")
	SG_ENDTEXT(g_sg)    = stg_gstring ("TE")

	# Initialize the input parameters.
	SG_CURSOR(g_sg) = 0
	SG_UPDCURSOR(g_sg)  = btoi (ttygetb (tty, "UC"))
	SG_CURSOR_X(g_sg)   = 0
	SG_CURSOR_Y(g_sg)   = 0

	# Save the device string in the descriptor.
	nextch = SG_NEXTCH(g_sg)
	SG_DEVNAME(g_sg) = nextch
	maxch = SG_SBUF(g_sg) + SZ_SBUF - nextch + 1
	nextch = nextch + gstrcpy (devname, Memc[nextch], maxch) + 1

	# Initialize the UIFNAME field.
	SG_UIFNAME(g_sg) = nextch
	Memc[nextch] = EOS
	nextch = nextch + SZ_UIFNAME + 1
	SG_NEXTCH(g_sg) = nextch
end


# STG_GSTRING -- Get a string value parameter from the graphcap table,
# placing the string at the end of the string buffer.  If the device does
# not have the named capability return a pointer to the null string,
# otherwise return a pointer to the string.  Since pointers are used,
# rather than indices, the string buffer is fixed in size.  The additional
# degree of indirection required with an index was not considered worthwhile
# in this application since the graphcap entries are never very large.

pointer procedure stg_gstring (cap)

char	cap[ARB]		# device capability to be fetched
pointer	strp, nextch
int	maxch, nchars
int	ttygets()
include	"stdgraph.com"

begin
	nextch = SG_NEXTCH(g_sg)
	maxch = SG_SBUF(g_sg) + SZ_SBUF - nextch + 1

	nchars = ttygets (g_tty, cap, Memc[nextch], maxch)
	if (nchars > 0) {
	    strp = nextch
	    nextch = nextch + nchars + 1
	} else
	    strp = SG_SBUF(g_sg)

	SG_NEXTCH(g_sg) = nextch
	return (strp)
end
