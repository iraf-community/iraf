# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<gki.h>
include	"../lib/ids.h"

# IDS_INIT -- Initialize the ids data structures from the graphcap entry
# for the device.  Called once, at OPENWS time, with the TTY pointer already
# set in the common.

procedure ids_init (tty, devname)

pointer	tty			# graphcap descriptor
char	devname[ARB]		# device name

pointer	nextch
int	maxch, i
real	char_height, char_width, char_size

bool	ttygetb()
real	ttygetr()
int	ttygeti(), btoi(), gstrcpy()

include	"../lib/ids.com"

begin
	# Allocate the ids descriptor and the string buffer.
	if ( i_kt == NULL) {
	    call calloc (i_kt, LEN_IDS, TY_STRUCT)
	    call malloc (IDS_SBUF(i_kt), SZ_SBUF, TY_CHAR)
	    call malloc (IDS_BITPL(i_kt), IDS_MAXBITPL+1, TY_SHORT)
	} else {
	    call mfree (IDS_FRAME(i_kt), TY_SHORT)
	}


	# Init string buffer parameters.  The first char of the string buffer
	# is reserved as a null string, used for graphcap control strings
	# omitted from the graphcap entry for the device.

	IDS_SZSBUF(i_kt) = SZ_SBUF
	IDS_NEXTCH(i_kt) = IDS_SBUF(i_kt) + 1
	Memc[IDS_SBUF(i_kt)] = EOS

	# get the device resolution from the graphcap entry.

	i_xres = ttygeti (tty, "xr")
	if (i_xres <= 0)
	    i_xres = 512
	i_yres = ttygeti (tty, "yr")
	if (i_yres <= 0)
	    i_yres = 512


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

	IDS_NCHARSIZES(i_kt) = min (MAX_CHARSIZES, ttygeti (tty, "th"))
	nextch = IDS_NEXTCH(i_kt)

	if (IDS_NCHARSIZES(i_kt) <= 0) {
	    IDS_NCHARSIZES(i_kt) = 1
	    IDS_CHARSIZE(i_kt,1) = 1.0
	    IDS_CHARHEIGHT(i_kt,1) = char_height
	    IDS_CHARWIDTH(i_kt,1)  = char_width
	} else {
	    Memc[nextch+2] = EOS
	    for (i=1;  i <= IDS_NCHARSIZES(i_kt);  i=i+1) {
		Memc[nextch] = 't'
		Memc[nextch+1] = TO_DIGIT(i)
		char_size = ttygetr (tty, Memc[nextch])
		IDS_CHARSIZE(i_kt,i)   = char_size
		IDS_CHARHEIGHT(i_kt,i) = char_height * char_size
		IDS_CHARWIDTH(i_kt,i)  = char_width  * char_size
	    }
	}

	# Initialize the output parameters.  All boolean parameters are stored
	# as integer flags.  All string valued parameters are stored in the
	# string buffer, saving a pointer to the string in the ids
	# descriptor.  If the capability does not exist the pointer is set to
	# point to the null string at the beginning of the string buffer.

	IDS_POLYLINE(i_kt)   = btoi (ttygetb (tty, "pl"))
	IDS_POLYMARKER(i_kt) = btoi (ttygetb (tty, "pm"))
	IDS_FILLAREA(i_kt)   = btoi (ttygetb (tty, "fa"))
	IDS_FILLSTYLE(i_kt)  =       ttygeti (tty, "fs")
	IDS_ROAM(i_kt)	     = btoi (ttygetb (tty, "ro"))
	IDS_CANZM(i_kt)	     = btoi (ttygetb (tty, "zo"))
	IDS_ZRES(i_kt)	     =       ttygeti (tty, "zr")
	IDS_CELLARRAY(i_kt)  = btoi (ttygetb (tty, "ca"))
	IDS_SELERASE(i_kt)   = btoi (ttygetb (tty, "se"))

	# how many image frames and graph (bit)planes do we get to play with?

	i_maxframes = ttygeti(tty, "ip")
	if ( i_maxframes < 1 )
	    i_maxframes = 1
	i_maxgraph  = ttygeti(tty, "gp")
	i_maxframes = min(int(i_maxframes), IDS_MAXIMPL)
	i_maxgraph  = min(int(i_maxgraph),  IDS_MAXGRPL)

	# allocate space for the frame descriptors
	# the "2" accounts for possible graphics channel ( see ids_expand.x)
	# and the trailing IDS_EOD

	call malloc (IDS_FRAME(i_kt), max(i_maxframes,i_maxgraph)+2, TY_SHORT)

	# Initialize the input parameters: last cursor used.

	IDS_LCURSOR(i_kt)     = 1

	# Save the device string in the descriptor.
	nextch = IDS_NEXTCH(i_kt)
	IDS_DEVNAME(i_kt) = nextch
	maxch = IDS_SBUF(i_kt) + SZ_SBUF - nextch + 1
	nextch = nextch + gstrcpy (devname, Memc[nextch], maxch) + 1
	IDS_NEXTCH(i_kt) = nextch

end


# IDS_GSTRING -- Get a string value parameter from the graphcap table,
# placing the string at the end of the string buffer.  If the device does
# not have the named capability return a pointer to the null string,
# otherwise return a pointer to the string.  Since pointers are used,
# rather than indices, the string buffer is fixed in size.  The additional
# degree of indirection required with an index was not considered worthwhile
# in this application since the graphcap entries are never very large.

pointer procedure ids_gstring (cap)

char	cap[ARB]		# device capability to be fetched
pointer	strp, nextch
int	maxch, nchars
int	ttygets()

include	"../lib/ids.com"

begin
	nextch = IDS_NEXTCH(i_kt)
	maxch = IDS_SBUF(i_kt) + SZ_SBUF - nextch + 1

	nchars = ttygets (i_tty, cap, Memc[nextch], maxch)
	if (nchars > 0) {
	    strp = nextch
	    nextch = nextch + nchars + 1
	} else
	    strp = IDS_SBUF(i_kt)

	IDS_NEXTCH(i_kt) = nextch
	return (strp)
end
