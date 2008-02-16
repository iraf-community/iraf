# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<gki.h>
include	"ccp.h"

# CCP_INIT -- Initialize the CCP data structures from the graphcap entry
# for the plotter.  Called once, at OPENWS time, with the TTY pointer already
# set in the common.  The companion routine CCP_RESET initializes the attribute
# packets.

procedure ccp_init (tty, devname)

pointer	tty			# graphcap descriptor
char	devname[ARB]		# device name

pointer	nextch
int	maxch, i
real	char_height, char_width, char_size, xres, yres, xwidth, yheight
real	mper_punit
bool	ttygetb()
real	ttygetr()
int	ttygeti(), btoi(), gstrcpy()
include	"ccp.com"

begin
	# Allocate the CCP descriptor, string buffer, and x,y segment buffers.
	if (g_cc == NULL) {
	    call calloc (g_cc, LEN_CCP, TY_STRUCT)
	    call malloc (CCP_SBUF(g_cc), SZ_SBUF, TY_CHAR)
	}

	# Init string buffer parameters.  The first char of the string buffer
	# is reserved as a null string, used for graphcap control strings
	# omitted from the graphcap entry for the device.

	CCP_SZSBUF(g_cc) = SZ_SBUF
	CCP_NEXTCH(g_cc) = CCP_SBUF(g_cc) + 1
	Memc[CCP_SBUF(g_cc)] = EOS

	# Get the device resolution, dimensions in meters, and meter-to-pltr
	# unit conversion factor from graphcap; if none are specified, use
	# compile-time constants.

	xres = ttygeti (tty, "xr")
	if (xres <= 0)
	    xres = GKI_MAXNDC
	yres = ttygeti (tty, "yr")
	if (yres <= 0)
	    yres = GKI_MAXNDC

	xwidth  = ttygetr (tty, "xs")
	if (xwidth <= 0.0)
	    xwidth = MAX_PL_XWIDTH
	yheight = ttygetr (tty, "ys")
	if (yheight <= 0.0)
	    yheight = MAX_PL_YHEIGHT

	mper_punit = ttygetr (tty, "MP")
	if (mper_punit <= 0.0)
	    mper_punit = DEF_MPER_PUNIT
	
	# Set up coordinate transformation if not explicitly specified to 
	# kernel task at run time.  Scale determined from graphcap is saved 
	# in case ccp_escape gets a metacode scale it cannot translate.
	# Set up default scale such that a full max_gki_ndc plot will fit in y.

	g_ydefault_scale = yheight / (mper_punit * GKI_MAXNDC)
	if (IS_INDEF (g_ytask_scale))
	    g_yndcto_p = g_ydefault_scale

	g_xdefault_scale = xwidth / (mper_punit * GKI_MAXNDC)
	if (IS_INDEF (g_xtask_scale)) 
	    g_xndcto_p = g_xdefault_scale

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

	# If the plotter has a set of discrete character sizes, get the
	# size of each by fetching the parameter "tN", where the N is
	# a digit specifying the text size index.  Compute the height and
	# width of each size character from the "ch" and "cw" parameters
	# and the relative scale of character size I.

	CCP_NCHARSIZES(g_cc) = min (MAX_CHARSIZES, ttygeti (tty, "th"))
	nextch = CCP_NEXTCH(g_cc)

	if (CCP_NCHARSIZES(g_cc) <= 0) {
	    CCP_NCHARSIZES(g_cc) = 1
	    CCP_CHARSIZE(g_cc,1) = 1.0
	    CCP_CHARHEIGHT(g_cc,1) = char_height
	    CCP_CHARWIDTH(g_cc,1)  = char_width
	} else {
	    Memc[nextch+2] = EOS
	    for (i=1;  i <= CCP_NCHARSIZES(g_cc);  i=i+1) {
		Memc[nextch] = 't'
		Memc[nextch+1] = TO_DIGIT(i)
		char_size = ttygetr (tty, Memc[nextch])
		CCP_CHARSIZE(g_cc,i)   = char_size
		CCP_CHARHEIGHT(g_cc,i) = char_height * char_size
		CCP_CHARWIDTH(g_cc,i)  = char_width  * char_size
	    }
	}

	# Get dash length, gap length, and n-tracing separation width:
	if (IS_INDEF (g_dashlen)) {
	    g_dashlen = ttygetr (tty, "DL")
	    if (g_dashlen <= 0.0)
		g_dashlen = DEF_DASHLEN
	}
	if (IS_INDEF (g_gaplen)) {
	    g_gaplen = ttygetr (tty, "GL")
	    if (g_gaplen <= 0.0)
		g_gaplen = DEF_GAPLEN
	}
	if (IS_INDEF (g_plwsep)) {
	    g_plwsep = ttygetr (tty, "PW")
	    if (g_plwsep <= 0.0)
		g_plwsep = DEF_PLWSEP
	}
		
	# Initialize the output parameters.  All boolean parameters are stored
	# as integer flags.  All string valued parameters are stored in the
	# string buffer, saving a pointer to the string in the CCP
	# descriptor.  If the capability does not exist the pointer is set to
	# point to the null string at the beginning of the string buffer.

	CCP_POLYLINE(g_cc)   = btoi (ttygetb (tty, "pl"))
	CCP_POLYMARKER(g_cc) = btoi (ttygetb (tty, "pm"))
	CCP_FILLAREA(g_cc)   = btoi (ttygetb (tty, "fa"))
	CCP_FILLSTYLE(g_cc)  =       ttygeti (tty, "fs")
	CCP_ROAM(g_cc)	     = btoi (ttygetb (tty, "ro"))
	CCP_ZOOM(g_cc)	     = btoi (ttygetb (tty, "zo"))
	CCP_ZRES(g_cc)	     =       ttygeti (tty, "zr")
	CCP_SELERASE(g_cc)   = btoi (ttygetb (tty, "se"))
	CCP_PIXREP(g_cc)     = btoi (ttygetb (tty, "pr"))

	# Initialize the input parameters.

	CCP_CURSOR(g_cc)     = 1

	# Save the device string in the descriptor.
	nextch = CCP_NEXTCH(g_cc)
	CCP_DEVNAME(g_cc) = nextch
	CCP_DEVCHAN(g_cc) = CCP_LDEV
	maxch = CCP_SBUF(g_cc) + SZ_SBUF - nextch + 1
	nextch = nextch + gstrcpy (devname, Memc[nextch], maxch) + 1
	CCP_NEXTCH(g_cc) = nextch

	# Initialize maximum-x tracker, used for "newframe" in ccp_clear.
	g_max_x = 0.0
end
