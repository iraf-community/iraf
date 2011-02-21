include	<mach.h>
include <imhdr.h>
include "rcamera.h"

define	LEN_KEYWORD	8

# CAM_STORE_KEYWORDS -- store CAMERA specific keywords in the IRAF image header.

procedure cam_store_keywords (parameters, im)

short	parameters[ARB]	# Pointer to program data structure
pointer	im		# Pointer to image

int	fd
real	value
int	stropen()
errchk	stropen, cam_sicard, cam_rcard, cam_hmscard, cam_ymdcard, cam_obscard

begin
	# Open image user area as a string
	fd = stropen (UNKNOWN(im), (LEN_USER_AREA - 1) * SZ_STRUCT, WRITE_ONLY)

	# FITS keyword are formatted and appended to the image user area.
	call cam_sicard (fd, "CCDPICNO", CCD_PICNO(parameters),
	    "ORIGINAL CCD PICTURE NUMBER")
	if (IMAGE_TYPE(parameters) < BEG_IRDATA) {
	    call cam_sicard (fd, "EXPTIME", ITIME(parameters),
	        "ACTUAL INTEGRATION TIME (SECONDS)") 
	    call cam_sicard (fd, "DARKTIME", TTIME(parameters),
	        "TOTAL ELAPSED TIME (SECONDS)")
	    call cam_sicard (fd, "OTIME", OTIME(parameters),
	        "SHUTTER OPEN TIME (SECS)")
	} else if (IMAGE_TYPE(parameters) >= BEG_IRDATA &&
	    IMAGE_TYPE(parameters) <= END_IRDATA) {
	    value = TTIME(parameters) / 1000. + OTIME(parameters)
	    call cam_rcard (fd, "EXPTIME", value, 
	        "ACTUAL INTEGRATION TIME (SECONDS)", 3) 
	}

	# Observation date, time and position cards
	call cam_obscard (fd, "IMAGETYP", IMAGE_TYPE(parameters),
	    "OBJECT,DARK,BIAS,ETC.")
	if (PIC_IRBSCALE(parameters) > 0) {
	    value = 1.0 / PIC_IRBSCALE(parameters)
	    call cam_rcard (fd, "IRBSCALE", value, "PICTURE SCALING FACTOR", 3)
	}
		
	call cam_ymdcard (fd, "DATE-OBS", OBS_YR(parameters),
	    OBS_MON(parameters), OBS_DAY(parameters), "DATE DD/MM/YY")
	call cam_hmscard (fd, "RA", RA_HR(parameters), RA_MIN(parameters),
	    RA_SEC(parameters), "RIGHT ASCENSION (TELESCOPE)")
	call cam_hmscard (fd, "DEC", DEC_DEG(parameters),
	    DEC_MIN(parameters), DEC_SEC(parameters),
	    "DECLINATION (TELESCOPE)")
	value = EPOCH(parameters) / 10.
	call cam_rcard (fd, "EPOCH", value, "EPOCH OF RA AND DEC", 2)
	call cam_hmscard (fd, "ZD", ZD_DEG(parameters), ZD_MIN(parameters),
	    ZD_SEC(parameters), "ZENITH DISTANCE")
	call cam_hmscard (fd, "UT", UT_HR(parameters), UT_MIN(parameters),
	    UT_SEC(parameters), "UNIVERSAL TIME")
	call cam_hmscard (fd, "ST", ST_HR(parameters), ST_MIN(parameters),
	    ST_SEC(parameters), "SIDEREAL TIME")
	if (AIR_MASS(parameters) != 0) {	
	    value = AIR_MASS(parameters) / 100.
	    call cam_rcard (fd, "AIRMASS", value, "AIR MASS", 3)
	}
			    
	# Observation instrumentation cards
	call cam_detcard (fd, "DETECTOR", CAM_HEAD(parameters),
	    "DETECTOR (CCD TYPE, PHOTON COUNTER, ETC)")
	if (GAIN(parameters) != 0) {
	    value = GAIN(parameters) / 100.
	    call cam_rcard (fd, "GAIN", value, "GAIN (ELECTRONS/ADU)", 2)
	}
	if (RDNOISE(parameters) != 0) {
	    value = RDNOISE(parameters) / 100.
	    call cam_rcard (fd, "RDNOISE", value, "READOUT NOISE (ELECTRONS)",
	        1)
	}
	if (PREFLASH(parameters) != 0) {
	    call cam_sicard (fd, "PREFLASH", PREFLASH(parameters),
		"PREFLASH TIME (SECONDS)")
	}
	value = CAM_TEMP(parameters) / 100.
	call cam_rcard (fd, "CAMTEMP", value, "CAMERA TEMPERATURE, DEG C", 2) 
	value = DEW_TEMP(parameters) / 100.
	call cam_rcard (fd, "DEWTEMP", value, "DEWAR TEMPRATURE, DEG C", 2) 
	if (PFLEVEL(parameters) != 0) {
	    call cam_sicard (fd, "PFLEVEL", PFLEVEL(parameters),
		"PREFLASH LEVEL")
	}
	call cam_2sintcard (fd, "FILTERS", F1POS(parameters), F2POS(parameters),
	    "FILTER BOLT POSITIONS")
	call cam_sicard (fd, "TVFILT", TV_FILTER(parameters), "TV FILTER")
	call cam_sicard (fd, "COMPLAMP", COMP_LAMP(parameters),
	    "COMPARISON LAMP")
	if (TILT_POS(parameters) != 0) {
	    call cam_sicard (fd, "TILTPOS", TILT_POS(parameters),
	        "TILT POSITION")
	}
	if (PED_POS(parameters) != 0) {
	    call cam_sicard (fd, "TELEFOCUS", PED_POS(parameters),
		"TELESCOPE FOCUS")
	}

	# Reduction flags
	if (BIAS_PIX(parameters) != 0) {
	    call cam_sicard (fd, "BIASPIX", BIAS_PIX(parameters), "")
	}
	if (BT_FLAG(parameters) != 0) {
	    call cam_sicard (fd, "OVERSCAN", BT_FLAG(parameters),
	        "OVERSCAN VALUE SUBTRACTED")
	    call cam_sicard (fd, "TRIM", short (1), "TRIMMED IMAGE")
	}
	if (BI_FLAG(parameters) != 0) {
	    call cam_sicard (fd, "ZEROCOR", BI_FLAG(parameters),
	        "ZERO IMAGE SUBTRACTED (PREFLASH, BIAS)")
	}
	if (BP_FLAG(parameters) != 0) {
	    call cam_sicard (fd, "FIXPIX", BP_FLAG(parameters),
	        "BAD PIXEL CORRECTION")
	}
	if (CR_FLAG(parameters) != 0) {
	    call cam_sicard (fd, "CRFLAG", CR_FLAG(parameters),
	        "COSMIC RAYS REMOVED")
	}
	if (DK_FLAG(parameters) != 0) {
	    call cam_sicard (fd, "DARKCOR", DK_FLAG(parameters),
	        "DARK SUBTRACTED")
	}
	if (FF_FLAG(parameters) != 0) {
	    call cam_sicard (fd, "FLATCOR", FF_FLAG(parameters),
	        "FLAT FIELD CORRECTION")
	}
	if (FR_FLAG(parameters) != 0) {
	    call cam_sicard (fd, "FRINGCOR", FR_FLAG(parameters),
	        "FRINGING SUBTRACTED")
	}
	if (FR_SC100(parameters) != 0) {
	    call bitpak (int (FR_SC100(parameters)), value, 1, 32)
	    call cam_rcard (fd, "FRINGSCL", value, "FRINGE SCALING", 2)
	}

	# Geometry parameters
	call cam_section (fd, parameters)

	if (PIC_XSUM(parameters) != 0 && PIC_YSUM(parameters) != 0) {
	    call cam_2sintcard (fd, "CCDSUM", PIC_XSUM(parameters),
	        PIC_YSUM(parameters), "ON CHIP SUMMATION (X,Y)")
	}

	call close (fd)

end


#  CAM_RCARD -- Format and append a FITS header card with a real
#  keyword value to the input string buffer.  

procedure cam_rcard (fd, keyword, value, comment, precision)

int	fd			# File descriptor of input string buffer
char	keyword[LEN_KEYWORD]	# FITS keyword
real	value			# Value of FITS keyword
char	comment[ARB]		# Comment string
int	precision		# Number of decimal places output


begin
	call fprintf (fd, "%-8.8s= %20.*f  /  %-45.45s\n")
	    call pargstr (keyword)
	    call pargi (precision)
	    call pargr (value)
	    call pargstr (comment)
end


#  CAM_ICARD -- Format and append a FITS header card with a short integer
#  keyword value to the input string buffer.

procedure cam_icard (fd, keyword, value, comment)

int	fd			# File descriptor of input string buffer
char	keyword[LEN_KEYWORD]	# FITS keyword
int	value			# Value of FITS keyword
char	comment[ARB]		# Comment string

begin
	call fprintf (fd, "%-8.8s= %20d  /  %-45.45s\n")
	    call pargstr (keyword)
	    call pargs (value)
	    call pargstr (comment)
end


#  CAM_SICARD -- Format and append a FITS header card with a short integer
#  keyword value to the input string buffer.

procedure cam_sicard (fd, keyword, value, comment)

int	fd			# File descriptor of input string buffer
char	keyword[LEN_KEYWORD]	# FITS keyword
short	value			# Value of FITS keyword
char	comment[ARB]		# Comment string

begin
	call fprintf (fd, "%-8.8s= %20d  /  %-45.45s\n")
	    call pargstr (keyword)
	    call pargs (value)
	    call pargstr (comment)
end


# CAM_CCARD -- Procedure to format a FITS string parameter.

procedure cam_ccard (fd, keyword, param, maxch, comment)

int	fd			# Output file descriptor
char	keyword[ARB]		# FITS keyword
char	param[ARB]		# FITS string parameter
int	maxch			# maximum number of chars in parameter
char	comment[ARB]		# Comment string

int	i, maxchar, nblanks

begin
	# Trim off trailing blanks and compute length of string to encode.
	for (i = maxch; (i >= 1) && param[i] == ' '; i = i - 1) 
	    ;
	param[i+1] = EOS

	maxchar = max (LEN_KEYWORD, min (i, LEN_OBJECT))
	nblanks = min (LEN_OBJECT - maxchar + 2, LEN_OBJECT)

	# Print the string.
	if (nblanks < 45) {
	    call fprintf (fd, "%-8.8s= '%*.*s'%33t/  %*.*s\n")
	        call pargstr (keyword)
	        call pargi (-maxchar)
	        call pargi (maxchar)
	        call pargstr (param)
	        call pargi (-nblanks)
	        call pargi (nblanks)
	        call pargstr (comment)
	} else {
	    call fprintf (fd, "%-8.8s= '%*.*s'%33t/  %-45.-45s\n")
	        call pargstr (keyword)
	        call pargi (-maxchar)
	        call pargi (maxchar)
	        call pargstr (param)
	        call pargstr (comment)
	}
end


#  CAM_HMSCARD -- Format and append a FITS header card to the input
#  string buffer.  The value is input as 3 short integers; it is output
#  in HH:MM:SS.  The procedure can be used for RA, DEC
#  and ST, UT and HA.

procedure cam_hmscard (fd, keyword, hours, minutes, seconds, comment)

int	fd			# File descriptor
char	keyword[LEN_KEYWORD]	# FITS keyword
short	hours			# Hours
short	minutes			# Minutes
short	seconds			# Seconds
char	comment			# Comment string


begin
	call fprintf (fd, "%-8.8s= '%s%02d:%02d:%02d'%33t/  %-45.45s\n")
	    call pargstr (keyword)
	    if (hours < 0 || minutes < 0 || seconds < 0)
		call pargstr ("-")
	    else
		call pargstr (" ")
	    call pargs (abs(hours))
	    call pargs (abs(minutes))
	    call pargs (abs(seconds))
	    call pargstr (comment)
end


define	CENTURY		100

#  CAM_YMDCARD - Format and append a FITS header card to the input
#  string buffer.  The value is input as 3 short integers; it is output
#  in the format dd/mm/yy.

procedure cam_ymdcard (fd, keyword, years, months, days, comment)

int	fd			# File descriptor
char	keyword[ARB]		# FITS keyword
short	years			# Hours
short	months			# Minutes
short	days			# Seconds
char	comment			# Comment string

short	const

begin
	const = mod (int (years), CENTURY)
	call fprintf (fd, "%-8.8s= '%02d/%02d/%02d'%33t/  %-45.45s\n")
	    call pargstr (keyword)
	    call pargs (days)
	    call pargs (months)
	    call pargs (const)
	    call pargstr (comment)
end


# CAM_OBSCARD -- Procedure to code the object type into a FITS
# card

procedure cam_obscard (fd, keyword, data_code, comment)

int	fd			# File descriptor
char	keyword[LEN_KEYWORD]	# FITS keyword
short	data_code		# type of data
char	comment[ARB]		# coment string

int	nchars
pointer	sp, str
int	gstrcpy(), itoc()

begin
	call smark (sp)
	call salloc (str, LEN_OBJECT, TY_CHAR)

	switch (data_code) {
	CASE OBJECT,IROBJECT:
	    nchars = gstrcpy ("OBJECT", Memc[str], LEN_OBJECT)
	CASE DARK,IRDARK:
	    nchars = gstrcpy ("DARK", Memc[str], LEN_OBJECT)
	CASE PFLAT,IRPFLAT:
	    nchars = gstrcpy ("PROJECTOR FLAT", Memc[str], LEN_OBJECT)
	CASE SFLAT,IRSFLAT:
	    nchars = gstrcpy ("SKY FLAT", Memc[str], LEN_OBJECT)
	CASE COMP,IRCOMP:
	    nchars = gstrcpy ("COMPARISON", Memc[str], LEN_OBJECT)
	CASE BIAS,IRBIAS:
	    nchars =gstrcpy ("BIAS", Memc[str], LEN_OBJECT)
	CASE DFLAT,IRDFLAT:
	    nchars = gstrcpy ("DOME FLAT", Memc[str], LEN_OBJECT)
	CASE MASK,IRMASK:
	    nchars = gstrcpy ("MASK", Memc[str], LEN_OBJECT)
	CASE MULT,IRMULT:
	    nchars = gstrcpy ("MULTIPLE EXP", Memc[str], LEN_OBJECT)
	CASE SCAN,IRSCAN:
	    nchars = gstrcpy ("SCAN", Memc[str], LEN_OBJECT)
	case OCCULTATION:
	    nchars = gstrcpy ("OCCULTATION", Memc[str], LEN_OBJECT)
	case IRGRID:
	    nchars = gstrcpy ("INFRARED GRID", Memc[str], LEN_OBJECT)
	case IRSPECTRA:
	    nchars = gstrcpy ("INFRARED SPECTRA", Memc[str], LEN_OBJECT)
	case IRSPECKLE:
	    nchars = gstrcpy ("INFRARED SPECKLE", Memc[str], LEN_OBJECT)
	default:
	    nchars = itoc (int (data_code), Memc[str], LEN_OBJECT)
	}

	call cam_ccard (fd, keyword, Memc[str], nchars, comment)

	call sfree (sp)
end


# CAM_DETCARD -- Procedure to code the detector into a FITS card.

procedure cam_detcard (fd, keyword, data_code, comment)

int	fd			# File descriptor
char	keyword[LEN_KEYWORD]	# FITS keyword
short	data_code		# type of data
char	comment[ARB]		# coment string

int	nchars
pointer	sp, str	
int	gstrcpy(), itoc()

begin
	call smark (sp)
	call salloc (str, LEN_OBJECT, TY_CHAR)

	switch (data_code) {
	CASE TEK1:
	    nchars = gstrcpy ("TEK1", Memc[str], LEN_OBJECT)
	CASE RCA3:
	    nchars = gstrcpy ("RCA3", Memc[str], LEN_OBJECT)
	CASE TI1:
	    nchars = gstrcpy ("TI1", Memc[str], LEN_OBJECT)
	CASE RCA0:
	    nchars = gstrcpy ("RCA0", Memc[str], LEN_OBJECT)
	CASE RCA2:
	    nchars = gstrcpy ("RCA2", Memc[str], LEN_OBJECT)
	CASE RCA1:
	    nchars = gstrcpy ("RCA1", Memc[str], LEN_OBJECT)
	CASE TI2:
	    nchars = gstrcpy ("TI2", Memc[str], LEN_OBJECT)
	CASE TI3:
	    nchars = gstrcpy ("TI3", Memc[str], LEN_OBJECT)
	CASE TI4:
	    nchars = gstrcpy ("TI4", Memc[str], LEN_OBJECT)
	CASE TI5:
	    nchars = gstrcpy ("TI5", Memc[str], LEN_OBJECT)
	default:
	    nchars = itoc (int (data_code), Memc[str], LEN_OBJECT)
	}

	call cam_ccard (fd, keyword, Memc[str], nchars, comment)

	call sfree (sp)
end
 
  
# CAM_2SINTCARD -- Procedure to encode the filter positions into a FITS card.

procedure cam_2sintcard (fd, keyword, sint1, sint2, comment)

int	fd			# File descriptor
char	keyword[LEN_KEYWORD]	# FITS keyword
short	sint1			# First short integer
short	sint2			# Second short integer
char	comment[ARB]		# comment string

int	maxch
pointer	sp, str
int	strlen()

begin
	call smark (sp)
	call salloc (str, LEN_OBJECT, TY_CHAR)

	call sprintf (Memc[str], LEN_OBJECT, "%d %d")
	    call pargs (sint1)
	    call pargs (sint2)

	maxch = max (strlen (Memc[str]), LEN_KEYWORD)
	call fprintf (fd, "%-8.8s= '%*.*s'%33t/  %-45.45s\n")
	    call pargstr (keyword)
	    call pargi (-maxch)
	    call pargi (maxch)
	    call pargstr (Memc[str])
	    call pargstr (comment)

	call sfree (sp)
end


# CAM_SECTION -- Procedure  to encode the camera keywords which are formated
# like sections.

procedure cam_section (fd, parameters)

int	fd			# pointer to string file descriptor
short	parameters[ARB]		# list of parameters

int	nx1, nx2, ny1, ny2, maxch
pointer	sp, str
int	strlen()

begin
	call smark (sp)
	call salloc (str, LEN_OBJECT, TY_CHAR)

	# Redefine the dimensions.
	if (PIC_NXRAW(parameters) <= 0)
	    PIC_NXRAW(parameters) = PIC_NX(parameters)
	if (PIC_NYRAW(parameters) <= 0)
	    PIC_NYRAW(parameters) = PIC_NY(parameters)

	# Write the DATASEC keyword.
	nx1 = 1 + PIC_XPRE(parameters) + PIC_NXOFF(parameters)
	nx2 = PIC_XPRE(parameters) + PIC_NXOFF(parameters) + PIC_NX(parameters)
	ny1 = 1 + PIC_YPRE(parameters) + PIC_NYOFF(parameters)
	ny2 = PIC_YPRE(parameters) + PIC_NYOFF(parameters) + PIC_NY(parameters)
	call sprintf (Memc[str], LEN_OBJECT, "[%d:%d,%d:%d]")
	    call pargi (nx1)
	    call pargi (nx2)
	    call pargi (ny1)
	    call pargi (ny2)
	maxch = strlen (Memc[str])
	call cam_ccard (fd, "DATASEC", Memc[str], maxch,
	    "IMAGE PORTION OF FRAME")

	# Write the BIASSEC keyword.
	if (BT_FLAG(parameters) == 0) {

	    if (PIC_XPRE(parameters) != 0) {
	        nx1 = 1 + PIC_S1(parameters)
	        nx2 = PIC_X0(parameters) - PIC_S2(parameters)
		ny1 = 1 + PIC_YPRE(parameters) + PIC_YT1(parameters)
		ny2 = PIC_YPRE(parameters) + PIC_NYRAW(parameters) - 
		    PIC_YT2(parameters)
	    }
	    if (PIC_X0(parameters) != 0) {
	        nx1 = 1 + PIC_XPRE(parameters) + PIC_NXRAW(parameters) +
	            PIC_S1(parameters)
	        nx2 = PIC_XPRE(parameters) + PIC_NXRAW(parameters) +
	            PIC_X0(parameters) - PIC_S2(parameters)
		ny1 = 1 + PIC_YPRE(parameters) + PIC_YT1(parameters)
		ny2 = PIC_YPRE(parameters) + PIC_NYRAW(parameters) -
		    PIC_YT2(parameters)
	    }

	    if (PIC_YPRE(parameters) != 0) {
		nx1 = 1 + PIC_XPRE(parameters) + PIC_XT1(parameters)
		nx2 = PIC_XPRE(parameters) +  PIC_NXRAW(parameters) -
		    PIC_XT2(parameters)
	        ny1 = 1 + PIC_S1(parameters)
	        ny2 = PIC_Y0(parameters) - PIC_S2(parameters)
	    }
	    if (PIC_Y0(parameters) != 0) {
		nx1 = 1 + PIC_XPRE(parameters) + PIC_XT1(parameters)
		nx2 = PIC_XPRE(parameters) +  PIC_NXRAW(parameters) -
		    PIC_XT2(parameters)
	        ny1 = 1 + PIC_YPRE(parameters) + PIC_NYRAW(parameters) +
	            PIC_S1(parameters)
	        ny2 = PIC_YPRE(parameters) + PIC_NYRAW(parameters) +
	            PIC_Y0(parameters) - PIC_S2(parameters)
	    }

	    call sprintf (Memc[str], LEN_OBJECT, "[%d:%d,%d:%d]")
	        call pargi (nx1)
	        call pargi (nx2)
	        call pargi (ny1)
	        call pargi (ny2)
	    maxch = strlen (Memc[str])
	    call cam_ccard (fd, "BIASSEC", Memc[str], maxch,
	        "OVERSCAN PORTION OF FRAME")
	}

	# Write the TRIMSEC keyword.
	if (BT_FLAG(parameters) == 0) {
	    nx1 = 1 + PIC_XPRE(parameters) +PIC_NXOFF(parameters) +
	        PIC_XT1(parameters)
	    nx2 = PIC_XPRE(parameters) + PIC_NXOFF(parameters) +
	        PIC_NX(parameters) - PIC_XT2(parameters)
	    ny1 = 1 + PIC_YPRE(parameters) + PIC_NYOFF(parameters) +
	        PIC_YT1(parameters)
	    ny2 = PIC_YPRE(parameters) + PIC_NYOFF(parameters) +
	        PIC_NY(parameters) - PIC_YT2(parameters)
	    call sprintf (Memc[str], LEN_OBJECT, "[%d:%d,%d:%d]")
	        call pargi (nx1)
	        call pargi (nx2)
	        call pargi (ny1)
	        call pargi (ny2)
	    maxch = strlen (Memc[str])
	    call cam_ccard (fd, "TRIMSEC", Memc[str], maxch,
	    "REGION TO BE EXTRACTED AFTER PROC")
	}

	# Write the CCDSEC keyword.
	nx1 = 1 + PIC_XPRE(parameters)
	nx2 = PIC_XPRE(parameters) + PIC_NXRAW(parameters)
	ny1 = 1 + PIC_XPRE(parameters)
	ny2 = PIC_YPRE(parameters) + PIC_NYRAW(parameters)
	call sprintf (Memc[str], LEN_OBJECT, "[%d:%d,%d:%d]")
	    call pargi (nx1)
	    call pargi (nx2)
	    call pargi (ny1)
	    call pargi (ny2)
	maxch = strlen (Memc[str])
	call cam_ccard (fd, "CCDSEC", Memc[str], maxch,
	    "ORIENTATION TO FULL FORMAT FRAME")

	# Write the ORIGSEC keyword.
	nx1 = 1 + PIC_XPRE(parameters)
	nx2 = PIC_XPRE(parameters) + PIC_NXRAW(parameters)
	ny1 = 1 + PIC_XPRE(parameters)
	ny2 = PIC_YPRE(parameters) + PIC_NYRAW(parameters)
	call sprintf (Memc[str], LEN_OBJECT, "[%d:%d,%d:%d]")
	    call pargi (nx1)
	    call pargi (nx2)
	    call pargi (ny1)
	    call pargi (ny2)
	maxch = strlen (Memc[str])
	call cam_ccard (fd, "ORIGSEC", Memc[str], maxch,
	    "ORIGINAL SIZE OF FULL FORMAT FRAME")

	call sfree (sp)
end
