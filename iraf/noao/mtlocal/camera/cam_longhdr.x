include	<mach.h>
include "rcamera.h"

define	LEN_KEYWORD	8

# CAM_LONG_HEADER -- Print CAMERA longheader on the standard output.

procedure cam_long_header (parameters, text)

short	parameters[ARB]	# Pointer to program data structure
char	text[ARB]	# ID string

real	value
errchk	cam_sicard, cam_rcard, cam_hmscard, cam_ymdcard, cam_obscard

begin
	# FITS keyword are formatted and appended to the image user area.
	call cam_ccard (STDOUT, "OBJECT", text, LEN_CAM_TEXT,
	    "TITLE OF IMAGE")
	call cam_sicard (STDOUT, "NAXIS1", NAXIS1(parameters),
	    "NUMBER OF IMAGE COLUMNS")
	call cam_sicard (STDOUT, "NAXIS2", NAXIS2(parameters),
	    "NUMBER OF IMAGE ROWS")
	call cam_sicard (STDOUT, "RECLEN", REC_LEN(parameters),
	    "RCAMERA RECORD LENGTH")
	call cam_sicard (STDOUT, "CCDPICNO", CCD_PICNO(parameters),
	    "ORIGINAL CCD PICTURE NUMBER")
	if (IMAGE_TYPE(parameters) < BEG_IRDATA) {
	    call cam_sicard (STDOUT, "EXPTIME", ITIME(parameters),
	        "ACTUAL INTEGRATION TIME (SECONDS)") 
	    call cam_sicard (STDOUT, "DARKTIME", TTIME(parameters),
	        "TOTAL ELAPSED TIME (SECONDS)")
	    call cam_sicard (STDOUT, "OTIME", OTIME(parameters),
	        "ACTUAL INTEGRATION TIME (SECS)")
	} else if (IMAGE_TYPE(parameters) >= BEG_IRDATA &&
	    IMAGE_TYPE(parameters) <= END_IRDATA) {
	    value = TTIME(parameters) / 1000. + OTIME(parameters)
	    call cam_rcard (STDOUT, "EXPTIME", value,
		"ACTUAL INTEGRATION TIME (SECONDS)", 3)
	}

	# Observation date, time and position cards
	call cam_obscard (STDOUT, "IMAGETYP", IMAGE_TYPE(parameters),
	    "OBJECT,DARK,BIAS,ETC.")
	if (PIC_IRBSCALE(parameters) > 0) {
	    value = 1.0 / PIC_IRBSCALE(parameters)
	    call cam_rcard (STDOUT, "IRBSCALE", value, "PICTURE SCALING FACTOR",
		3)
	}
	call cam_ymdcard (STDOUT, "DATE-OBS", OBS_YR(parameters),
	    OBS_MON(parameters), OBS_DAY(parameters), "DATE DD/MM/YY")
	call cam_hmscard (STDOUT, "RA", RA_HR(parameters), RA_MIN(parameters),
	    RA_SEC(parameters), "RIGHT ASCENSION (TELESCOPE)")
	call cam_hmscard (STDOUT, "DEC", DEC_DEG(parameters),
	    DEC_MIN(parameters), DEC_SEC(parameters),
	    "DECLINATION (TELESCOPE)")
	value = EPOCH(parameters) / 10.
	call cam_rcard (STDOUT, "EPOCH", value, "EPOCH OF RA AND DEC", 2)
	call cam_hmscard (STDOUT, "ZD", ZD_DEG(parameters), ZD_MIN(parameters),
	    ZD_SEC(parameters), "ZENITH DISTANCE")
	call cam_hmscard (STDOUT, "UT", UT_HR(parameters), UT_MIN(parameters),
	    UT_SEC(parameters), "UNIVERSAL TIME")
	call cam_hmscard (STDOUT, "ST", ST_HR(parameters), ST_MIN(parameters),
	    ST_SEC(parameters), "SIDEREAL TIME")
	if (AIR_MASS(parameters) != 0) {	
	    value = AIR_MASS(parameters) / 100.
	    call cam_rcard (STDOUT, "AIRMASS", value, "AIR MASS", 3)
	}
			    
	# Observation instrumentation cards
	call cam_detcard (STDOUT, "DETECTOR", CAM_HEAD(parameters),
	    "DETECTOR (CCD TYPE, PHOTON COUNTER, ETC)")
	if (GAIN(parameters) != 0) {
	    value = GAIN(parameters) / 100.
	    call cam_rcard (STDOUT, "GAIN", value, "GAIN (ELECTRONS/ADU)", 2)
	}
	if (RDNOISE(parameters) != 0) {
	    value = RDNOISE(parameters) / 100.
	    call cam_rcard (STDOUT, "RDNOISE", value,
	        "READOUT NOISE (ELECTRONS)", 1)
	}
	if (PREFLASH(parameters) != 0) {
	    call cam_sicard (STDOUT, "PREFLASH", PREFLASH(parameters),
		"PREFLASH TIME (SECONDS)")
	}
	value = CAM_TEMP(parameters) / 100.
	call cam_rcard (STDOUT, "CAMTEMP", value, "CAMERA TEMPERATURE, DEG C",
	    2) 
	value = DEW_TEMP(parameters) / 100.
	call cam_rcard (STDOUT, "DEWTEMP", value, "DEWAR TEMPRATURE, DEG C", 2) 
	if (PFLEVEL(parameters) != 0) {
	    call cam_sicard (STDOUT, "PFLEVEL", PFLEVEL(parameters),
		"PREFLASH LEVEL")
	}
	call cam_2sintcard (STDOUT, "FILTERS", F1POS(parameters), F2POS(parameters),
	    "FILTER BOLT POSITIONS")
	call cam_sicard (STDOUT, "TVFILT", TV_FILTER(parameters), "TV FILTER")
	call cam_sicard (STDOUT, "COMPLAMP", COMP_LAMP(parameters),
	    "COMPARISON LAMP")
	if (TILT_POS(parameters) != 0) {
	    call cam_sicard (STDOUT, "TILTPOS", TILT_POS(parameters),
	        "TILT POSITION")
	}
	if (PED_POS(parameters) != 0) {
	    call cam_sicard (STDOUT, "TELEFOCUS", PED_POS(parameters),
		"TELESCOPE FOCUS")
	}

	# Reduction flags
	if (BIAS_PIX(parameters) != 0) {
	    call cam_sicard (STDOUT, "BIASPIX", BIAS_PIX(parameters), "")
	}
	if (BT_FLAG(parameters) != 0) {
	    call cam_sicard (STDOUT, "OVERSCAN", BT_FLAG(parameters),
	        "OVERSCAN SUBTRACTED")
	    call cam_sicard (STDOUT, "TRIM", short (1), "TRIMMED IMAGE")
	}
	if (BI_FLAG(parameters) != 0) {
	    call cam_sicard (STDOUT, "ZEROCOR", BI_FLAG(parameters),
	        "ZERO LEVEL SUBTRACTED (PREFLASH, BIAS)")
	}
	if (BP_FLAG(parameters) != 0) {
	    call cam_sicard (STDOUT, "FIXPIX", BP_FLAG(parameters),
	        "BAD PIXEL CORRECTION")
	}
	if (CR_FLAG(parameters) != 0) {
	    call cam_sicard (STDOUT, "CRFLAG", CR_FLAG(parameters),
	        "COSMIC RAYS REMOVED")
	}
	if (DK_FLAG(parameters) != 0) {
	    call cam_sicard (STDOUT, "DARKCOR", DK_FLAG(parameters),
	        "DARK SUBTRACTED")
	}
	if (FF_FLAG(parameters) != 0) {
	    call cam_sicard (STDOUT, "FLATCOR", FF_FLAG(parameters),
	        "FLAT FIELD CORRECTION")
	}
	if (FR_FLAG(parameters) != 0) {
	    call cam_sicard (STDOUT, "FRINGCOR", FR_FLAG(parameters),
	        "FRINGING SUBTRACTED")
	}
	if (FR_SC100(parameters) != 0) {
	    call bitpak (int (FR_SC100(parameters)), value, 1, 32)
	    call cam_rcard (STDOUT, "FRINGSCL", value, "FRINGE SCALING", 2)
	}

	call cam_section (STDOUT, parameters)

	if (PIC_XSUM(parameters) != 0 && PIC_YSUM(parameters) != 0) {
	    call cam_2sintcard (STDOUT, "CCDSUM", PIC_XSUM(parameters),
	        PIC_YSUM(parameters), "ON CHIP SUMMATION (X,Y)")
	}
end
