include <imhdr.h>
include "quadgeom.h"

procedure t_quadjoin ()

char	input[SZ_FNAME]			#TI Input image root name.
char	output[SZ_FNAME]		#TI Output image name.
char	instrument[SZ_FNAME]		#TI Instrument translation file.
bool	delete				#TI delete sub-images when done.

int	namps, amp
pointer	in[QG_MAXAMPS], out, qg
char	logstr[SZ_LINE]
bool	inplace

pointer	immap()
int	quadmap(), imaccess()
bool	streq(), clgetb()

errchk	ccddelete()

begin
	# Open instrument file
	call clgstr    ("instrument",  instrument,  SZ_FNAME)
	call hdmopen   (instrument)

	# Get input image name and output image names.
	call clgstr ("input", input, SZ_FNAME)
	call xt_imroot (input, input, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)

	# If the output name is null the opperation is "done in place one 
	# removed". That is:
	#	the sub-images are combined to form a temporary image
	#	the ORIGINAL PARENT IMAGE is deleted or copied to a backup image
	#       the temporary image is renamed to the original parent image
	#
	if (streq (output, "")) {
	    call mktemp ("tmp", output, SZ_FNAME)
	    inplace = true
	} else {
	    inplace = false
	}

	# Get delete sub-image flag
	delete = clgetb ("delete")

	# Allocate quadgeom structure
	call quadalloc (qg)

	# Open input sub-images
	namps = quadmap (input, READ_ONLY, false, 0, qg, in)

#	call quaddump (qg)

	# Open output image
	out = immap (output, NEW_COPY, in[1])

	# Merge header information to form header for composite image.
#	call quadjoinhdr (in, out, qg)
	call quadjoinhdr2 (in, out, qg)

        switch (IM_PIXTYPE(out)) {
        case TY_USHORT, TY_SHORT:
	    call qjoins (in, out, qg)

        case TY_INT:
	    call qjoini (in, out, qg)

        case TY_LONG:
	    call qjoinl (in, out, qg)

        case TY_REAL:
	    call qjoinr (in, out, qg)

        case TY_DOUBLE:
	    call qjoind (in, out, qg)

        default:
            call error (1, "unsupported pixel datatype")
        }

	# Log opperation
	if (QG_NAMPSX(qg) == 2 && QG_NAMPSY(qg) == 2) {
	    call sprintf (logstr, SZ_LINE, "Quad-readout image")
	} else if (QG_NAMPSX(qg) == 2 || QG_NAMPSY(qg) == 2) {
	    call sprintf (logstr, SZ_LINE, "Dual-readout image: nampsx=%d nampsy=%d")
		call pargi (QG_NAMPSX(qg))
		call pargi (QG_NAMPSY(qg))
	} else {
	    call sprintf (logstr, SZ_LINE, "Single-readout image")
	}
	call timelog (logstr, SZ_LINE)
	call ccdlog (input, logstr)

	# Tidy up
	call imunmap (out)
	do amp = 1, namps
	    call imunmap (in[amp])

	# Delete sub-images
	if (delete)
	    call quaddelete (qg, input)

	if (inplace) {
  	    # Replace the input by the output image.
	    if (imaccess (input, READ_ONLY) == YES) {
		iferr (call ccddelete (input)) {
		    call imdelete (output)
		    call error (0, "Can't delete or make backup of original image")
		}
	    }
	    call imrename (output, input)
	}

	call quadfree (qg)
	call hdmclose ()
end

# Merge header information and write to header of output image.

procedure quadjoinhdr (in, out, qg)

pointer	in[ARB]		#I Pointer to input sub-images.
pointer	out		#I Pointer to output image.
pointer	qg		#I Pointer to quadgeom structure.

char	keyword[SZ_LINE], section[SZ_LINE], buffer[SZ_LINE]
real	rval, ccdmean
int	amp, brk

int	hdmaccf(), strsearch()
real	hdmgetr()

begin
	# Set image dimensions
	IM_LEN (out, 1) = QG_NX(qg, 0)
	IM_LEN (out, 2) = QG_NY(qg, 0)

	# Add defined sections to output image header.
	if ((QG_DX1 (qg, 0) != 0) && (hdmaccf (out, "trim") == NO)) {
	    call sprintf (section, SZ_LINE, "[%d:%d,%d:%d]")
		call pargi (QG_DX1(qg, 0))
		call pargi (QG_DX2(qg, 0))
		call pargi (QG_DY1(qg, 0))
		call pargi (QG_DY2(qg, 0))
	    call hdmpstr (out, "datasec", section)

	    call sprintf (section, SZ_LINE, "[%d:%d,%d:%d]")
		call pargi (QG_TX1(qg, 0))
		call pargi (QG_TX2(qg, 0))
		call pargi (QG_TY1(qg, 0))
		call pargi (QG_TY2(qg, 0))
	    call hdmpstr (out, "trimsec", section)

	    call sprintf (section, SZ_LINE, "[%d:%d,%d:%d]")
		call pargi (QG_BX1(qg, 0))
		call pargi (QG_BX2(qg, 0))
		call pargi (QG_BY1(qg, 0))
		call pargi (QG_BY2(qg, 0))
	    call hdmpstr (out, "biassec", section)
	}

	if (QG_CX1 (qg, 0) != 0) {
	    call sprintf (section, SZ_LINE, "[%d:%d,%d:%d]")
		call pargi (QG_CX1(qg, 0))
		call pargi (QG_CX2(qg, 0))
		call pargi (QG_CY1(qg, 0))
		call pargi (QG_CY2(qg, 0))
	    call hdmpstr (out, "ccdsec", section)
	}

	# Set AMPSECnm 
	do amp = 1, QG_NAMPS(qg) {
	    call sprintf (keyword, SZ_LINE, "ampsec%s")
		call pargstr (Memc[QG_AMPID(qg, amp)])

	    call sprintf (section, SZ_LINE, "[%d:%d,%d:%d]")
		call pargi (QG_AX1(qg, amp))
		call pargi (QG_AX2(qg, amp))
		call pargi (QG_AY1(qg, amp))
		call pargi (QG_AY2(qg, amp))

	    call hdmpstr (out, keyword, section)
	}

	# Tidy up processing history
	if (hdmaccf (out, "trim") == YES) {
	    do amp = 1, QG_NAMPS(qg) 
		call mergehist (in[amp], out, "trim", Memc[QG_AMPID(qg, amp)])
	    call hdmdelf (out, "trim")
	    call strcpy ("Trimmed", buffer, SZ_LINE)
	    call timelog (buffer, SZ_LINE)
	    call hdmpstr (out, "trim", buffer)
	}

	if (hdmaccf (out, "overscan") == YES) {
	    do amp = 1, QG_NAMPS(qg) 
		call mergehist (in[amp], out, "overscan", Memc[QG_AMPID(qg, amp)])
	    call hdmdelf (out, "overscan")
	    call strcpy ("Overscan corrected", buffer, SZ_LINE)
	    call timelog (buffer, SZ_LINE)
	    call hdmpstr (out, "overscan", buffer)
	}

	if (hdmaccf (out, "ccdmean") == YES) {
	    ccdmean = 0.0
	    do amp = 1, QG_NAMPS(qg) {
		rval = hdmgetr (in[amp], "ccdmean")
		ccdmean = ccdmean + rval
		call sprintf (keyword, SZ_LINE, "ccdmea%s")
		    call pargstr (Memc[QG_AMPID(qg, amp)])
		call hdmputr (out, keyword, rval)
	    }
	    ccdmean = ccdmean / QG_NAMPS(qg)
	    call hdmdelf (out, "ccdmean")
	    call hdmputr (out, "ccdmean", ccdmean)
	}

	# Move CCDPROC keyword to end of header
	if (hdmaccf (out, "ccdproc") == YES) {
	    call hdmgstr (in, "ccdproc", buffer, SZ_LINE)
	    call hdmdelf (out, "ccdproc")
	    brk = strsearch (buffer, "CCD")
	    if (brk !=0)
		call strcpy (buffer[brk-3], buffer, SZ_LINE)
	    call timelog (buffer, SZ_LINE)
	    call hdmpstr (out, "ccdproc", buffer)
	}
end

define	SZ_KEYWRD	8		# Number of chars in FITS keyword

define	REVSTRING	"1.000	09Mar94 (Included amplifier geometry keywords)"

# Merge header information and write to header of output image.

procedure quadjoinhdr2 (in, out, qg)

pointer	in[ARB]		#I Pointer to input sub-images.
pointer	out		#I Pointer to output image.
pointer	qg		#I Pointer to quadgeom structure.

pointer	sp, keyword, section, buffer
real	rval, ccdmean
int	amp, brk, ch

int	ax1, ax2, ay1, ay2
int	bx1, bx2, by1, by2
int	dx1, dx2, dy1, dy2
int	tx1, tx2, ty1, ty2

int	hdmaccf(), strsearch()
real	hdmgetr()

begin
	call smark (sp)
	call salloc (keyword, SZ_KEYWRD, TY_CHAR)
	call salloc (section, SZ_LINE,   TY_CHAR)
	call salloc (buffer,  SZ_LINE,   TY_CHAR)

	# Set image dimensions
	IM_LEN (out, 1) = QG_NX(qg, 0)
	IM_LEN (out, 2) = QG_NY(qg, 0)

	# Set the header revision level if not already set.
	if (hdmaccf (out, "HDR_REV") == NO) {
	    call hdmpstr (out, "HDR_REV", REVSTRING)
	}

	# Update nampsyx and amplist
	call sprintf (Memc[buffer], SZ_LINE, "%d %d")
	    call pargi (QG_NAMPSY(qg))
	    call pargi (QG_NAMPSX(qg))
	call hdmpstr (out, "nampsyx", Memc[buffer])

	ch = 1
	do amp = 1, QG_NAMPS(qg) {
	    call sprintf (Memc[buffer+ch-1], 3, "%2s ")
		call pargstr (Memc[QG_AMPID(qg, amp)])
	    ch = ch + 3
	}
	call hdmpstr (out, "amplist", Memc[buffer])

	# Update geometry keywords for each amplifier in the header.
	# If the corresponding section is undefined any old keywords are deleted
	# The TSECyx, DSECyx and BSECyx keywords are only retained if the image
	# has not been trimmed.
	do amp = 1, QG_NAMPS (qg) {

	    # Ampsec (ASECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_LINE, "ASEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])

	    ax1 = QG_AX1 (qg, amp)
	    ax2 = QG_AX2 (qg, amp)
	    ay1 = QG_AY1 (qg, amp)
	    ay2 = QG_AY2 (qg, amp)

	    call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
		call pargi (ax1)
		call pargi (ax2)
		call pargi (ay1)
		call pargi (ay2)

	    call hdmpstr (out, Memc[keyword], Memc[section])

	    # Biassec (BSECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_LINE, "BSEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])

	    if ((hdmaccf (out, "trim") == NO) && (QG_BX1 (qg, amp) != 0)) {


		bx1 = QG_BX1 (qg, amp) + ax1 - 1
		bx2 = QG_BX2 (qg, amp) + ax1 - 1
		by1 = QG_BY1 (qg, amp) + ay1 - 1
		by2 = QG_BY2 (qg, amp) + ay1 - 1

		call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
		    call pargi (bx1)
		    call pargi (bx2)
		    call pargi (by1)
		    call pargi (by2)

		call hdmpstr (out, Memc[keyword], Memc[section])

	    } else if (hdmaccf (out, Memc[keyword]) == YES) {

		call hdmdelf (out, Memc[keyword])

	    }

	    # CCDsec (CSECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_LINE, "CSEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])

	    if ((hdmaccf (out, "trim") == NO) && (QG_CX1 (qg, amp) != 0)) {

		call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
		    call pargi (QG_CX1(qg, amp))
		    call pargi (QG_CX2(qg, amp))
		    call pargi (QG_CY1(qg, amp))
		    call pargi (QG_CY2(qg, amp))

		call hdmpstr (out, Memc[keyword], Memc[section])

	    } else if (hdmaccf (out, Memc[keyword]) == YES) {

		call hdmdelf (out, Memc[keyword])

	    }

	    # Datasec (DSECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_LINE, "DSEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])

	    if ((hdmaccf (out, "trim") == NO) && (QG_DX1 (qg, amp) != 0)) {

		dx1 = QG_DX1 (qg, amp) + ax1 - 1
		dx2 = QG_DX2 (qg, amp) + ax1 - 1
		dy1 = QG_DY1 (qg, amp) + ay1 - 1
		dy2 = QG_DY2 (qg, amp) + ay1 - 1

		call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
		    call pargi (dx1)
		    call pargi (dx2)
		    call pargi (dy1)
		    call pargi (dy2)
		call hdmpstr (out, Memc[keyword], Memc[section])

	    } else if (hdmaccf (out, Memc[keyword]) == YES) {

		call hdmdelf (out, Memc[keyword])

	    }

	    # Trimsec (TSECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_LINE, "TSEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])

	    if ((hdmaccf (out, "trim") == NO) && (QG_TX1 (qg, amp) != 0)) {


		tx1 = QG_TX1 (qg, amp) + ax1 - 1
		tx2 = QG_TX2 (qg, amp) + ax1 - 1
		ty1 = QG_TY1 (qg, amp) + ay1 - 1
		ty2 = QG_TY2 (qg, amp) + ay1 - 1

		call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
		    call pargi (tx1)
		    call pargi (tx2)
		    call pargi (ty1)
		    call pargi (ty2)
		call hdmpstr (out, Memc[keyword], Memc[section])

	    } else if (hdmaccf (out, Memc[keyword]) == YES) {

		call hdmdelf (out, Memc[keyword])

	    }

	}

	# Delete biassec, ccdsec, datasec and trimsec if present.
	if (hdmaccf (out, "biassec") == YES) {
	    call hdmdelf (out, "biassec")
	}

	if (hdmaccf (out, "datasec") == YES) {
	    call hdmdelf (out, "datasec")
	}

	if (hdmaccf (out, "trimsec") == YES) {
	    call hdmdelf (out, "trimsec")
	}

	if (hdmaccf (out, "ccdsec") == YES) {
	    call hdmdelf (out, "ccdsec")
	}

	# If image has been trimmed insert CCDSEC for entire image. This is 
	# derived from the CCDSEC's for the sub-images in the BLH and TRH
	# corners.
	if (hdmaccf (out, "trim") == YES) {
	    call sprintf (Memc[section], SZ_LINE, "[%d:%d,%d:%d]")
		call pargi (QG_CX1(qg, 1))
		call pargi (QG_CX2(qg, QG_NAMPS(qg)))
		call pargi (QG_CY1(qg, 1))
		call pargi (QG_CY2(qg, QG_NAMPS(qg)))
 
	    call hdmpstr (out, "CCDSEC", Memc[section])
	}

	# Tidy up processing history as appropriate

	# Overscan Subtraction
	if (hdmaccf (out, "overscan") == YES) {
	    do amp = 1, QG_NAMPS(qg) 
		call merge_overscan (in[amp], out, Memc[QG_AMPID(qg, amp)])
	    
	    call hdmdelf (out, "overscan")
	    call strcpy ("Overscan corrected", Memc[buffer], SZ_LINE)
	    call timelog (Memc[buffer], SZ_LINE)
	    call hdmpstr (out, "overscan", Memc[buffer])
	}

	# Triming.
	if (hdmaccf (out, "trim") == YES) {

	    do amp = 1, QG_NAMPS(qg) 
		call merge_trim (in[amp], out, Memc[QG_AMPID(qg, amp)])

	    call hdmdelf (out, "trim")
	    call strcpy ("Trimmed", Memc[buffer], SZ_LINE)
	    call timelog (Memc[buffer], SZ_LINE)
	    call hdmpstr (out, "trim", Memc[buffer])

	}

	# CCDMEAN
	if (hdmaccf (out, "ccdmean") == YES) {
	    ccdmean = 0.0
	    do amp = 1, QG_NAMPS(qg) {
		rval = hdmgetr (in[amp], "ccdmean")
		ccdmean = ccdmean + rval
		call sprintf (Memc[keyword], SZ_LINE, "ccdmea%s")
		    call pargstr (Memc[QG_AMPID(qg, amp)])
		call hdmputr (out, Memc[keyword], rval)
	    }
	    ccdmean = ccdmean / QG_NAMPS(qg)
	    call hdmdelf (out, "ccdmean")
	    call hdmputr (out, "ccdmean", ccdmean)
	}

	# Move CCDPROC keyword to end of header
	if (hdmaccf (out, "ccdproc") == YES) {
	    call hdmgstr (in, "ccdproc", Memc[buffer], SZ_LINE)
	    call hdmdelf (out, "ccdproc")
	    brk = strsearch (Memc[buffer], "CCD")
	    if (brk !=0)
		call strcpy (Memc[buffer+brk-4], Memc[buffer], SZ_LINE)
	    call timelog (Memc[buffer], SZ_LINE)
	    call hdmpstr (out, "ccdproc", Memc[buffer])
	}

	call sfree (sp)
end

define	OVSC_FMT1	"Overscan section is %s with mean=%g"
define	OVSC_FMT2	"Overscan section is %s"
define	OVSC_FMT3	"Overscan section is %s with function=%s" 

procedure merge_overscan (in, out, ampid)

pointer	in		# Input quadrant image
pointer	out		# Output image
char	ampid[2]	# Label for readout

pointer	sp, buffer, amplifier, biassec, func, rootname, fullname
real	mean
int	idx

int	hdmaccf(), stridx(), nscan()


begin
	call smark (sp)
	call salloc (buffer,    SZ_LINE,   TY_CHAR)
	call salloc (amplifier, SZ_LINE,   TY_CHAR)
	call salloc (biassec,   SZ_LINE,   TY_CHAR)
	call salloc (func,      SZ_LINE,   TY_CHAR)
	call salloc (rootname,  SZ_KEYWRD, TY_CHAR)
	call salloc (fullname,  SZ_KEYWRD, TY_CHAR)

	if (hdmaccf (out, "overscan") == YES) {

	    # Get BSECyx 
	    call sprintf (Memc[fullname], SZ_LINE, "BSEC%2s")
		call pargstr (ampid)
	    call hdmgstr (in, Memc[fullname], Memc[biassec], SZ_LINE)

	    # Get overscan flag and retrieve the mean value if present
	    call hdmgstr (in, "overscan", Memc[buffer], SZ_LINE)
	    idx = stridx ("=", Memc[buffer])
	    if (idx == 0) {
		call sprintf (Memc[buffer], SZ_LINE, OVSC_FMT2)
		    call pargstr (Memc[biassec])

	    } else {
		call sscan (Memc[buffer+idx])
		    call gargr (mean)
	        if (nscan() == 1) {
		    call sprintf (Memc[buffer], SZ_LINE, OVSC_FMT1)
			call pargstr (Memc[biassec])
			call pargr (mean)
		} else {
		    call strcpy (Memc[buffer+idx], Memc[func], SZ_LINE)
		    call sprintf (Memc[buffer], SZ_LINE, OVSC_FMT3)
			call pargstr (Memc[biassec])
			call pargstr (Memc[func])
		}
	    }

	    # Get overscan keyword name and append AMP_ID
	    call hdmname ("overscan", Memc[rootname], 6)
	    call strcpy (Memc[rootname], Memc[fullname], 6)
	    call strcat (ampid, Memc[fullname], SZ_KEYWRD)

	    # Write new overscan keyword.
	    call timelog (Memc[buffer], SZ_LINE)
	    call hdmpstr (out, Memc[fullname], Memc[buffer])
	
	    # And record opperation in logfile
	    call sprintf (Memc[amplifier], SZ_LINE, "    AMP%s")
		call pargstr (ampid)
	    call ccdlog (Memc[amplifier], Memc[buffer])

	}

	call sfree (sp)
end

define	TRIM_FMT	"Trim data section is %s"

procedure merge_trim (in, out, ampid)

pointer	in		# Input quadrant image
pointer	out		# Output image
char	ampid[2]	# Label for readout

pointer	sp, buffer, amplifier, trimsec, rootname, fullname

int	hdmaccf()


begin
	call smark (sp)
	call salloc (buffer,   SZ_LINE,   TY_CHAR)
	call salloc (amplifier, SZ_LINE,   TY_CHAR)
	call salloc (trimsec,  SZ_LINE,   TY_CHAR)
	call salloc (rootname, SZ_KEYWRD, TY_CHAR)
	call salloc (fullname, SZ_KEYWRD, TY_CHAR)

	if (hdmaccf (out, "trim") == YES) {

	    # Get BSECyx 
	    call sprintf (Memc[fullname], SZ_LINE, "TSEC%2s")
		call pargstr (ampid)
	    call hdmgstr (in, Memc[fullname], Memc[trimsec], SZ_LINE)

	    call sprintf (Memc[buffer], SZ_LINE, TRIM_FMT)
		    call pargstr (Memc[trimsec])

	    # Get overscan keyword name and append AMP_ID
	    call hdmname ("trim", Memc[rootname], 6)
	    call strcpy (Memc[rootname], Memc[fullname], 6)
	    call strcat (ampid, Memc[fullname], SZ_KEYWRD)

	    # Write new overscan keyword.
	    call timelog (Memc[buffer], SZ_LINE)
	    call hdmpstr (out, Memc[fullname], Memc[buffer])

	    # And record opperation in logfile
	    call sprintf (Memc[amplifier], SZ_LINE, "    AMP%s")
		call pargstr (ampid)
	    call ccdlog (Memc[amplifier], Memc[buffer])
	}

	call sfree (sp)
end

procedure mergehist (in, out, keyword, ampid)

pointer	in		# Input quadrant image
pointer	out		# Output image
char	keyword[ARB]	# Header keyword to modify
char	ampid[2]	# Label for readout

char	rootname[6], fullname[8]
char	buffer[SZ_LINE]

int	hdmaccf()

begin
	if (hdmaccf (out, keyword) == YES) {
	    call hdmgstr (in, keyword, buffer, SZ_LINE)
	    call hdmname (keyword, rootname, 6)
	    call strcpy (rootname, fullname, 6)
	    call strcat (ampid, fullname, 8)
	    call hdmpstr (out, fullname, buffer)
	}
end
