include	<imhdr.h>
include	<error.h>
include	"ccdtypes.h"

define	SZ_CCDLINE	80	# Size of line for output


# T_CCDLIST -- List CCD image information and processing status.
#
# Each input image of the specified image type is listed in either a one
# line short format, a name only format, or a longer format.  The image
# name, size, pixel type, image type, subset ID, processing flags and
# title are printed on one line.  For the long format image details of
# the processing operations are printed.

procedure t_ccdlist ()

int	list, ccdtype
bool	names, lformat
pointer	sp, image, im

bool	clgetb()
int	imtopenp(), imtgetim()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Get the task parameters and open the translation file.
	list = imtopenp ("images")
	names = clgetb ("names")
	lformat = clgetb ("long")
	call clgstr ("instrument", Memc[image], SZ_FNAME)
	call hdmopen (Memc[image])

	# List each iamge.
	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    # Map the image and the instrument header translation.
	    # Check the image type.
	    call set_input (Memc[image], im, ccdtype)
	    if (im == NULL)
		next

	    # Select the output format.
	    if (names) {
		call printf ("%s\n")
		    call pargstr (Memc[image])
	    } else if (lformat) {
	        call shortlist (Memc[image], ccdtype, im)
		call longlist (im, ccdtype)
	    } else
	        call shortlist (Memc[image], ccdtype, im)
	    call flush (STDOUT)

	    call imunmap (im)
	}

	# Finish up.
	call hdmclose ()
	call imtclose (list)
	call sfree (sp)
end


# SHORTLIST -- List the one line short format consisting of the image name,
# iamge size, pixel type, image type, subset ID, processing flags, and
# title.

procedure shortlist (image, ccdtype, im)

char	image			# Image name
int	ccdtype			# CCD image type
pointer	im			# IMIO pointer

bool	ccdflag()
pointer	sp, str, subset

begin
	call smark (sp)
	call salloc (str, SZ_CCDLINE, TY_CHAR)
	call salloc (subset, SZ_CCDLINE, TY_CHAR)

	# Get the image type and subset ID.
	call ccdtypes (im, Memc[str], SZ_CCDLINE)
	call ccdsubset (im, Memc[subset], SZ_CCDLINE)

	# List the image name, size, pixel type, image type, and subset.
	call printf ("%s[%d,%d][%s][%s][%d]")
	    call pargstr (image)
	    call pargi (IM_LEN(im,1))
	    call pargi (IM_LEN(im,2))
	    call pargtype1 (IM_PIXTYPE(im))
	    call pargstr (Memc[str])
	    call pargstr (Memc[subset])

	# Format and list the processing flags.
	Memc[str] = EOS
	if (ccdflag (im, "fixpix"))
	    call strcat ("B", Memc[str], SZ_CCDLINE)
	if (ccdflag (im, "overscan"))
	    call strcat ("O", Memc[str], SZ_CCDLINE)
	if (ccdflag (im, "trim"))
	    call strcat ("T", Memc[str], SZ_CCDLINE)
	if (ccdflag (im, "zerocor"))
	    call strcat ("Z", Memc[str], SZ_CCDLINE)
	if (ccdflag (im, "darkcor"))
	    call strcat ("D", Memc[str], SZ_CCDLINE)
	if (ccdflag (im, "flatcor"))
	    call strcat ("F", Memc[str], SZ_CCDLINE)
	if (ccdflag (im, "illumcor"))
	    call strcat ("I", Memc[str], SZ_CCDLINE)
	if (ccdflag (im, "fringcor"))
	    call strcat ("Q", Memc[str], SZ_CCDLINE)
	if (Memc[str] != EOS) {
	    call printf ("[%s]")
		call pargstr (Memc[str])
	}

	# List the title.
	call printf (":%s\n")
	    call pargstr (IM_TITLE(im))

	call sfree (sp)
end


# LONGLIST -- Add the long format listing.
# List some instrument parameters and information about each processing
# step indicated by the processing parameters.  If the processing step has
# not been done yet indicate this and the parameters to be used.

procedure longlist (im, ccdtype)

pointer	im			# IMIO pointer
int	ccdtype			# CCD image type

real	rval, hdmgetr()
pointer	sp, instr, outstr
bool	clgetb(), ccdflag(), streq()
define	done_	99

begin
	call smark (sp)
	call salloc (instr, SZ_LINE, TY_CHAR)
	call salloc (outstr, SZ_LINE, TY_CHAR)

	# List some image parameters.
	Memc[outstr] = EOS
	ifnoerr (rval = hdmgetr (im, "exptime")) {
	    call sprintf (Memc[instr], SZ_LINE, " exposure=%d")
		call pargr (rval)
	    call strcat (Memc[instr], Memc[outstr], SZ_LINE)
	}
	ifnoerr (rval = hdmgetr (im, "darktime")) {
	    call sprintf (Memc[instr], SZ_LINE, " darktime=%d")
		call pargr (rval)
	    call strcat (Memc[instr], Memc[outstr], SZ_LINE)
	}
	call printf ("   %s\n")
	    call pargstr (Memc[outstr])

	# List the processing strings.
	if (ccdflag (im, "fixpix")) {
	    call hdmgstr (im, "fixpix", Memc[outstr], SZ_LINE)
	    call printf ("    %s\n")
		call pargstr (Memc[outstr])
	} else if (clgetb ("fixpix")) {
	    call clgstr ("fixfile", Memc[outstr], SZ_LINE)
	    if (streq (Memc[outstr], "image"))
		call hdmgstr (im, "fixfile", Memc[outstr], SZ_LINE)
	    if (Memc[outstr] != EOS) {
	        call printf ("    [TO BE DONE] Bad pixel file is %s\n")
		    call pargstr (Memc[outstr])
	    } else
	        call printf (
		    "    [TO BE DONE] Bad pixel file needs to be specified\n")
	}

	if (ccdflag (im, "overscan")) {
	    call hdmgstr (im, "overscan", Memc[outstr], SZ_LINE)
	    call printf ("    %s\n")
		call pargstr (Memc[outstr])
	} else if (clgetb ("overscan")) {
	    call clgstr ("biassec", Memc[outstr], SZ_LINE)
	    if (streq (Memc[outstr], "image"))
		call hdmgstr (im, "biassec", Memc[outstr], SZ_LINE)
	    call printf ("    [TO BE DONE] Overscan strip is %s\n")
		call pargstr (Memc[outstr])
	}

	if (ccdflag (im, "trim")) {
	    call hdmgstr (im, "trim", Memc[outstr], SZ_LINE)
	    call printf ("    %s\n")
		call pargstr (Memc[outstr])
	} else if (clgetb ("trim")) {
	    call clgstr ("trimsec", Memc[outstr], SZ_LINE)
	    if (streq (Memc[outstr], "image"))
		call hdmgstr (im, "trimsec", Memc[outstr], SZ_LINE)
	    call printf ("    [TO BE DONE] Trim image section is %s\n")
		call pargstr (Memc[outstr])
	}

	if (ccdtype == ZERO) {
	    if (ccdflag (im, "readcor")) {
		call hdmgstr (im, "readcor", Memc[outstr], SZ_LINE)
	        call printf ("    %s\n")
		    call pargstr (Memc[outstr])
	    } else if (clgetb ("readcor"))
	        call printf (
		    "    [TO BE DONE] Convert to readout format\n")
	    goto done_
	}
	if (ccdflag (im, "zerocor")) {
	    call hdmgstr (im, "zerocor", Memc[outstr], SZ_LINE)
	    call printf ("    %s\n")
		call pargstr (Memc[outstr])
	} else if (clgetb ("zerocor"))
	    call printf ("    [TO BE DONE] Zero level correction\n")

	if (ccdtype == DARK)
	    goto done_
	if (ccdflag (im, "darkcor")) {
	    call hdmgstr (im, "darkcor", Memc[outstr], SZ_LINE)
	    call printf ("    %s\n")
		call pargstr (Memc[outstr])
	} else if (clgetb ("darkcor"))
	    call printf ("    [TO BE DONE] Dark count correction\n")

	if (ccdtype == FLAT) {
	    if (ccdflag (im, "scancor")) {
		call hdmgstr (im, "scancor", Memc[outstr], SZ_LINE)
	        call printf ("    %s\n")
		    call pargstr (Memc[outstr])
	    } else if (clgetb ("scancor"))
	        call printf (
		    "    [TO BE DONE] Convert to scan format\n")
	    if (ccdflag (im, "skyflat")) {
		call hdmgstr (im, "skyflat", Memc[outstr], SZ_LINE)
	        call printf ("    %s\n")
		    call pargstr (Memc[outstr])
	    }
	    if (ccdflag (im, "illumflt")) {
		call hdmgstr (im, "illumflt", Memc[outstr], SZ_LINE)
	        call printf ("    %s\n")
		    call pargstr (Memc[outstr])
	    }
	    goto done_
	}
	if (ccdflag (im, "flatcor")) {
	    call hdmgstr (im, "flatcor", Memc[outstr], SZ_LINE)
	    call printf ("    %s\n")
		call pargstr (Memc[outstr])
	} else if (clgetb ("flatcor"))
	    call printf ("    [TO BE DONE] Flat field correction\n")

	if (ccdtype == ILLUM) {
	    if (ccdflag (im, "mkillum")) {
		call hdmgstr (im, "mkillum", Memc[outstr], SZ_LINE)
	        call printf ("    %s\n")
		    call pargstr (Memc[outstr])
	    } else
	        call printf (
		    "    [TO BE DONE] Convert to illumination correction\n")
	    goto done_
	}
	if (ccdflag (im, "illumcor")) {
	    call hdmgstr (im, "illumcor", Memc[outstr], SZ_LINE)
	    call printf ("    %s\n")
		call pargstr (Memc[outstr])
	} else if (clgetb ("illumcor"))
	    call printf ("    [TO BE DONE] Illumination correction\n")

	if (ccdtype == FRINGE)
	    goto done_
	if (ccdflag (im, "fringcor")) {
	    call hdmgstr (im, "fringecor", Memc[outstr], SZ_LINE)
	    call printf ("    %s\n")
		call pargstr (Memc[outstr])
	} else if (clgetb ("fringecor"))
	    call printf ("    [TO BE DONE] Fringe correction\n")

done_
	call sfree (sp)
end


# PARGTYPE1 -- Convert an integer type code into a string, and output the
# string with PARGSTR to FMTIO.  Taken from IMHEADER.

procedure pargtype1 (dtype)

int	dtype

begin
	switch (dtype) {
	case TY_UBYTE:
	    call pargstr ("ubyte")
	case TY_BOOL:
	    call pargstr ("bool")
	case TY_CHAR:
	    call pargstr ("char")
	case TY_SHORT:
	    call pargstr ("short")
	case TY_USHORT:
	    call pargstr ("ushort")
	case TY_INT:
	    call pargstr ("int")
	case TY_LONG:
	    call pargstr ("long")
	case TY_REAL:
	    call pargstr ("real")
	case TY_DOUBLE:
	    call pargstr ("double")
	case TY_COMPLEX:
	    call pargstr ("complex")
	case TY_POINTER:
	    call pargstr ("pointer")
	case TY_STRUCT:
	    call pargstr ("struct")
	default:
	    call pargstr ("unknown datatype")
	}
end
