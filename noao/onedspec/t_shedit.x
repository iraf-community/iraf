include	<imhdr.h>
include	<error.h>
include "oned.h"
include "idsmtn.h"

# SHEDIT -- Spectrum Header Parameter Editor 

procedure t_shedit ()

int	root, nrecs
pointer	sp, str, records, ids, im

int	clpopni(), get_next_image(), decode_ranges()
pointer	immap()

begin
	# Allocate memory
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (records, 3*MAX_RANGES, TY_INT)
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)

	# Open input file name template
	root = clpopni ("input")
	call clgstr ("records", Memc[str], SZ_LINE)
	if (decode_ranges (Memc[str], Memi[records], MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Loop over all input images.
	call reset_next_image ()
	while (get_next_image (root, Memi[records], nrecs, Memc[str], SZ_LINE)
	    != EOF) {
	    iferr (im = immap (Memc[str], READ_WRITE, 0)) {
		call erract (EA_WARN)
		next
	    }

	    # Get header parameters, put them in a parameter file, edit the
	    # parameters with EPARAM, get the parameters from the parameter
	    # file, and store them in the image header.
	    call load_ids_hdr (ids, im, 1)
	    call put_params (Memc[str], IM_TITLE(im), ids)
	    call clcmdw ("eparam shparams")
	    call get_params (ids, IM_TITLE(im), SZ_IMTITLE)
	    call store_keywords (ids, im)
	    call imunmap (im)
	}

	# Free space
	call sfree (sp)
	call clpcls (root)

	# Null out record string to avoid learn mode
	call clpstr ("records", "")
end


# GET_PARAMS -- Get header parameters from the parameter file.

procedure get_params (ids, title, maxchars)

pointer	ids			# Header parameters
char	title[maxchars]		# Title
int	maxchars		# Maximum number of chars in title

int	clgeti()
real	clgetr()

begin
	call clgstr ("shparams.title", title, maxchars)
	ITM(ids) = clgetr ("shparams.exposure")
	OFLAG(ids) = clgeti ("shparams.oflag")
	BEAM(ids) = clgeti ("shparams.beam")
	NP1(ids) = clgeti ("shparams.np1")
	NP2(ids) = clgeti ("shparams.np2")
	W0(ids) = clgetr ("shparams.crval1")
	WPC(ids) = clgetr ("shparams.cdelt1")
	AIRMASS(ids) = clgetr ("shparams.airmass")

	UT(ids) = clgetr ("shparams.ut")
	ST(ids) = clgetr ("shparams.st")
	RA(ids) = clgetr ("shparams.ra")
	DEC(ids) = clgetr ("shparams.dec")
	HA(ids) = clgetr ("shparams.ha")

	DF_FLAG(ids) = clgeti ("shparams.df")
	SM_FLAG(ids) = clgeti ("shparams.sm")
	QF_FLAG(ids) = clgeti ("shparams.qf")
	DC_FLAG(ids) = clgeti ("shparams.dc")
	QD_FLAG(ids) = clgeti ("shparams.qd")
	EX_FLAG(ids) = clgeti ("shparams.ex")
	BS_FLAG(ids) = clgeti ("shparams.bs")
	CA_FLAG(ids) = clgeti ("shparams.ca")
	CO_FLAG(ids) = clgeti ("shparams.co")
end


# PUT_PARAMS -- Put header parameters to the parameter file.

procedure put_params (image, title, ids)

char	image[ARB]	# Image name
char	title[ARB]	# Title
pointer	ids		# Header parameters

begin
	call clpstr ("shparams.image", image)
	call clpstr ("shparams.title", title)
	call clputr ("shparams.exposure", ITM(ids))
	call clputi ("shparams.oflag", OFLAG(ids))
	call clputi ("shparams.beam", BEAM(ids))
	call clputi ("shparams.np1", NP1(ids))
	call clputi ("shparams.np2", NP2(ids))
	call clputr ("shparams.crval1", W0(ids))
	call clputr ("shparams.cdelt1", WPC(ids))
	call clputr ("shparams.airmass", AIRMASS(ids))

	call put_hms ("shparams.ut", UT(ids))
	call put_hms ("shparams.st", ST(ids))
	call put_hms ("shparams.ra", RA(ids))
	call put_hms ("shparams.dec", DEC(ids))
	call put_hms ("shparams.ha", HA(ids))

	call clputi ("shparams.df", DF_FLAG(ids))
	call clputi ("shparams.sm", SM_FLAG(ids))
	call clputi ("shparams.qf", QF_FLAG(ids))
	call clputi ("shparams.dc", DC_FLAG(ids))
	call clputi ("shparams.qd", QD_FLAG(ids))
	call clputi ("shparams.ex", EX_FLAG(ids))
	call clputi ("shparams.bs", BS_FLAG(ids))
	call clputi ("shparams.ca", CA_FLAG(ids))
	call clputi ("shparams.co", CO_FLAG(ids))
end


# PUT_SEX -- Put real parameters to parameter file in HMS format.

procedure put_hms (param, value)

char	param		# CL parameter name
real	value		# Value to be put
pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call sprintf (Memc[str], SZ_FNAME, "%.1h")
	    call pargr (value)
	call clpstr (param, Memc[str])
	call sfree (sp)
end
