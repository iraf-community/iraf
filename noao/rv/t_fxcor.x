include	<ctype.h>
include <gset.h>
include <imhdr.h>
include <error.h>
include "rvcomdef.h"
include "rvpackage.h"
include "rvflags.h"
include "rvsample.h"

# T_FXCOR -  Task entry point code.  Initial procedure just handles the
# aquisition of the images and passes the pointers on to other work routines.

procedure t_fxcor()

pointer	rv 					# RV struct pointer
pointer	sp, device, root			# stack storage
pointer	infile, rinfile				# image list pointers

pointer rv_open(), imtopenp()
int	rv_imio(), rv_clpars()
int	imtlen()
bool 	interactive, clgetb()
errchk	rv_open, rv_clpars, rv_imio

define	error_		99

begin
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)

	# Get file names of the spectra and open the package structure.
	call clgstr ("output", Memc[root], SZ_FNAME)
	call clgstr ("graphics", Memc[device], SZ_FNAME)
	interactive = clgetb ("interactive")
	rv = rv_open (Memc[root], Memc[device], interactive)

	# Do some CLIO to get some more parameters.
	infile = imtopenp ("objects")
	rinfile = imtopenp ("templates")

	RV_OBJECTS(rv) = infile			# various initializations
	RV_TEMPLATES(rv) = rinfile
	RV_NOBJS(rv) = imtlen (infile)
	RV_NTEMPS(rv) = imtlen (rinfile)
	RV_RECORD(rv) = 0
	RV_TEMPNUM(rv) = 1
	RV_IMNUM(rv) = 1
	if (rv_clpars(rv) == ERR_READ)
	    goto error_

	# Read the images and let's get started
	RV_TEMPNUM(rv) = 1
	RV_OAPNUM(rv) = RV_APNUM(rv)
	RV_RAPNUM(rv) = RV_APNUM(rv)
	if (rv_imio(rv,infile,rinfile) == ERR_READ)
	    goto error_

	# Open the graphics and gtools pointers
	if (RV_GP(rv) == NULL)
	    call init_gp (rv, interactive, DEVICE(rv))

	if (RV_INTERACTIVE(rv) == YES) {
	    RV_GTYPE(rv) = CORRELATION_PLOT
	    call rv_cursor (rv, infile, rinfile)
	} else 
	    call rv_batch (rv, infile, rinfile)

error_	call imtclose (RV_OBJECTS(rv))			# close list pointers
	call imtclose (RV_TEMPLATES(rv))
	call rv_close (rv)				# free the structure
	call flush (STDOUT)
	call sfree (sp)
end


# RV_CLPARS - Get the parameters from the par file.

int procedure rv_clpars (rv)

pointer	rv				# RV struct pointer

pointer	sp, func, ap, cont, rb, filt, vb, ccf, obs
int	code
bool	clgetb()
real	clgetr(), obsgetr()
pointer	obsopen()
int	rv_apnum_range(), rv_mask_regions(), rv_chk_filter(), cod_verbose()
int	cod_fitfunc(), cod_which(), btoi(), cod_ccftype(), cod_rebin()

errchk  obsopen, rv_apnum_range, obsgetr

begin
	call smark (sp)
	call salloc (ccf, SZ_FNAME, TY_CHAR)
	call salloc (cont, SZ_FNAME, TY_CHAR)
	call salloc (rb, SZ_FNAME, TY_CHAR)
	call salloc (filt, SZ_FNAME, TY_CHAR)
	call salloc (func, SZ_FNAME, TY_CHAR)
	call salloc (ap, SZ_FNAME, TY_CHAR)
	call salloc (vb, SZ_FNAME, TY_CHAR)
	call salloc (obs, SZ_FNAME, TY_CHAR)

	code = OK
	RV_APODIZE(rv) = clgetr ("apodize")
	RV_AUTOWRITE(rv) = btoi (clgetb("autowrite"))
	RV_AUTODRAW(rv) = btoi (clgetb("autodraw"))
	RV_BACKGROUND(rv) = clgetr ("background")
	RV_FITHGHT(rv) = clgetr ("height")
	RV_FITWIDTH(rv) = clgetr ("width")
	RV_IMUPDATE(rv) = btoi (clgetb ("imupdate"))
	RV_MINWIDTH(rv) = clgetr ("minwidth")
	RV_MAXWIDTH(rv) = clgetr ("maxwidth")
	RV_PEAK(rv) = btoi (clgetb("peak"))
	RV_PIXCORR(rv) = btoi (clgetb("pixcorr"))
	RV_WEIGHTS(rv) = clgetr ("weights")
	RV_WINPAR(rv) = clgetr ("window")
	RV_WINCENPAR(rv) = clgetr ("wincenter")

	call clgstr ("ccftype", Memc[ccf], SZ_FNAME)
	RV_CCFTYPE(rv) = cod_ccftype (Memc[ccf])

	call clgstr ("continuum", Memc[cont], SZ_FNAME)
	RV_CONTINUUM(rv) = cod_which (Memc[cont])

	call clgstr ("rebin", Memc[rb], SZ_FNAME)
	RV_REBIN(rv) = cod_rebin (Memc[rb])

	call clgstr ("filter", Memc[filt], SZ_FNAME)
	RV_FILTER(rv) = cod_which (Memc[filt])
	if (RV_FILTER(rv) == BOTH || RV_FILTER(rv) == OBJ_ONLY) {
	    if (rv_chk_filter(rv,OBJECT_SPECTRUM) != OK)
		call error (0, "Invalid filter specification.")
	} else if (RV_FILTER(rv) == BOTH || RV_FILTER(rv) == TEMP_ONLY) {
	    if (rv_chk_filter(rv,REFER_SPECTRUM) != OK)
		call error (0, "Invalid filter specification.")
	}

	call clgstr ("function", Memc[func], SZ_FNAME)
	RV_FITFUNC(rv) = cod_fitfunc (Memc[func])

	call clgstr ("apertures", Memc[ap], SZ_FNAME)
	code = rv_apnum_range (rv, Memc[ap])

	# Get the regions to mask (if any)
	if (rv_mask_regions(rv) == ERR)
	    call rv_errmsg ("Error getting masking regions.")

	# Open observatory database and get the observatory parameters
        iferr {
            call clgstr ("observatory", Memc[obs], SZ_FNAME)
            RV_OBSPTR(rv) = obsopen (Memc[obs])
        } then
            call error (0, "Error opening `observatory' database.")
	iferr {
            RV_ALTITUDE(rv) = obsgetr (RV_OBSPTR(rv), "altitude")
            RV_LATITUDE(rv) = obsgetr (RV_OBSPTR(rv), "latitude")
            RV_LONGITUDE(rv) = obsgetr (RV_OBSPTR(rv), "longitude")
        } then
            call error (0, "Error getting observatory parameters.")

	# Now get the filter pset information
	call filt_get_pars (rv)

	# Parse the verbose parameter
	call clgstr ("verbose", Memc[vb], SZ_FNAME)
	RV_VERBOSE(rv) = cod_verbose (Memc[vb])
	if (RV_VERBOSE(rv) == ERR) {
	    call error (0, 
		"`verbose' must be one of `short|long|nogki|nolog|txtonly'")
	}

	# Check for a debug session
	if (RV_APODIZE(rv) == 0.116)
	    call op_debug (rv)

	call sfree (sp)
	return (code)
end


# RV_MASK_REGIONS - Decode the sample regions string.

int procedure rv_mask_regions (rv)

pointer	rv					#I RV struct pointer

pointer	sp, buf1, buf2

int	rv_load_sample()
bool	streq()

errchk	samp_open

begin	
	call smark (sp)
	call salloc (buf1, SZ_LINE, TY_CHAR); call aclrs (Memc[buf1], SZ_LINE)
	call salloc (buf2, SZ_LINE, TY_CHAR); call aclrs (Memc[buf2], SZ_LINE)

	call clgstr("osample", Memc[buf1], SZ_LINE)
	call clgstr("rsample", Memc[buf2], SZ_LINE)

	# Parse the object parameter.  We also allocate the sample
	# structure here since this routine isn't called anywhere else.
	iferr (call samp_open (RV_OSAMPLE(rv)))
	    call error (0, "Error allocating object sample structure.")
	SR_IMTYPE(RV_OSAMPLE(rv)) = OBJECT_SPECTRUM
	SR_MODIFY(RV_OSAMPLE(rv)) = NO
	SR_PARENT(RV_OSAMPLE(rv)) = rv
	SR_COUNT(RV_OSAMPLE(rv)) = 0
        if (streq(Memc[buf1],"") || streq(Memc[buf1]," ")) {
            call error (0, "`osample' parameter specified as a NULL string") 
	} else if (streq(Memc[buf1], "*")) {
	    ORCOUNT(rv) = ALL_SPECTRUM
	} else {
	    if (rv_load_sample(RV_OSAMPLE(rv), Memc[buf1]) == ERR) {
		call sfree (sp)
	    	return (ERR)
	    }
	}

	# Parse the template sample parameter.
	iferr (call samp_open (RV_RSAMPLE(rv)))
	    call error (0, "Error allocating template sample structure.")
	SR_IMTYPE(RV_RSAMPLE(rv)) = REFER_SPECTRUM
	SR_MODIFY(RV_RSAMPLE(rv)) = NO
	SR_PARENT(RV_RSAMPLE(rv)) = rv
	SR_COUNT(RV_RSAMPLE(rv)) = 0
        if (streq(Memc[buf2],"") || streq(Memc[buf2]," ")) {
            call error (0, "`rsample' parameter specified as a NULL string") 
	} else if (streq(Memc[buf2], "*")) {
	    RRCOUNT(rv) = ALL_SPECTRUM
	} else {
	    if (rv_load_sample(RV_RSAMPLE(rv), Memc[buf2]) == ERR) {
		call sfree (sp)
	    	return (ERR)
	    }
	}

	call sfree (sp)
	return (OK)
end


# RV_IMIO - Do the initial image io for the RVXCOR task.  Process all of
# the template spectra and read the first object spectrum. 

int procedure rv_imio (rv, infile, rinfile)

pointer	rv				#I RV struct pointer
pointer infile				#I Object list input file
pointer	rinfile				#I Template list input file

pointer	sp, fname
int	rv_getim(), imtgetim()
int	read_template_list()
errchk	rv_getim, read_template_list

define	error_ 		99

begin

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Get the first input image.
	if (infile != NULL) {
	    if (imtgetim (infile,Memc[fname],SZ_FNAME) == EOF) {
	        call rv_errmsg ("No input object images in list.")
	        goto error_
	    }
	}

	# Now read in the first OBJECT spectrum.
	if (rv_getim (rv, Memc[fname], OBJECT_SPECTRUM, INDEF, INDEF, 
	    INDEFI) == ERR_READ)
	        goto error_

	# Read in the template data.
	# First get all of the template spectra.
	if (read_template_list (rv, rinfile) == ERR_READ)
	    goto error_

	call sfree (sp)
	return (OK)

error_	call sfree (sp)
	return (ERR_READ)
end
