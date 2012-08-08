include <imhdr.h>
include "rvpackage.h"
include "rvflags.h"
include "rvkeywords.h"

# RVIMUTIL.X  - Image utility routines for operations on image list pointers.
# Procedures to position pointers by reading next, previous, or random
# images. Also included are miscellaneous routines for getting image info
# from just a name.


# NEXT_SPEC - Get the next spectrum in the input file list.  Updates the
# image number in the struct and returns an error code if something went wrong.

int procedure next_spec (rv, infile, written)

pointer	rv					#I RV struct pointer
pointer infile					#I File list pointer
bool	written					#U Have results been written?

char	imname[SZ_FNAME]
#int	get_spec(), rv_verify_aps(), imtrgetim(), imtlen()
int	get_spec(), rv_apnum_range(), imtrgetim(), imtlen()
real	clgetr()

begin
        if (RV_IMNUM(rv)+1 <= imtlen(infile)) {
	    RV_IMNUM(rv) = RV_IMNUM(rv) + 1
	    #if (rv_verify_aps(rv,APPARAM(rv),APLIST(rv,1),NUMAPS(rv)) != OK)
	    if (rv_apnum_range(rv,APPARAM(rv)) != OK)
		return (ERR_READ)
	    if (imtrgetim(infile,RV_IMNUM(rv),imname,SZ_FNAME) != EOF) {
		if (get_spec(rv, imname, OBJECT_SPECTRUM) == ERR_READ)
		    return (ERR_READ)
	        written = false
	
		# Now update the data cache flags
		RV_FITDONE(rv) = NO
		RV_NEWXCOR(rv) = YES
		IS_DBLSTAR(rv) = NO
	 	RV_Y1(rv) = INDEF
		RV_Y2(rv) = INDEF
		RV_WINCENPAR(rv) = clgetr ("wincenter")
		call amovkr (0.0, COEFF(rv,1), 4)

	    } else 
	       call rv_errmsg ("Error reading next image name from list.")
        } else {
	    call rv_errmsg ("At end of input list.")
	    RV_NEWXCOR(rv) = NO
	}

	return (OK)
end


# PREV_SPEC - Get the previous spectrum in the input file list.  Updates the
# image number in the struct and returns an error code if something went wrong.

int procedure prev_spec (rv, infile, written)

pointer	rv					#I RV struct pointer
pointer infile					#I File list pointer
bool	written					#U Have results been written?

char	imname[SZ_FNAME]
int	get_spec(), imtrgetim(), rv_apnum_range()
real	clgetr()

begin
        if (RV_IMNUM(rv)-1 >= 1) {
	    RV_IMNUM(rv) = RV_IMNUM(rv) - 1
	    if (rv_apnum_range(rv,APPARAM(rv)) != OK)
		return (ERR_READ)
	    if (imtrgetim(infile,RV_IMNUM(rv),imname,SZ_FNAME) != EOF) {
		if (get_spec(rv, imname, OBJECT_SPECTRUM) == ERR_READ)
		    return (ERR_READ)
	        written = false

		# Now update the data cache flags
		RV_FITDONE(rv) = NO
		RV_NEWXCOR(rv) = YES
		IS_DBLSTAR(rv) = NO
	 	RV_Y1(rv) = INDEF
		RV_Y2(rv) = INDEF
		RV_WINCENPAR(rv) = clgetr ("wincenter")
		call amovkr (0.0, COEFF(rv,1), 4)

	    } else 
	       call rv_errmsg ("Error reading previous image from list.")
        } else {
	    call rv_errmsg ("At beginning of input list.")
	    RV_NEWXCOR(rv) = NO
	}

	return (OK)
end


# NEXT_TEMP - Get the next spectrum in the template file list.  Updates the
# template number in the struct and returns an error code if something went 
# wrong.

int procedure next_temp (rv, rinfile, written)

pointer	rv					#I RV struct pointer
pointer	rinfile					#I Template list pointer
bool	written					#U Have results been written?

int	imtrgetim(), get_spec(), rv_verify_aps()
real	clgetr()

begin
        if (RV_TEMPNUM(rv)+1 <= RV_NTEMPS(rv)) {
	   RV_TEMPNUM(rv) = RV_TEMPNUM(rv) + 1
	    if (rv_verify_aps(rv,APPARAM(rv),APLIST(rv,1),NUMAPS(rv)) != OK)
		return (ERR_READ)
	    if (imtrgetim(rinfile,RV_TEMPNUM(rv),RIMAGE(rv),SZ_FNAME) != EOF){
		if (get_spec(rv,RIMAGE(rv),REFER_SPECTRUM) == ERR_READ)
		    call error (0,"Error reading next template.")
		call rv_imtitle (RIMAGE(rv), TEMPNAME(rv), SZ_FNAME)
	        written = false
		RV_TEMPCODE(rv) = TEMPCODE(rv,RV_TEMPNUM(rv))
		call amovkr (0.0, COEFF(rv,1), 4)
		RV_FITDONE(rv) = NO
		RV_NEWXCOR(rv) = YES
		IS_DBLSTAR(rv) = NO
	 	RV_Y1(rv) = INDEF
		RV_Y2(rv) = INDEF
		RV_WINCENPAR(rv) = clgetr ("wincenter")

	   } else 
	       call rv_errmsg ("Error reading next image name from list.")
        } else { 
	    call rv_errmsg ("At end of template list.")
	    RV_NEWXCOR(rv) = NO
	}

	return (OK)
end


# PREV_TEMP - Get the previous spectrum in the template file list.  Updates the
# template number in the struct and returns an error code if something went 
# wrong.

int procedure prev_temp (rv, rinfile, written)

pointer	rv					#I RV struct pointer
pointer rinfile					#I File list pointer
bool	written					#U Have results been written?

int	imtrgetim(), get_spec(), rv_verify_aps()
real	clgetr()

begin
        if (RV_TEMPNUM(rv)-1 >= 1) {
	   RV_TEMPNUM(rv) = RV_TEMPNUM(rv) - 1
	   if (rv_verify_aps(rv,APPARAM(rv),APLIST(rv,1),NUMAPS(rv)) != OK)
		return (ERR_READ)
	   if (imtrgetim(rinfile,RV_TEMPNUM(rv),RIMAGE(rv),SZ_FNAME) != EOF){
		if (get_spec(rv,RIMAGE(rv),REFER_SPECTRUM) == ERR_READ)
		    call error (0,"Error reading next template.")
		call rv_imtitle (RIMAGE(rv), TEMPNAME(rv), SZ_FNAME)
	        written = false
		RV_TEMPCODE(rv) = TEMPCODE(rv,RV_TEMPNUM(rv))
		call amovkr (0.0, COEFF(rv,1), 4)
		RV_FITDONE(rv) = NO
	        RV_NEWXCOR(rv) = YES
		IS_DBLSTAR(rv) = NO
	 	RV_Y1(rv) = INDEF
		RV_Y2(rv) = INDEF
		RV_WINCENPAR(rv) = clgetr ("wincenter")
        
	   } else 
	       call rv_errmsg ("Error reading previous image from list.")
        } else {
	    call rv_errmsg ("At beginning of template list.")
	    RV_NEWXCOR(rv) = NO
	}

	return (OK)
end


# NEXT_AP - Get the next spectrum in the aperture data list.  Updates the
# aperture number in the struct and returns an error code if something went 
# wrong.  

int procedure next_ap (rv, written)

pointer	rv					#I RV struct pointer
bool	written					#I Have results been written?

int	apnum
int	rv_imdim(), rv_getim()
real	clgetr()
bool	silent

begin
	silent = false
	if (CURAPNUM(rv) == 0) {
	    apnum = 1
	    silent = TRUE
	} else
	    apnum = CURAPNUM(rv)

	if (apnum+1 > NUMAPS(rv)) {
	    if (!silent) {
                call rv_errmsg ("At end of object aperture list.")
		RV_NEWXCOR(rv) = NO
	    }
	    return (OK)
        } else {
	    # Get the next object aperture
	    CURAPNUM(rv) = CURAPNUM(rv) + 1
            RV_APNUM(rv) = APLIST(rv,CURAPNUM(rv))
            if (rv_getim(rv, IMAGE(rv), OBJECT_SPECTRUM, INDEF, INDEF, 
		INDEFI) == ERR_READ) {
		    RV_APNUM(rv) = APLIST(rv,apnum)
		    RV_OAPNUM(rv) = RV_APNUM(rv)
                    return (ERR_READ)
	    }
            written = false

            # Now update the data cache flags
	    call amovkr (0.0, COEFF(rv,1), 4)
	    RV_FITDONE(rv) = NO
	    RV_NEWXCOR(rv) = YES
	    IS_DBLSTAR(rv) = NO
	    RV_Y1(rv) = INDEF
	    RV_Y2(rv) = INDEF
	    RV_WINCENPAR(rv) = clgetr ("wincenter")
	}

	# Now try to get the next template aperture (may be one-dimensional)
	if (rv_imdim(RIMAGE(rv),2) > 1) {
	    if (apnum+1 > NUMAPS(rv)) {
	        call rv_errmsg ("At end of template aperture list.")
		RV_NEWXCOR(rv) = NO
	    } else {
               	if (rv_getim(rv,RIMAGE(rv), REFER_SPECTRUM, INDEF, INDEF,
		    INDEFI) == ERR_READ)
                        return (ERR_READ)
                written = false
	    }
	}

        return (OK)
end


# PREV_AP - Get the previous spectrum in the aperture data list.  Updates the
# aperture number in the struct and returns an error code if something went 
# wrong.  Since all of the aperture have been read from disk, just decrement
# the aperture number and pull the data from the BIN cache.

int procedure prev_ap (rv, written)

pointer	rv					#I RV struct pointer
bool	written					#I Have results been written?

int	apnum
int	rv_imdim(), rv_getim()
real	clgetr()
bool	silent

begin
	silent = false
	if (apnum == (NUMAPS(rv) + 1)) {
	    apnum = NUMAPS(rv)
	    silent = TRUE
	} else
	    apnum = CURAPNUM(rv)

        if (apnum-1 < 1) {
	    if (!silent) {
                call rv_errmsg ("At beginning of object aperture list.")
		RV_NEWXCOR(rv) = NO
	    }
	    return (OK)
        } else {
	    # Get the next object aperture
	    CURAPNUM(rv) = CURAPNUM(rv) - 1
            RV_APNUM(rv) = APLIST(rv,CURAPNUM(rv))
            if (rv_getim(rv, IMAGE(rv), OBJECT_SPECTRUM, INDEF, INDEF,
		INDEFI) == ERR_READ) {
		    RV_APNUM(rv) = APLIST(rv,apnum)
		    RV_OAPNUM(rv) = RV_APNUM(rv)
                    return (ERR_READ)
	    }
            written = false

            # Now update the data cache flags
	    call amovkr (0.0, COEFF(rv,1), 4)
	    RV_FITDONE(rv) = NO
	    RV_NEWXCOR(rv) = YES
	    IS_DBLSTAR(rv) = NO
	    RV_Y1(rv) = INDEF
	    RV_Y2(rv) = INDEF
	    RV_WINCENPAR(rv) = clgetr ("wincenter")
	}

	# Now try to get the next previous aperture (may be one-dimensional)
	if (rv_imdim(RIMAGE(rv),2) > 1) {
            if (rv_getim(rv, RIMAGE(rv), REFER_SPECTRUM, INDEF, INDEF,
		INDEFI) == ERR_READ)
                    return (ERR_READ)
            written = false
	}

	return (OK)
end


# GET_SPEC - Low level routine to do the common drudge work of reading the
# spectrum, plotting the new spec and displaying mask.

int procedure get_spec (rv, imname, spec_type)

pointer	rv					#I RV struct pointer
char	imname[SZ_FNAME]			#I Image name to read
int	spec_type				#I Type of spectrum to read

int	rv_getim()

begin
	# Try to read the data from the image
   	if (rv_getim(rv, imname, spec_type, INDEF, INDEF, INDEFI) == ERR_READ) {
	    RV_NEWXCOR(rv) = NO
	    return (ERR_READ)
	}
	RV_NEWXCOR(rv) = YES
	return (OK)
end


# CONSTRUCT_FILE_NAMES - Construct the log file names from a root

procedure cons_file_names (root, log, meta, verb, maxch)

char	root[maxch]				#I Root file name
char	log[maxch]				#O Text log file name
char	meta[maxch]				#O Metacode file name
char	verb[maxch]				#O Verbose file name
int	maxch					#I Max chars

begin
	call sprintf (log, maxch, "%s.txt")
	    call pargstr (root)
	call sprintf (meta, maxch, "%s.gki")
	    call pargstr (root)
	call sprintf (verb, maxch, "%s.log")
	    call pargstr (root)
end


# RV_IMDIM - Utility to get simply a dimensionality of an image name.

int procedure rv_imdim (image, dim)

char	image[SZ_FNAME]				#I Image name
int	dim					#I Which dimension to get

int	dim_len
pointer	im, sp, bp, immap()
errchk	immap

begin
	call smark (sp)
	call salloc (bp, SZ_FNAME, TY_CHAR)

	iferr (im = immap(image, READ_ONLY, 0)) {
	    call sprintf (Memc[bp], SZ_FNAME,
		 "rv_imdim: Error opening image `%s'.")
		    call pargstr (image)
	    call error (0, Memc[bp])
	}

	if (dim == 1) {
	    dim_len = IM_LEN(im,1)
	} else if (dim == 2) {
	    if (IM_NDIM(im) == 1 || (IM_NDIM(im) == 2 && IM_LEN(im,2) == 1))
		dim_len = 1
	    else
	        dim_len = IM_LEN(im,2)
	} else
	    dim_len = IM_LEN(im,dim)

	call imunmap (im)
	call sfree (sp)
	return (dim_len)
end


# RV_IMTITLE - Procedure to get the IM_TITLE string given a file name

procedure rv_imtitle (image, title, maxchar)

char	image[SZ_FNAME]				#I Image name
char	title[SZ_FNAME]				#O Title
int	maxchar					#I Max chars

pointer	im, sp, bp,  immap()
errchk	immap

begin
	call smark (sp)
	call salloc (bp, SZ_LINE, TY_CHAR)

	iferr (im = immap(image, READ_ONLY, 0)) {
	    call sprintf (Memc[bp], SZ_FNAME,
		 "rv_imtitle: Error opening image `%s'.")
		    call pargstr (image)
	    call error (0, Memc[bp])
	}
        call rv_fill_blanks (IM_TITLE(im), title, maxchar)

	call imunmap (im)
	call sfree (sp)
end


# RV_IMTEMPVEL - Procedure to get the VHELIO string given a file name.

real procedure rv_imtempvel (rv, image)

pointer	rv					#I RV struct pointer
char	image[SZ_FNAME]				#I Image name

pointer	im, sp, bp,  immap()
real	tvel, imgetr()
int	imaccf()
errchk	immap, imaccf, imgetr

begin
	call smark (sp)
	call salloc (bp, SZ_FNAME, TY_CHAR)

	iferr (im = immap(image, READ_ONLY, 0)) {
	    call sprintf (Memc[bp], SZ_FNAME,
		 "rv_imtempvel: Error opening image `%s'.")
		    call pargstr (image)
	    call error (0, Memc[bp])
	}

	# Get the velocity from the reference star image header.  Save the
	# warning for outputting results.
	if (imaccf(im, KW_VHELIO(rv)) == YES)
	    tvel = imgetr (im, KW_VHELIO(rv))
	else
	    tvel = INDEFR

	call imunmap (im)
	call sfree (sp)
	return (tvel)
end
