include <ctype.h>
include <imhdr.h>
include <imset.h>
include	<pmset.h>

# MP_OPEN -- Open the specified mask for image i/o
#
# Open the specified pixel mask. The input pixel mask specification may be
#
#     ""               The mask is undefined
#
#     "EMPTY"          The mask is undefined
#
#     "!KEYWORD"       The mask is the pixel mask pointed to by the reference
#                      image header keyword KEYWORD
#     "!^KEYWORD"      The mask is inverse of the pixel mask pointed to by the
#                      reference image header keyword KEYWORD
#     "MASK"           The mask is a pixel mask or image 
#                      
#     "^MASK"          The mask is inverse of the pixel mask or image
#                      
#     "EXPR"           The mask is specified by an integer expression
#                      
#     "@FILE"          The mask is specified by the an integer expression in 
#                      the text file FILE
#
# The input mask specification is transformed into a simple 0 and 1 mask
# internally where 0 is the pass value and 1 is the stop value. The format
# of EXPR is still a TBD but I would eventually like to support
# an algebra that includes simple image  expressions as in the IMEXPR task,
# and regions descriptors similar to those defined in the PROS XRAY package.
# The latter have the problem in that they must be limited to 1D images (point,
# line egments) or 2D images (box, rectangle, ellipse, # annulus, wedge, etc).
# It maybe possible to expand this to 3D in some cases,  e.g. cubes, spheres,
# ellipsoids etc although dealing with the angles may  become complicated. At
# any rate I will put aside the issue of on the fly mask generation for the
# moment. If a section is specified on the input image but not on the mask
# image then imio/mio will automatically track the proper section in the mask.
# If a section is specified on the mask that section of the mask will be used,
# and it must correspond in size to the input image or image section.

pointer procedure mp_open (pmsource, refim, pmname, sz_pmname) 

char	pmsource[ARB]			#I the pixel mask specificiation
pointer	refim				#I the reference image pointer
char	pmname[ARB]			#O the pixel mask name
int	sz_pmname			#I the maximum  pixel name length

pointer	sp, fname, kfname
pointer	pmim, pm
int	ip, flags, invflag
pointer	im_pmmap(), mp_pmmap()
int	imaccess(), imstati()
bool	streq()
errchk	im_pmmap(), mp_pmmap(), imgstr()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (kfname, SZ_FNAME, TY_CHAR)

	# Remove leading whitespace from the pixel source specification.
	ip = 1
	while (IS_WHITE(pmsource[ip]))
	    ip = ip + 1
	call strcpy (pmsource[ip], Memc[fname], SZ_FNAME)
	flags = 0
	pmname[1] = EOS

	# If the mask is undefined specify an empty mask.
	if (Memc[fname] == EOS || streq (Memc[fname], "EMPTY")) {

	    ifnoerr (pmim = im_pmmap ("EMPTY", READ_ONLY+BOOLEAN_MASK, refim)) {
		call strcpy ("EMPTY", pmname, sz_pmname)
		pm = imstati (pmim, IM_PMDES)
		call mp_invert (pm)
		call imseti (pmim, IM_PMDES, pm)
	    } else
		pmim = NULL

	# If the mask specification is a keyword.
	} else if (Memc[fname] == '!') {

	    # Invert the specified mask. Note there is a bug in the
	    # invert mask flag which needs to be worked around.
	    ip = 1
	    if (Memc[fname+ip] == '^') {
		ip = ip + 1
		flags = BOOLEAN_MASK
		invflag = NO
	    } else {
		#flags = INVERT_MASK + BOOLEAN_MASK
		flags = BOOLEAN_MASK
		invflag = YES
	    }

	    # Find the mask name.
	    ifnoerr (call imgstr (refim, Memc[fname+ip], Memc[kfname],
	        SZ_FNAME)) {
	        iferr (pmim = mp_pmmap (Memc[kfname], refim, flags, invflag)) {
		    pmim = NULL
		} else if (invflag == NO) {
		    call strcpy ("^", pmname, sz_pmname)
		    call strcat (Memc[kfname], pmname, sz_pmname)
		} else {
		    call strcpy (Memc[kfname], pmname, sz_pmname)
		}
	    } else
		pmim = NULL

	# If the mask specification is a mask / or image file.
	} else if (imaccess (Memc[fname], READ_ONLY) == YES) {

	    #flags = BOOLEAN_MASK+INVERT_MASK
	    flags = BOOLEAN_MASK
	    invflag = YES
	    call strcpy (Memc[fname], pmname, sz_pmname)
	    iferr (pmim = mp_pmmap (Memc[fname], refim, flags, invflag)) 
		pmim = NULL
	    else
	        call strcpy (Memc[fname], pmname, sz_pmname)

	} else if (Memc[fname] == '^') {
	    if (imaccess (Memc[fname+1], READ_ONLY) == YES) {
	        flags = BOOLEAN_MASK
	        invflag = NO
	        call strcpy (Memc[fname], pmname, sz_pmname)
	        iferr (pmim = mp_pmmap (Memc[fname+1], refim, flags, invflag))
		    pmim = NULL
		else
	            call strcpy (Memc[fname], pmname, sz_pmname)
	    } else
		pmim = NULL

	} else {
	    pmim = NULL
	}

	call sfree (sp)

	return (pmim)
end


# MP_PMMAP - Open a pixel mask READ_ONLY. The input mask may be a pixel
# list image or a non-pixel list image. The invflag is temporary, put into
# deal with the fact that mio has a bug in this flag.


pointer procedure mp_pmmap (pmname, refim, flags, invflag)

char	pmname[ARB]		#I the pixel list or image name
pointer	refim			#I the reference image descriptor
int	flags			#I the pixel list or image flags
int	invflag			#I invert mask flag, remove when pmio fixed

pointer	sp, section, pmim, pm, tmp_refim
int	use_section
pointer	im_pmmap(), mp_immap()
int	imstati()
errchk	im_pmmap(), mp_immap()

begin
	# Does the pmname include an image section.
	call smark (sp)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call imgsection (pmname, Memc[section], SZ_FNAME)
	if (Memc[section] == EOS) {
	    use_section = NO
	    tmp_refim = refim
	} else {
	    use_section = YES
	    tmp_refim = NULL
	}

	# Open the mask as a pixel list.
	ifnoerr (pmim = im_pmmap (pmname, READ_ONLY+flags, tmp_refim)) {

	    if (use_section == YES)
	        call mp_section (pmim)
	    if (invflag == YES) {
	        pm = imstati (pmim, IM_PMDES)
	        call mp_invert (pm)
		call imseti (pmim, IM_PMDES, pm)
	    }

	# Open the mask as an image file.
	} else ifnoerr (pmim = mp_immap (pmname)) {

	    if (invflag == YES) {
	        pm = imstati (pmim, IM_PMDES)
	        call mp_invert (pm)
		call imseti (pmim, IM_PMDES, pm)
	    }

	} else {
	    pmim = NULL
	}

	call sfree (sp)

	return (pmim)
end


# MP_IMMAP -- Map an image as a pixel file

pointer procedure mp_immap (pmname)

char	pmname[ARB]		#I the pixel list or image name

pointer	sp, v1, v2, im, pm, data, pmim
int	ndim, npix
pointer	immap(), pm_newmask(), im_pmmapo()
int	imgnli()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	# Open the input image.
	im = immap (pmname, READ_ONLY, 0)
	ndim = IM_NDIM(im)
	npix = IM_LEN(im,1)

	# Open the mask with a depth of 1 bit.
	pm = pm_newmask (im, 1)

	# Copy the image to a mask.
	while (imgnli (im, data, Meml[v1]) != EOF) {
	    # may need to convert negative values here ...
	    call pm_plpi (pm, Meml[v2], Memi[data], 0, npix, PIX_SRC)
	    call amovl (Meml[v1], Meml[v2], ndim)
	}
	call imunmap (im)

	pmim = im_pmmapo (pm, NULL)

	call sfree (sp)

	return (pmim)
end


# MP_SECTION -- Create the a new mask from the specified mask section.

procedure mp_section (pmim)

pointer	pmim			#U mask image descriptor

pointer	newpm, newpmim, sp, v1, v2, ibuf
pointer	pl_create(), im_pmmapo()
int	ndim, depth, npix
int	imgnls()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)

	ndim = IM_NDIM(pmim)
	depth = 1
	npix = IM_LEN(pmim,1)

	newpm = pl_create (ndim, IM_LEN(pmim,1), depth)
	while (imgnls (pmim, ibuf, Meml[v1]) != EOF) {
	    call pm_plps (newpm, Meml[v2], Mems[ibuf], 1, npix, PIX_SRC)
	    call amovl (Meml[v1], Meml[v2], ndim)
	}

	call imunmap (pmim)
	newpmim = im_pmmapo (newpm, NULL)
	pmim = newpmim

	call sfree (sp)
end


# MP_MPCOPY -- Copy the input to the output mask setting the mapping
# parameters appropriately

procedure mp_mpcopy (im, pmim, pmout)

pointer	im			#I the input image descriptor
pointer	pmim			#I the input mask descriptor
pointer	pmout			#I the output mask descriptor

pointer	sp, axlen, v, oldpm, newpm
int	naxes, depth
pointer	pl_create()
int	imstati(), mp_samesize()

int	pm_stati()
int	refim, mapstat

begin
	call smark (sp)
	call salloc (axlen, IM_MAXDIM, TY_LONG)
	call salloc (v, IM_MAXDIM, TY_LONG)

	# Create new mask.
	oldpm = imstati (pmim, IM_PLDES)
	call pl_gsize (oldpm, naxes, Meml[axlen], depth)
	newpm = pl_create (naxes, Meml[axlen], depth)

	# Store old values of the input mask reference image and mapping
	# descriptors here. Maybe ...
	refim = pm_stati (oldpm, P_REFIM)
	mapstat = pm_stati (oldpm, P_MAPXY)

	# Set the input mask mapping parameters.
	call pm_seti (oldpm, P_REFIM, im)
	if (mp_samesize (im, pmim) == YES)
	    call pm_seti (oldpm, P_MAPXY, NO)

	# Restore old values of the input mask reference image and mapping
	# descriptors here. Maybe ...

	# Store old values of the output reference image and mapping descriptors
	# here. Don't need to do this since this is the desired behavior.

	# Set the input mask mapping parameters.
	call pm_seti (newpm, P_REFIM, im)
	if (mp_samesize (im, pmim) == YES)
	    call pm_seti (newpm, P_MAPXY, NO)

	# Restore old values of the output mask reference image and mapping
	# descriptors here. Don't need to do this since this is the
	# desired behavior.

	# Copy the input to the output mask using the mapping parameters
	# as appropriate
	call amovkl (long(1), Meml[v], IM_MAXDIM)
	call pm_rop (oldpm, Meml[v], newpm, Meml[v], Meml[axlen], PIX_SRC)

	call imseti (pmout, IM_PLDES, newpm)
	call sfree (sp)
end


# MP_MIOPEN - Open an mio descriptor and set the mapping parameters 
# appropriately. This should be done by doing pm_stati calls on
# the pm descriptor via the P_REFIM and P_MAPXY parameters and the
# corresponding PRIVATE1 / PRIVATE2 parameters in plio but this
# mechanism is not working at present. For now test im / pmim for
# equality in number of dimensions and size.

pointer procedure mp_miopen (im, pmim)

pointer	im			#I the input image descriptor
pointer	pmim			#I the input mask image descriptor

pointer	pm, mp
int	samesize
pointer	mio_openo()
int	imstati(), mp_samesize()

begin
	# Open the pixel mask.
	pm = imstati (pmim, IM_PLDES)

	# Open the mio descriptor which set the mapping status using
	# the image descriptor, i.e. the mapping status is yes if the
	# image was opened with a section.
	mp = mio_openo (pm, im)

	# Turn off mapping if  the image and mask are exactly the same
	# size.
	samesize = mp_samesize (im, pmim)
	if (samesize == YES)
	    call pm_seti (pm, P_MAPXY, NO)

	return (mp)
end


# MP_SAMESIZE -- Return YES if the image and mask are the same size.

int procedure mp_samesize (im, pmim)

pointer	im			#I the input image descriptor
pointer	pmim			#I the input image descriptor

int	i, samesize

begin
	if (IM_NDIM(im) == IM_NDIM(pmim)) {
	    samesize = YES
	    do i = 1, IM_NDIM(im) {
		if (IM_LEN(im,i) == IM_LEN(pmim,i))
		    next
		samesize = NO
		break
	    }
	} else {
	    samesize = NO
	}

	return (samesize)
end


# MP_INVERT -- Invert a pixel mask.

procedure mp_invert (pm)

pointer	pm			#U plio descriptor

pointer	sp, axlen, v, newpm
int	naxes, depth
pointer	pl_create()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (axlen, IM_MAXDIM, TY_LONG)
	call salloc (v, IM_MAXDIM, TY_LONG)

	# Get pixel mask characteristics.
	call pl_gsize (pm, naxes, Meml[axlen], depth)

	# Create the new inverted mask.
	newpm = pl_create (naxes, Meml[axlen], depth)
	call amovkl (long(1), Meml[v], IM_MAXDIM)
	call pl_rop (pm, Meml[v], newpm, Meml[v], Meml[axlen],
	    PIX_NOT(PIX_SRC))

	# Close the old mask and update the mask pointer.
	call pl_close (pm)
	pm = newpm

	call sfree (sp)
end


# MP_COPY -- Make a copy of an existing pixel mask.

pointer procedure mp_copy (oldpm)

pointer	oldpm			#I old pixel mask pointer

pointer	sp, axlen, v, newpm
int	naxes, depth
pointer	pl_create()

begin
	call smark (sp)
	call salloc (axlen, IM_MAXDIM, TY_LONG)
	call salloc (v, IM_MAXDIM, TY_LONG)

	call pl_gsize (oldpm, naxes, Meml[axlen], depth)
	newpm = pl_create (naxes, Meml[axlen], depth)

	call amovkl (long(1), Meml[v], IM_MAXDIM)
	call pl_rop (oldpm, Meml[v], newpm, Meml[v], Meml[axlen],
	    PIX_SRC)

	call sfree (sp)

	return (newpm)
end

