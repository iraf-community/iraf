# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<syserr.h>
include	<error.h>
include	<imhdr.h>
include	<imio.h>
include <finfo.h>
include <fset.h>
include <mii.h>
include <mach.h>
include	"fxf.h"


# FXF_OPEN -- Open/create a FITS format image with extensions.

procedure fxf_open (kernel, im, o_im, root, extn, ksection, group, gc_arg,
	acmode, status)

int	kernel			#I IKI kernel
pointer	im			#I image descriptor
pointer	o_im			#I other descriptor for NEW_COPY image
char	root[ARB]		#I root image name
char	extn[ARB]		#I extension, if any
char	ksection[ARB]		#I [extname,extver,overwrite,append,inherit..]
int	group			#I index of group to be accessed
int	gc_arg			#I [NOT USED]
int	acmode			#I access mode
int	status			#O status flag to calling routine

long	fi[LEN_FINFO]
int	newimage, i, gn, ksinh, type, fmode
pointer	sp, path, fit_extn, ua, o_fit, fit
bool    pre_read, fks_extn_or_ver, dyh, fsec, plio
int	fxf_check_dup_extnv(), itoc(), strcmp(), strncmp()
int     open(), access(), imgeti(), fstatl(), finfo(), fxf_header_size()
pointer pl_open()

errchk	fmkcopy, calloc, open, fxf_rheader, fxf_prhdr, fxf_gaccess
errchk	fxf_fclobber, fxf_ksection, fxf_alloc, syserr, syserrs
errchk  fxf_check_group
define	duperr_ 91
define	err_ 92

begin
	call smark (sp)
	call salloc (path, SZ_PATHNAME, TY_CHAR)
	call salloc (fit_extn, FITS_LENEXTN, TY_CHAR)
	call fxf_init()
	ua = IM_USERAREA(im)

	fmode = acmode

	# Allocate internal FITS image descriptor.
	call fxf_alloc (fit)

	IM_KDES(im) = fit
	IM_HFD(im) = NULL
	FIT_IM(fit) = im
	call amovki (1, FIT_LENAXIS(fit,1), IM_MAXDIM)

	# Generate full header file name.
	if (extn[1] == EOS) {
	    call fxf_gethdrextn (im, o_im, fmode, Memc[fit_extn], FITS_LENEXTN)
	    call iki_mkfname (root, Memc[fit_extn], Memc[path], SZ_PATHNAME)
	    call strcpy (Memc[fit_extn], extn, FITS_LENEXTN)
	} else                                 
	    call iki_mkfname (root, extn, Memc[path], SZ_PATHNAME)

	# Header and pixel filename are the same.
	call strcpy (Memc[path], IM_HDRFILE(im), SZ_IMHDRFILE)
	call strcpy (IM_HDRFILE(im), IM_PIXFILE(im), SZ_IMPIXFILE)

	newimage = NO
	if (access (IM_HDRFILE(im), 0, 0) == NO)
	    newimage = YES
	FIT_NEWIMAGE(fit) = newimage 

	# Initialize kernel section default values.
	call fxf_ksinit (fit)

	# For simplicity treat the APPEND mode as NEW_IMAGE.  For the FK
	# is the same.

	if (fmode == APPEND)
	    fmode = NEW_IMAGE
	FIT_ACMODE(fit) = fmode

	# Read fkinit and ksection and check that the extension number
	# specifications therein and the IMIO cluster index "group" are
	# consistent.

	call fxf_check_group (im, ksection, fmode, group, ksinh)

	fks_extn_or_ver = FKS_EXTNAME(fit) != EOS || !IS_INDEFL(FKS_EXTVER(fit))

	# Check if a file section is necessary.
	fsec = (fks_extn_or_ver || group >= 0)
	call fxf_gaccess (im, fsec)

	# The previous call could have changed FIT_NEWIMAGE; reset value.
	newimage = FIT_NEWIMAGE(fit)

	if (fks_extn_or_ver)
	   FIT_GROUP(fit) = -1

	# See if we want to write a dummy primary unit.
	#
	# For PLIO, if creating a new output file and we want to create a
	# BINTABLE, create a dummy header.  Otherwise see if a type is
	# requested, in which case we would need to create a dummmy header
	# if no file is present yet.

	type = 0
	if (FKS_SUBTYPE(fit) == FK_PLIO)
	    type = FK_PLIO

	dyh = false
	if (newimage == YES && (fks_extn_or_ver || type > 0)) {
	    call fxf_dummy_header (im, status)
	    if (status == ERR)
		goto err_ 
	    newimage = NO
	    dyh = true
	    if (fmode == NEW_COPY && type == FK_PLIO) 
		FIT_PIXOFF(fit) = fxf_header_size(im) + FITS_BLOCK_CHARS
	}
	if (newimage == NO) {
	    if (finfo (IM_HDRFILE(im), fi) != ERR)
		FIT_EOFSIZE(fit) = (FI_SIZE(fi)+SZB_CHAR-1)/SZB_CHAR + 1
	    else
		call syserrs (SYS_FOPEN, IM_HDRFILE(im))
	}

	if (newimage == YES)
	    FKS_OVERWRITE(fit) = NO
	else
	    FIT_XTENSION(fit) = YES

	FIT_NEWIMAGE(fit) = newimage

	# If all these conditions are met then set the pre_read flag.
	pre_read = (fks_extn_or_ver ||
	    FKS_OVERWRITE(fit) == YES || FKS_INHERIT(fit) == YES)

	if (newimage == NO && fmode != READ_ONLY) {
	    # See that INHERIT makes sense if it has been set by
	    # 'fkinit' when reading a file with PHU (naxis != 0).

	    if (FKS_INHERIT(fit) == YES && group != 0) {
		gn = 0
		iferr (call fxf_prhdr (im, gn)) {
		    FKS_INHERIT(fit) = NO

		    # Issue an error only if the inherit is in the filename.
		    if (fmode == NEW_COPY && ksinh == YES)
			call syserr (SYS_FXFBADINH)
	        } else if (FIT_NAXIS(fit) != 0)
		    FKS_INHERIT(fit) = NO

	        # Reset the pre_read flag.
		pre_read = ((FKS_DUPNAME(fit) == NO && 
		    FKS_INHERIT(fit) == YES) || FKS_OVERWRITE(fit) == YES)
	    }

	    if (pre_read && fmode != NEW_COPY && !dyh)
	        call fxf_prhdr (im, group)

	    if (access (IM_HDRFILE(im), fmode, 0) == NO)
		call syserrs (SYS_FNOWRITEPERM, IM_HDRFILE(im))
	}

	switch (fmode) {
	case NEW_IMAGE, APPEND:
	    if (newimage == NO) {
	        # Make sure the UA is empty when overwriting.
	        if (pre_read && FKS_OVERWRITE(fit) == YES)  
		    Memc[ua] = EOS
		 
	        if (FKS_DUPNAME(fit) == NO)
	            if (fxf_check_dup_extnv (im, group) == YES)
		        goto duperr_
	    } else {
		# See if it is necessary to invalidate the cache entry for the
		# current filename. It could happen that the user has deleted
		# the filename and a new file with the same is created.

		call fxf_check_old_name (im)
	    }
	    
	    if (FKS_INHERIT(fit) == YES)
		FIT_INHERIT(fit) = YES

	    # Initialize a new copy of a PLIO image mask.
	    if (type == FK_PLIO) 
		IM_PL(im) = pl_open (NULL)

	case NEW_COPY:
	    # Completely new copy of an existing image. This could mean a
	    # new file or append a new image to an existing file.

	    # Initialize a new copy of a PLIO image mask.
	    if (type == FK_PLIO) {
		IM_PL(im) = pl_open (NULL)
	        if (IM_PL(o_im) != NULL)
	            call fxf_plpf (im)
	    }

	    if (newimage == YES || FKS_APPEND(fit) == NO) 
		call fxf_check_old_name (im)

	    # For a PLIO mask, make sure there are no SUBYTPE keywords in
	    # the UA since this will be rewritten by fxf_updhdr().
	    
	    if (IM_PL(o_im) != NULL)
		call fxf_clean_pl (im)

	    if (IM_KDES(o_im) != NULL && IM_KERNEL(o_im) == IM_KERNEL(im)) {
		o_fit = IM_KDES(o_im)
	        call strcpy (FIT_EXTTYPE(o_fit), FIT_EXTTYPE(fit), SZ_EXTTYPE)
		call strcpy (FIT_EXTNAME(o_fit), FIT_EXTNAME(fit), LEN_CARD)
		FIT_EXTVER(fit) =  FIT_EXTVER(o_fit) 

		# Reset the value of the keyword INHERIT in the new_copy
		# image if the input has a no_inherit in the filename.

		FIT_INHERIT(fit) = NO
		call fxf_filter_keyw (im, "INHERIT")

		# Change the value only if explicitly done in the output
		# kernel section.

		if (FKS_INHERIT(fit) == YES)
	            FIT_INHERIT(fit) = YES

	    } else {
		# Reblock if old image is imh for example.
		if (IM_UABLOCKED(im) != YES)
		    call fxf_reblock (im)

		# See if the old image have EXTNAME or EXTVER keywords.
		# Notice that old image does not have to be of FITS type.

		iferr (call imgstr (o_im,"EXTNAME",FIT_EXTNAME(fit),LEN_CARD))
		    FIT_EXTNAME(fit) = EOS
		iferr (FIT_EXTVER(fit) = imgeti (o_im, "EXTVER"))
		    FIT_EXTVER(fit) = INDEFL
		call strcpy ("IMAGE", FIT_EXTTYPE(fit), SZ_EXTTYPE)
	    }

	    # Delete ORIGIN keyword, since we are going to put a new one.
	    call fxf_filter_keyw (im, "ORIGIN")

	    # Now that we have a new_copy of the input FITS structure,
	    # initialize some of its members.

	    FIT_HFD(fit) = NULL
	    FIT_NEWIMAGE(fit) = newimage
	    if (newimage == NO)
		FIT_XTENSION(fit) = YES
	    FIT_ACMODE(fit) = fmode
	    if (FKS_APPEND(fit) != YES)
	        FIT_GROUP(fit) = group
	    FIT_BSCALE(fit) = 1.0d0
	    FIT_BZERO(fit) = 0.0d0

            if (FKS_OVERWRITE(fit) == NO) {
		if (FKS_EXTNAME(fit) == EOS)
		    call strcpy (FIT_EXTNAME(fit), FKS_EXTNAME(fit), LEN_CARD)
		else
		    call imastr (im, "EXTNAME", FKS_EXTNAME(fit))
    
		if (IS_INDEFL(FKS_EXTVER(fit)))
		    FKS_EXTVER(fit) =  FIT_EXTVER(fit) 
		else
		    call imaddi (im, "EXTVER", FKS_EXTVER(fit))

		# We need to pre_read extensions headers to check for
		# duplicates with these extname and extver.

		if (FKS_EXTNAME(fit) != EOS ||!IS_INDEFL(FKS_EXTVER(fit))) 
		    pre_read = true
	    }

	    if (newimage == NO && !dyh) {
	        if (pre_read) {
		    iferr (call fxf_prhdr (im, group))
			;
	        }

		# Check for duplicated EXTNAME and/or EXTVER if any of the
		# following conditions are met.

		if (FKS_DUPNAME(fit) == NO && FKS_OVERWRITE(fit) == NO && 
		    (fks_extn_or_ver || FIT_EXTNAME(fit) != EOS || 
		    !IS_INDEFL(FIT_EXTVER(fit)))) {
		    if (fxf_check_dup_extnv (im, group) == YES)
		        goto duperr_
		}
	    }

	    FIT_NAXIS(fit) = IM_NDIM(im)
	    do i = 1, IM_NDIM(im)
		FIT_LENAXIS(fit,i) = IM_LEN(im,i)

	    # Inherit datatype of input template image if specified,
	    # otherwise default datatype to real.

	    if (IM_PIXTYPE(o_im) != NULL)
		IM_PIXTYPE(im) = IM_PIXTYPE(o_im)
	    else
		IM_PIXTYPE(im) = TY_REAL

	default:
	    # No Overwrite allowed in READ_ONLY or READ_WRITE.
	    FKS_OVERWRITE(fit) = NO

	    # Check that we have single FITS file.
	    if (!fsec && group == -1)
	        group = 0

	    # Open an existing image.
            iferr (call fpathname (IM_HDRFILE(im), Memc[path], SZ_PATHNAME))
		goto err_
	    if (fmode == READ_WRITE)
		IM_HFD(im) = open (Memc[path], READ_WRITE, BINARY_FILE)
	    else
		IM_HFD(im) = open (Memc[path], READ_ONLY, BINARY_FILE)

	    iferr (call fxf_rheader (im, group, fmode)) {
		call close (IM_HFD(im))
		call mfree (fit, TY_STRUCT)
		call sfree (sp)
		status = ERR
		call erract (EA_ERROR)
	    }

	    if (group == 0) 
	        FIT_XTENSION(fit) = NO
	    else
	        FIT_XTENSION(fit) = YES

	    # Some non-iraf fits files might have keywords that are
	    # imcompatible with our header. For example if hediting the header,
	    # make sure that they are eliminated.

	    if (fmode == READ_WRITE)
	       call fxf_discard_keyw (im)

	    FIT_EOFSIZE(fit) = fstatl (IM_HFD(im), F_FILESIZE) + 1

	    # PLIO.  If we read the header of a PLIO_1 compressed image file
	    # then it is a PL file; now read the data.

	    plio = (strncmp (FIT_EXTSTYPE(fit), "PLIO_1", 6) == 0)
	    if (plio)  {
		call fxf_plread (im)

		# We need to setup the IMIO descriptor if we need to write
		# over a section; in particular IM_PFD needs to be defined.

		if (fmode == READ_WRITE)
		    call fxf_plpf (im)
	    }

	    # Close the header file.
	    call close (IM_HFD(im))
	    IM_HFD(im) = NULL

	    # Do not allow the user to see any non_IMAGE extensions.
	    if (strcmp ("IMAGE", FIT_EXTTYPE(fit)) != 0 &&
		    strcmp ("SIMPLE", FIT_EXTTYPE(fit)) != 0 && !plio)
		call syserrs (SYS_IKIEXTN, IM_NAME(im))
	}

	FIT_HFD(fit) = IM_HFD(im)
	status = OK

	call sfree (sp)
	return
duperr_
	i = itoc (group, Memc[path], LEN_CARD)
	call syserrs (SYS_FXFOPEXTNV, Memc[path])
err_
	status = ERR
	call mfree (fit, TY_STRUCT)
	call sfree (sp)
end


# FXF_ALLOC -- Initialize memory for the FIT descriptor.

procedure fxf_alloc (fit)

pointer fit	 		#I input fits descriptor

errchk	calloc

begin
	call calloc (fit, LEN_FITDES, TY_STRUCT)

	FIT_GROUP(fit) = -1
	FIT_PIXTYPE(fit) = NULL
	FIT_BSCALE(fit) = 1.0d0
	FIT_BZERO(fit) = 0.0d0
	FIT_XTENSION(fit) = NO
	FIT_INHERIT(fit) = NO
	FIT_EOFSIZE(fit) = 0
	FIT_EXTNAME(fit) = EOS
	FIT_EXTVER(fit) = INDEFL
end


# FXF_INIT -- Initialize any runtime FITS kernel descriptors to their
# process startup state.

procedure fxf_init()

int	i
bool	first_time
data	first_time /true/

include "fxfcache.com"

begin
	# Disable the hdrcache until it is fully initialized in rfitshdr.
	if (first_time) {
	    rf_cachesize = 0
	    do i = 1, MAX_CACHE {
		rf_fit[i] = 0
	    }

	    first_time = false
	}
end


# FXF_KS_RDHDR -- Procedure to preread the FITS headers up to group
# 'group'.  The idea is to have the offset pointers in memory since the
# can be overwritten or when no group (i.e. -1) is given and the extname or
# extver are specified.

procedure fxf_prhdr (im, group)

pointer	im  	 		#I image descriptor
int	group			#I maximum group number to read

int	poff, extv
pointer	fit, lim, lfit, sp, path
errchk	fpathname, open, syserr, fxf_alloc, calloc
int	open(), imgeti()

begin
	call smark (sp)
	call salloc (path, SZ_PATHNAME, TY_CHAR)

	# We will use a local temporary imio and fit structures.
#	call calloc (lim, LEN_IMDES+LEN_IMHDR+MIN_LENUSERAREA, TY_STRUCT)
	call calloc (lim, LEN_IMDES+IM_LENHDRMEM(im), TY_STRUCT)

	call fxf_alloc (lfit)

	IM_KDES(lim) = lfit
	fit = IM_KDES(im)

	FIT_GROUP(lfit) = group
	FIT_ACMODE(lfit) = FIT_ACMODE(fit)
	call strcpy (FKS_EXTNAME(fit), FKS_EXTNAME(lfit), LEN_CARD)
	FKS_EXTVER(lfit) = FKS_EXTVER(fit)

	iferr (extv = imgeti (im, "EXTVER"))
	     extv = INDEFL

	FKS_OVERWRITE(lfit) = FKS_OVERWRITE(fit)
	FKS_DUPNAME(lfit) = FKS_DUPNAME(fit)
        FKS_INHERIT(lfit) = FKS_INHERIT(fit)
        FKS_CACHESIZE(lfit) = FKS_CACHESIZE(fit)

	# Open an existing image.
        call strcpy (IM_HDRFILE(im), IM_HDRFILE(lim), SZ_PATHNAME)
        call strcpy (IM_NAME(im), IM_NAME(lim), SZ_PATHNAME)

        call fpathname (IM_HDRFILE(im), Memc[path], SZ_PATHNAME)
	IM_HFD(lim) = open (Memc[path], READ_ONLY, BINARY_FILE)

	IM_LENHDRMEM(lim) = IM_LENHDRMEM(im)

	# If we want to inherit the global header we need to read
	# the group specified in the filename.

	iferr (call fxf_rfitshdr (lim, group, poff)) {
	     call close (IM_HFD(lim))
             call mfree (lfit, TY_STRUCT)
	     call mfree (lim, TY_STRUCT)
	     call sfree (sp)
	     call erract (EA_ERROR)

	} else {
	    call close (IM_HFD(lim))
	    call sfree (sp)
	    if (FKS_OVERWRITE(fit) == YES)
		FIT_GROUP(fit) = FIT_GROUP(lfit)
   	    group = FIT_GROUP(lfit)

	    # Now set the offset pointers to the original 'fit' struct.
	    FIT_HDRPTR(fit) = FIT_HDRPTR(lfit)
	    FIT_PIXPTR(fit) = FIT_PIXPTR(lfit)
	    FIT_EXTEND(fit) = FIT_EXTEND(lfit)

	    FIT_CACHEHDR(fit) = FIT_CACHEHDR(lfit)
	    FIT_CACHEHLEN(fit) = FIT_CACHEHLEN(lfit)

	    FIT_NAXIS(fit) = FIT_NAXIS(lfit)
	    FIT_INHERIT(fit) = FIT_INHERIT(lfit)
	    FIT_PLMAXLEN(fit) = FIT_PLMAXLEN(lfit)

	    IM_CTIME(im) = IM_CTIME(lim)

	    call mfree (lfit, TY_STRUCT)
	    call mfree (lim, TY_STRUCT)

  	    if (extv != INDEFL)
	        call imaddi (im, "EXTVER", extv)
	}
end


# FXF_DUMMY_HEADER -- Built a minimum Primary Fits header.  This is
# necessary in case we are creating an IMAGE extension and we don't 
# want to put any information in the PHU.

procedure fxf_dummy_header (im, status)

pointer im 			#I image descriptor
int	status			#O status flag

char    blank[1]	
pointer sp, path, spp, mii, pn, n
int	iso_cutover, fd, nblanks, size_rec

int	strlen(), open(), envgeti()
long	clktime()

begin
	call smark (sp)
	call salloc (spp, FITS_BLOCK_BYTES, TY_CHAR)
	call salloc (mii, FITS_BLOCK_CHARS, TY_INT)
	call salloc (path, SZ_PATHNAME, TY_CHAR)

	status = OK

	iferr {
	    call fpathname (IM_HDRFILE(IM), Memc[path], SZ_PATHNAME)
	    fd = open (Memc[path], NEW_FILE, BINARY_FILE)
	} then {
	    call sfree (sp)
	    status = ERR
	    return
	}

	pn = spp
	call fxf_akwb ("SIMPLE", YES, "FITS STANDARD", pn)
	call fxf_akwi ("BITPIX", 8, "Character information", pn)
	call fxf_akwi ("NAXIS", 0, "No image data array present", pn)
	call fxf_akwb ("EXTEND", YES, "File may contain extensions", pn)
        call fxf_akwc ("ORIGIN", FITS_ORIGIN,
	    strlen(FITS_ORIGIN), "FITS file originator", pn)

	# Dates after iso_cutover use ISO format dates.
	iferr (iso_cutover = envgeti (ENV_ISOCUTOVER))
	    iso_cutover = DEF_ISOCUTOVER

	# Encode the DATE keyword.
        call fxf_encode_date (clktime(long(0)), Memc[path], LEN_CARD,
	    "ISO", 2000)
	call fxf_akwc ("DATE", Memc[path],
	    strlen(Memc[path]), "Date FITS file was generated", pn)

	blank[1] = ' '
        call amovkc (blank[1], Memc[pn], LEN_CARD)
	call amovc ("END", Memc[pn], 3)
	pn = pn + LEN_CARD

	n = pn - spp
	size_rec = FITS_BLOCK_CHARS
        nblanks = FITS_BLOCK_BYTES - n
	call amovkc (blank[1], Memc[spp+n], nblanks)
	call miipak (Memc[spp], Memi[mii], size_rec*2, TY_CHAR, MII_BYTE)
	call write (fd, Memi[mii], size_rec)

	call close (fd)

	call sfree (sp)
end


# FXF_CHECK_DUP_EXTN_VER --- Function to check for a duplicate EXTNAME or
# EXTVER in the FITS file open with NEW_COPY mode. The filename specification
# does not have EXTNAME nor EXTVER in the ksection.
# Returns YES if there are duplicates.

int procedure fxf_check_dup_extnv (im, group)

pointer im			#I image descriptor
int	group			#O extension number where there is a duplicate

int	cindx
pointer extn, extv, sp, hdrfile, fit, poff
int	fxf_extnv_error()
bool    streq()

include "fxfcache.com"

begin
        call smark (sp)
	call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)

	call fpathname (IM_HDRFILE(im), Memc[hdrfile], SZ_PATHNAME)
	fit = IM_KDES(im)
	
	do cindx=1, rf_cachesize {
            if (rf_fit[cindx] == NULL)
	        next

	    if (streq (Memc[hdrfile], rf_fname[1,cindx])) {
	        extn = rf_pextn[cindx]
	        extv = rf_pextv[cindx]
	        poff = rf_pixp[cindx]      # pixel offset -1 if EOF
	        group = 1

	        # Now compare the input image FIT_EXT(NAME,VER) with
	        # the cache values of the NEW_COPY images.

	        while (Memc[extn+LEN_CARD*group] != EOS || 
		    !IS_INDEFL(Memi[extv+group]) || Memi[poff+group] != -1) {
		    if (fxf_extnv_error (fit, group, extn, extv) == YES) {
		        call sfree (sp)
		        if (FKS_OVERWRITE(fit) == YES)
		            return (NO)
	                else
		            return (YES)
		    } else
		        group = group + 1
	        } 
	    }
	}

	call sfree (sp)
	return (NO)
end


# FXF_CHECK_OLD_NAME -- Check is the filename is already in cache for a
# NEWIMAGE == YES mode; if so, make the entry obsolete.

procedure fxf_check_old_name (im)

pointer im				#I image descriptor

int	cindx
pointer sp, hdrfile, fit
bool    streq()

include "fxfcache.com"

begin
        call smark (sp)
	call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)

	call fpathname (IM_HDRFILE(im), Memc[hdrfile], SZ_PATHNAME)

	fit = IM_KDES(im)
	do cindx=1, rf_cachesize {
            if (rf_fit[cindx] == NULL)
	        next

	    # Verify that we have the correct file.
	    if (streq (Memc[hdrfile], rf_fname[1,cindx])) {
                call mfree (rf_pextv[cindx], TY_INT)
                call mfree (rf_pextn[cindx], TY_CHAR)
                call mfree (rf_pixp[cindx], TY_INT)
                call mfree (rf_hdrp[cindx], TY_INT)
                call mfree (rf_fit[cindx], TY_STRUCT)
                call mfree (rf_hdr[cindx], TY_CHAR)
	        rf_fit[cindx] = NULL
	        rf_mtime[cindx] = 0      # invalidate cache entry
	        rf_fname[1,cindx] = EOS
	        break
            }		
	}

	call sfree (sp)
end


# FXF_REBLOCK -- If the user area is not blocked to fixed length records, e.g.,
# as is possible in a new copy image, reblock it fixed length.

procedure fxf_reblock (im)

pointer	im				#I image descriptor

pointer	sp, lbuf, op, ua
int	fd, spool, nlines, nchars, sz_userarea, len_hdrmem
errchk	stropen, open, getline, putline, realloc, seek, fcopyo
int	open(), stropen(), getline()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	ua = IM_USERAREA(im)
	fd = stropen (Memc[ua], ARB, READ_ONLY)
	spool = open ("rb_spool", READ_WRITE, SPOOL_FILE)
	
	# Reblock into a spool file, counting the lines.
	for (nlines=0;  ;  nlines=nlines+1) {
	    nchars = getline (fd, Memc[lbuf])
	    if (nchars <= 0)
		break

	    for (op=nchars;  op <= LEN_CARD;  op=op+1)
		Memc[lbuf+op-1] = ' '
	    Memc[lbuf+LEN_CARD] = '\n'
	    Memc[lbuf+LEN_CARD+1] = EOS
	    call putline (spool, Memc[lbuf])
	}

	call close (fd)

	# Reallocate header the right size.
	sz_userarea = nlines * (LEN_CARD+1) + SZ_EXTRASPACE

	IM_HDRLEN(im) = LEN_IMHDR +
	    (sz_userarea - SZ_EXTRASPACE + SZ_MII_INT-1) / SZ_MII_INT
	len_hdrmem = LEN_IMHDR +
	    (sz_userarea+1 + SZ_MII_INT-1) / SZ_MII_INT

	if (IM_LENHDRMEM(im) < len_hdrmem) {
	    IM_LENHDRMEM(im) = len_hdrmem
	    call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	}

	# Move spooled data back to user area.
	ua = IM_USERAREA(im)
	fd = stropen (Memc[ua], sz_userarea, NEW_FILE)
	call seek (spool, BOFL)
	call fcopyo (spool, fd)

	IM_UABLOCKED(im) = YES
	call close (fd)
	call close (spool)
	call sfree (sp)
end


# FXF_FCLOBBER -- Clobber an existing FITS file.  We use the environment
# variable 'clobber' rather than 'imclobber' because is a file and not
# an image.

procedure fxf_fclobber (file)

char	file			#I input filename to delete

int	cindx
bool    streq()
include	"fxfcache.com"

begin
	iferr (call delete (file))
	    call filerr (file, SYS_FCANTCLOB)
				     
	# Remove the name from the cache.
	do cindx=1, rf_cachesize {
	    if (rf_fit[cindx] == NULL)
		next

	    # Verify that we have the correct file.
	    if (streq (file, rf_fname[1,cindx])) {
		if (rf_fit[cindx] != NULL) {
		    call mfree (rf_pextv[cindx], TY_INT)
		    call mfree (rf_pextn[cindx], TY_CHAR)
		    call mfree (rf_pixp[cindx], TY_INT)
		    call mfree (rf_hdrp[cindx], TY_INT)
		    call mfree (rf_fit[cindx], TY_STRUCT)
		    call mfree (rf_hdr[cindx], TY_CHAR)
                    rf_fit[cindx] = NULL
		}
	    }
	}
end


# FXF_ACCESS -- Check if a file section is necessary to access any
# particular extension.

procedure fxf_gaccess (im, fsec)

pointer	im		#I image descriptor
bool	fsec		#I true if extname,extver or group have values

bool	mef
int	acmode, fit, newimage, group
bool    envgetb(), fnullfile()
errchk	syserr, syserrs, fxf_fclobber

begin
	fit = IM_KDES(im)
	acmode = FIT_ACMODE(fit) 
	newimage = FIT_NEWIMAGE(fit)

	if (acmode == READ_ONLY || acmode == READ_WRITE) {
	    # If no file section then see if it is a MEF by prereading an
	    # extension.

	    if (!fsec) {
		group = 1
		mef = false
		ifnoerr (call fxf_prhdr (im, group))
		    mef = true
		else {
		    # Flag error if the group does not exist and overwrite+.
		    if (FKS_OVERWRITE(fit) == YES)
		        call syserrs (SYS_FXFEXTNF, IM_NAME(im))
		}
		# Multi-extension file but no extension was specified.
		if (mef)
		    call syserrs (SYS_FXFOPNOEXTNV, IM_NAME(im))
		FIT_GROUP(fit) = 0
		FIT_XTENSION(fit) = NO
	    }
	}
	
	switch (acmode) {
	case NEW_COPY, NEW_IMAGE, APPEND:
	    if (envgetb ("imclobber")) {
		if (newimage == NO) {
	            if (FKS_APPEND(fit) != YES && FKS_OVERWRITE(fit) != YES) {
		        # Clobber the file.
		        call fxf_fclobber (IM_HDRFILE(im))
		        FIT_NEWIMAGE(fit) = YES
		    }
		}
	    } else {
	        if (newimage == NO)
		    if (FKS_APPEND(fit) != YES && FKS_OVERWRITE(fit) != YES) {
			if (!fnullfile (IM_HDRFILE(im)))
			    call syserrs (SYS_IKICLOB, IM_HDRFILE(im))
		    }
	   }
	default:
	    ;
	}

end


# FXF_CHECK_GROUP -- Check for group specification from fkinit, ksection
# and cluster index are equal when specifified and they are also compatible
# when (extname,extver) is in the kernel sections.

procedure fxf_check_group (im, ksection, acmode, group, ksinh)

pointer	im			#I imio descriptor
char	ksection[ARB]		#I kernel section
int	acmode			#I fits unit extension mode
int	group			#U extension number in the image section
int	ksinh			#O INHERIT value from the filename ksection

pointer sp, ks, fit
bool    fks_extn_or_ver, inherit_override	
int	igroup, kgroup, fgroup, tgroup, sv_inherit, newimage, append
bool    fnullfile()
int	envgets()

errchk  syserrs, fxf_ks_error

begin
	call smark (sp)
	call salloc (ks, SZ_LINE, TY_CHAR)

	fit = IM_KDES(im)
	newimage = FIT_NEWIMAGE(fit)

	# Set the FKINIT defaults; these override the builtin defaults.
	fgroup = -1
	igroup = -1

	FKS_APPEND(fit) = NO_KEYW
	if (envgets (ENV_FKINIT, Memc[ks], SZ_LINE) != 0)
	    call fxf_ksection (Memc[ks], fit, igroup)

	append = FKS_APPEND(fit)

	sv_inherit = FKS_INHERIT(fit)
	FKS_INHERIT(fit) = NO_KEYW
	FKS_APPEND(fit) = NO_KEYW

	# Parse the kernel section.
	call fxf_ksection (ksection, fit, kgroup)
	ksinh = FKS_INHERIT(fit)

	# Check for various error conditions.
	if (FKS_OVERWRITE(fit) == YES && FKS_APPEND(fit) == YES)
	    call syserrs (SYS_FXFKSNOVR, "append")

	if (append == NO_KEYW && FKS_APPEND(fit) == NO_KEYW)
	    FKS_APPEND(fit) = NO
	else if (append != NO_KEYW)
	    FKS_APPEND(fit) = append

	if (append == YES && FKS_OVERWRITE(fit) == YES)
	    FKS_APPEND(fit) = NO

	if (group != -1) {
	    if (kgroup != -1 && group != kgroup)
	        call syserrs (SYS_FXFKSBADGR, IM_NAME(im))
	    else if (igroup != -1 && group != igroup)
	        call syserrs (SYS_FXFKSBADFKIG, IM_NAME(im))
	    fgroup = group
	} else if (kgroup != -1) {
	    if (group != -1 && group != kgroup)
	        call syserrs (SYS_FXFKSBADGR, IM_NAME(im))
	    else if (igroup != -1 && group != igroup)
	        call syserrs (SYS_FXFKSBADFKIG, IM_NAME(im))
	    fgroup = kgroup
	} else if (igroup != -1) {
	    if ((group != -1 && group != igroup) ||
		    (kgroup != -1 && kgroup != igroup))
	        call syserrs (SYS_FXFKSBADFKIG, IM_NAME(im))
	    fgroup = igroup
        }	       
	group = fgroup      

	# Pre-read the data header.  This is done after processing the user
	# ksection as we need to get the extname/extver if any.
	# EXTNAME or EXTVER has priority when defined over group.

	fks_extn_or_ver =
	    (FKS_EXTNAME(fit) != EOS || !IS_INDEFL(FKS_EXTVER(fit)))

	tgroup = fgroup
	if (fks_extn_or_ver) 
	    tgroup = -1

	if (newimage == NO && !fnullfile (IM_HDRFILE(im))) {
	    iferr (call fxf_prhdr (im, tgroup)) {
		# If group does not exist and over+, it is an error.
		if (FKS_OVERWRITE(fit) == YES)
		    call syserrs (SYS_FXFEXTNF, IM_NAME(im))
	        else
		    call erract (EA_ERROR)
	    }   
	}

	if (fgroup != -1 && tgroup != fgroup && fks_extn_or_ver)
	    call syserrs (SYS_FXFKSBADEXN, IM_NAME(im))

	if (fgroup == -1 && fks_extn_or_ver)
	    group = tgroup

	FIT_EXPAND(fit) = FKS_EXPAND(fit)

	# For overwrite we need to force group to be the kernel section
	# extension number. 

	if (FKS_OVERWRITE(fit) == YES)
	    FIT_GROUP(fit) = max(kgroup,group)
	else
	    FIT_GROUP(fit) = group

	if (FKS_APPEND(fit) == YES)
	    FIT_GROUP(fit) = -1

	# See if there are some error conditions with the ksection.
	call fxf_ks_errors (fit, acmode)

	# Check to see if the user ksection sets the inherit flag.  If so
	# this overrides all the defaults, including the data header.

	inherit_override = (FKS_INHERIT(fit) != NO_KEYW)
	if (!inherit_override)
	    FKS_INHERIT(fit) = sv_inherit

	# A data header has precedence over the more global fkinit.
	# If inherit is disabled in the data header don't enable it here.

	if (!inherit_override && FIT_INHERIT(fit) == NO)
	    FKS_INHERIT(fit) = NO

	call sfree (sp)
end


# FXF_CLEAN_PL -- Filter PLIO keywords from the UA.

procedure fxf_clean_pl (im)

pointer	im	#I image descriptor

begin
	#### (This is incredibly inefficient...)
	call fxf_filter_keyw (im, "TFORM1")
	call fxf_filter_keyw (im, "TFIELDS")
	call fxf_filter_keyw (im, "ZIMAGE")
	call fxf_filter_keyw (im, "ZCMPTYPE")
	call fxf_filter_keyw (im, "ZBITPIX")
	call fxf_filter_keyw (im, "ZNAXIS")
	call fxf_filter_keyw (im, "ZNAXIS1")
	call fxf_filter_keyw (im, "ZNAXIS2")
	call fxf_filter_keyw (im, "ZTILE1")
	call fxf_filter_keyw (im, "ZTILE2")
	call fxf_filter_keyw (im, "ZNAME1")
	call fxf_filter_keyw (im, "ZVAL1")
end
