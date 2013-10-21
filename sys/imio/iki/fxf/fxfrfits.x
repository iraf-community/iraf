# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include <syserr.h>
include <time.h>
include <ctype.h>
include <imhdr.h>
include <imio.h>
include <finfo.h>
include <fset.h>
include <mach.h>
include <imset.h>
include <error.h>
include "fxf.h"

# FXFRFITS.X -- Routines to load FITS header in memory and set up the cache
# mechanism.

define	LEN_UACARD_100	 8100
define	LEN_UACARD_5	 405


# FXF_RFITSHDR -- Procedure to read one or more FITS header while caching
# the primary header, set the FITS memory structure for each
# filename, the header and pixel offset from the beginning
# and the EXTNAME and EXTVER value for each extension.

procedure fxf_rfitshdr (im, group, poff)

pointer im		#I image descriptor
int	group		#I Group number to read
int	poff		#O char offset the the pixel area in the FITS image

long	fi[LEN_FINFO]
pointer hoff,totpix, extn, extv
pointer sp, fit, o_fit, lbuf, hdrfile, hdr
int	cindx, cfit, user, fitslen, offs_count
int	in, spool, slot, i, nrec1440, acmode

bool	initialized, reload, extname_or_ver, ext_append
data	initialized /false/
int	rf_refcount

bool	streq()
long	cputime(), fstatl()

int	finfo(), open(), stropen(), getline()

errchk	putline, syserrs, seek, calloc, realloc, syserr
errchk	fpathname, calloc, fxf_load_header, fxf_skip_xtn, fxf_read_xtn

include "fxfcache.com"

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)

	# Initialize the header file cache on the first call.  The kernel
	# doesn't appear to work with the cache completely deactivated, so
	# the minimum cachesize is 1.

	if (!initialized) {
	    rf_refcount = 0
	    do i = 1, MAX_CACHE
		rf_fit[i] = 0
	    rf_cachesize = max(1, min(MAX_CACHE, FKS_CACHESIZE(IM_KDES(im))))
	    initialized = true
	} else
	    rf_refcount = rf_refcount + 1

	o_fit = IM_KDES(im)
	reload = false
	slot = 1
	# Get file system info on the desired header file.
	call fpathname (IM_HDRFILE(im), Memc[hdrfile], SZ_PATHNAME)

	if (finfo (Memc[hdrfile], fi) == ERR) 
	    call syserrs (SYS_FOPEN, IM_HDRFILE(im))

        acmode = FIT_ACMODE(o_fit)
	ext_append = (acmode == NEW_IMAGE || acmode == NEW_COPY)
	repeat {
	    # Search the header file cache for the named image.
	    do cindx = 1, rf_cachesize {
		if (rf_fit[cindx] == NULL) {
		   slot = cindx
		   next
		}
		if (streq (Memc[hdrfile], rf_fname[1,cindx])) {
		    # File is in cache; is cached entry still valid?
		    # If we are appending extension, do not reload from
		    # disk.

		    if (FI_MTIME(fi) != rf_mtime[cindx] && !ext_append) {
			# File modify date has changed, reuse slot.
			slot = cindx
			break
		    } 
		    
		    # For every non-empty file the fxf_open() call
		    # pre reads every PHU, so that when the fxf_rdhdr()
		    # comes, the cache entry is already here.

		    # Return the cached header.
		    rf_lru[cindx] = rf_refcount
		    cfit = rf_fit[cindx]
		    FIT_XTENSION(cfit) = FIT_XTENSION(o_fit)
		    FIT_ACMODE(cfit) = FIT_ACMODE(o_fit)
		    FIT_EXPAND(cfit) = FIT_EXPAND(o_fit)

		    # Load Extend value from cache header entry to
		    # the current fit struct entry.

		    FIT_EXTEND(o_fit) = FIT_EXTEND(cfit)

		    call amovi (FIT_ACMODE(cfit), FIT_ACMODE(o_fit),
			LEN_FITBASE)
		    hoff = rf_hdrp[cindx]
		    poff = rf_pixp[cindx]
		    extn = rf_pextn[cindx]
		    extv = rf_pextv[cindx]
		    FIT_GROUP(o_fit) = group
		    FIT_HDRPTR(o_fit) = hoff
		    FIT_PIXPTR(o_fit) = poff

		    extname_or_ver = (FKS_EXTNAME(o_fit) != EOS ||
			!IS_INDEFL (FKS_EXTVER(o_fit)))

		    # If the group number or extname_or_ver has not been
		    # specified we need to load the group number where there
		    # is data i.e., FIT_NAXIS != 0. The 'cfit' structure would
		    # have this group number.

		    if (group == -1 && !extname_or_ver) {
			if (FIT_GROUP(cfit) != -1) {
			    group = FIT_GROUP(cfit)
			    FIT_GROUP(o_fit) = group

			} else if (FIT_NAXIS(cfit) != 0) {
			    # See if the main FITS unit has data when
			    # group = -1 is specified.

			    group = 0
			    FIT_GROUP(cfit) = 0
			    FIT_GROUP(o_fit) = 0
			}	
		    }

		    # The main header has already been read at this point,
		    # now merge with UA.

		    if (group == 0) {
			hdr = rf_hdr[cindx]
			fitslen = rf_fitslen[cindx]
			FIT_EXTEND(o_fit) = FIT_EXTEND(cfit)
			call fxf_merge_w_ua (im, hdr, fitslen)

		    } else {
			# Read intermediate xtension headers if not in
			# hoff and poff yet.
			offs_count = FIT_NUMOFFS(cfit)
			call fxf_read_xtn (im,
			    cfit, group, hoff, poff, extn, extv)
		    } 

		    # IM_CTIME takes the value of the DATE keyword
		    if (IM_CTIME(im)==0) {
			IM_CTIME(im) = FI_CTIME(fi)
		    }

		    # FIT_MTIME takes the value of keyword IRAF-TLM.
		    # If not present use the mtime from the finfo value.

		    if (FIT_MTIME(cfit) == 0) {
			FIT_MTIME(cfit) = FI_MTIME(fi)
		    }

		    # Invalidate entry if cache is disabled.
		    if (rf_cachesize <= 0)
			rf_time[cindx] = 0

		    call sfree (sp)
		    return				# IN CACHE

		} else {
		    # Keep track of least recently used slot.
		    if (rf_lru[cindx] < rf_lru[slot])
			slot = cindx
		}
	    }

	    # Either the image header is not in the cache, or the cached
	    # entry is invalid.	 Prepare the given cache slot and read the
	    # header into it.

	    # Free old save buffer and descriptor.
	    if (rf_fit[slot] != NULL) {
		call mfree (rf_pextv[slot], TY_INT)
		call mfree (rf_pextn[slot], TY_CHAR)
		call mfree (rf_pixp[slot], TY_INT)
		call mfree (rf_hdrp[slot], TY_INT)
		call mfree (rf_fit[slot], TY_STRUCT)
		call mfree (rf_hdr[slot], TY_CHAR)
		rf_fit[slot] = NULL
		rf_lru[slot] = 0
		rf_fname[1,slot] = EOS
	    }

	    # Allocate a spool file for the FITS data.
	    spool = open ("spool", NEW_FILE, SPOOL_FILE)

	    # Allocate cache version of FITS descriptor.
	    call calloc (fit, LEN_FITBASE, TY_STRUCT)
	    call calloc (hoff, MAX_OFFSETS, TY_INT) 
	    call calloc (poff, MAX_OFFSETS, TY_INT) 
	    call calloc (extn, MAX_OFFSETS*LEN_CARD, TY_CHAR) 
	    call calloc (extv, MAX_OFFSETS, TY_INT) 

	    # Initialize the entries.
	    call amovki (INDEFL, Memi[extv], MAX_OFFSETS)
	    call aclrc (Memc[extn], MAX_OFFSETS)
	    call amovki (-1, Memi[poff], MAX_OFFSETS)

	    FIT_GROUP(fit) = -1
	    FIT_HDRPTR(fit) = hoff
	    FIT_PIXPTR(fit) = poff
	    FIT_NUMOFFS(fit) = MAX_OFFSETS
	    FIT_ACMODE(fit) = FIT_ACMODE(o_fit)
	    FIT_BSCALE(fit) = 1.0d0
	    FIT_BZERO(fit) = 0.0d0
	    FIT_XTENSION(fit) = NO
	    FIT_EXTNAME(fit) = EOS
	    FIT_EXTVER(fit) = INDEFL
	    FIT_EXTEND(fit) = -3

	    # Initialize the cache entry.
	    call strcpy (Memc[hdrfile], rf_fname[1,slot], SZ_PATHNAME)
	    rf_fit[slot] = fit
	    rf_hdrp[slot] = hoff
	    rf_pixp[slot] = poff
	    rf_pextn[slot] = extn
	    rf_pextv[slot] = extv
	    rf_lru[slot] = rf_refcount
	    rf_mtime[slot] = FI_MTIME(fi)

	    if (!reload)
		rf_time[slot] = cputime (0)

	    reload = true

	    in = IM_HFD(im)
	    call seek (in, BOFL)
	    
	    # Read main FITS header and copy to spool fd.
	    FIT_IM(fit) = im
	    call amovki (1, FIT_LENAXIS(fit,1), IM_MAXDIM)

	    call fxf_load_header (in, fit, spool, nrec1440, totpix)


	    # Record group 0 (PHU) as having just been read.
	    FIT_GROUP(fit) = 0

	    call seek (spool, BOFL)
	    fitslen = fstatl (spool, F_FILESIZE)

	    # Prepare cache area to store the FITS header.
	    call calloc (hdr, fitslen, TY_CHAR)
	    user = stropen (Memc[hdr], fitslen, NEW_FILE)
	    rf_hdr[slot] = hdr
	    rf_fitslen[slot] = fitslen
	    FIT_CACHEHDR(fit) = hdr
	    FIT_CACHEHLEN(fit) = fitslen

	    # Append the saved FITS cards to saved cache.
	    while (getline (spool, Memc[lbuf]) != EOF) 
		call putline (user, Memc[lbuf])

	    call close (user)
	    call close (spool)
	    
	    # Group 0 (i.e. Main Fits unit)
	    Memi[hoff] = 1	       # beginning of primary h.u.
	    Memi[poff] = nrec1440 + 1  # first pixel data of main u.

	    # Set group 1 offsets.
	    Memi[hoff+1] = Memi[poff] + totpix
	    Memi[poff+1] = -1
	}

        call sfree (sp)
end


# FXF_READ_XTN -- Procedure to read a FITS extension header and at the same
# time make sure that the EXTNAME and EXTVER values are not repeated 
# with those in the cache.

procedure fxf_read_xtn (im, cfit, igroup, hoff, poff, extn, extv)

pointer im			#I Image descriptor
pointer cfit			#I Cached FITS descriptor
int	igroup			#I Group number to process
pointer hoff			#I Pointer to header offsets array
pointer poff			#I Pointer to pixel offsets array
pointer extn			#I Pointer to extname's array
pointer extv			#I Pointer to extver's array

char	messg[SZ_LINE]
pointer lfit, sp, po, ln
int	spool, ig, acmode, i
int	fitslen, xtn_hd, nrec1440, totpix, in, group
int	strcmp(), getline()
long	offset, fstatl()
int	open(), fxf_extnv_error()
bool	ext_append, get_group

errchk	fxf_load_header, fxf_skip_xtn, syserr, syserrs
define	rxtn_ 91

begin
	# Allocate a spool file for the FITS header.
	spool = open ("FITSHDRX", READ_WRITE, SPOOL_FILE)

	lfit = IM_KDES(im)
	group = FIT_GROUP(lfit)
	acmode = FIT_ACMODE(lfit)
	ext_append = (acmode == NEW_IMAGE || acmode == NEW_COPY)

	# If we have 'overwrite' in the ksection then look for the
	# existent extname/extver we want to overwrite since we don't
	# want to append.

	if (FKS_OVERWRITE(lfit) == YES)
	    ext_append = false

	# See if we want to look at an extension given the EXT(NAME,VER)
	# field in the ksection.

	if (FKS_EXTNAME(lfit) != EOS || !IS_INDEFL (FKS_EXTVER(lfit))) {
	    ig = 1
	    repeat {
		call fseti (spool, F_CANCEL, YES)
		xtn_hd = NO

		# Has last extension header been read?
		if (Memi[poff+ig] <= 0) { 
		    iferr {
			call fxf_skip_xtn (im,
			    ig, cfit, hoff, poff, extn, extv, spool)
			xtn_hd = YES
		    } then {
			if (ext_append) {
			    # We have reach the end of extensions.
			    FIT_GROUP(lfit) = -1    # message for fxf_updhdr
			    return
			} else {
			    call fxf_form_messg (lfit, messg)
			    call syserrs (SYS_FXFRFNEXTNV, messg)
			}
		    } else {	
			# If we want to append an extension then.
			if (ext_append && FKS_DUPNAME(lfit) == NO)
			    if (fxf_extnv_error (lfit, ig, extn, extv) == YES) {
				call fxf_form_messg (lfit, messg)
				call syserrs (SYS_FXFOPEXTNV, messg)
			    } 
		    }
		}

		if (fxf_extnv_error (lfit, ig, extn, extv) == YES) {
		    # We have matched either or both FKS_EXTNAME and FKS_EXTVER
		    # with the values in the cache.

		    if (ext_append && FKS_DUPNAME(lfit) == NO)	{
			call fxf_form_messg (lfit, messg)
			call syserrs (SYS_FXFOPEXTNV, messg)
		    } 
		    group = ig
		    FIT_GROUP(lfit) = ig
		    goto rxtn_

		} else {
		    ig = ig + 1
		    next
		}
	    }

	} else {
	    # No extname or extver specified.
	    # Read through the Extensions until group number is reached;
	    # if no number is supplied, read until EOF to load header and
	    # pixel offsets necessary to append and extension.
	    
	    if (igroup == -1 && FIT_GROUP(cfit) == -1)
		group = MAX_INT

	    # We are trying to get the first group that meets these condition.
	    get_group = (FIT_GROUP(cfit) == -1 && igroup == -1) 

	    do ig = 0, group  {
		xtn_hd = NO

		# Has last extension header been read?
		if (Memi[poff+ig] <= 0 ) {
		    call fseti (spool, F_CANCEL, YES)
		    iferr {
			call fxf_skip_xtn (im,
			    ig, cfit, hoff, poff, extn, extv, spool)
			xtn_hd = YES
		    } then {
			if (ext_append) {
			    # We have reach the end of extensions.
			    FIT_GROUP(lfit) = -1    # message for fxf_updhdr
			    return
			} else {
			    call syserrs (SYS_FXFRFEOF, IM_NAME(im))
			    return
			}
		    }

		    # Mark the first group that contains an image
		    # i.e. naxis != 0. 

		    if (FIT_NAXIS(lfit) != 0 && 
			    strcmp ("IMAGE", FIT_EXTTYPE(lfit)) == 0) {
			if (get_group) {
			    FIT_GROUP(cfit) = ig   # save on cache fits struct
			    FIT_GROUP(lfit) = ig   # also on current
			    break
			} else if (FIT_GROUP(cfit) <= 0)
			    FIT_GROUP(cfit) = ig 
		    }
	       }
	    }
	}
rxtn_
	if (xtn_hd == NO) {
	    in = IM_HFD(im)
	    offset = Memi[hoff+group]
	    call seek (in, offset)
	    FIT_IM(lfit) = im
	    call fseti (spool, F_CANCEL, YES)
	    call fxf_load_header (in, lfit, spool, nrec1440, totpix)
	}

	# If requested a non supported BINTABLE format, post an error
	# message and return to the caller.

	if (strcmp(FIT_EXTTYPE(lfit), "BINTABLE") == 0) {
	    if (strcmp(FIT_EXTSTYPE(lfit), "PLIO_1") != 0) {
	        call close (spool)
		call syserrs (SYS_IKIEXTN, IM_NAME(im))
            }
	}
	
	# Merge Image Extension header to the user area.
	fitslen = fstatl (spool, F_FILESIZE)

	# Copy the spool array into a static array. We cannot reliable
	# get the pointer from the spool file.
	call smark (sp)
	call salloc (ln, LEN_UACARD, TY_CHAR)

	if (po != NULL)
	    call mfree(po, TY_CHAR)
	call calloc (po, fitslen+1, TY_CHAR)

	i = po
	call seek (spool, BOFL)
	while (getline (spool, Memc[ln]) != EOF) {

	    call amovc (Memc[ln], Memc[i], LEN_UACARD)
	    i = i + LEN_UACARD
	}
	Memc[i] = EOS

	# Make the user aware that they cannot use inheritance
	# if the PDU contains a data array.

	if (Memi[poff] != Memi[hoff+1]) {
	    if (FKS_INHERIT(lfit) == YES) {
		call syserr (SYS_FXFBADINH)
	    }
	} else {
	    # Disable inheritance if PHDU has a DU.
	    if (Memi[poff+0] != Memi[hoff+1])
		FIT_INHERIT(lfit) = NO
	}

	# Reset the value of FIT_INHERIT if FKS_INHERIT is set
	if (FKS_INHERIT(lfit) != NO_KEYW)
	    FIT_INHERIT(lfit) = FKS_INHERIT(lfit)

	if (FIT_TFIELDS(lfit) > 0) {
	    fitslen = fitslen + FIT_TFIELDS(lfit)*LEN_UACARD
	    call realloc (po, fitslen, TY_CHAR)
        }

	call fxf_merge_w_ua (im, po, fitslen)

	call mfree (po, TY_CHAR)

	call sfree (sp)
	call close (spool)
end


# FXF_EXTNV_ERROR -- Integer boolean function (YES,NO) to find out if the
# value of kernel section parameter FKS_EXTNAME and FKS_EXTVER are not
# repeated in the cache pointed by extn and extv.

int procedure fxf_extnv_error (fit, ig, extn, extv)

pointer fit			#I fit descriptor
int	ig			#I extension number
pointer extn, extv		#I pointers to arrays for extname and extver 

bool	bxtn, bxtv, bval, bxtn_eq, bxtv_eq
int	fxf_strcmp_lwr()

begin
	bxtn = (FKS_EXTNAME(fit) != EOS)
	bxtv = (!IS_INDEFL (FKS_EXTVER(fit)))

	if (bxtn)
	    bxtn_eq =
		(fxf_strcmp_lwr(FKS_EXTNAME(fit), Memc[extn+LEN_CARD*ig]) == 0)
	if (bxtv)
	    bxtv_eq = (FKS_EXTVER(fit) == Memi[extv+ig])
	
	if (bxtn && bxtv) {
	    # Since both FKS_EXTNAME and FKS_EXTVER are defined, see if they
	    # repeated in the cache.

	    bval = (bxtn_eq && bxtv_eq)

	} else if (bxtn && !bxtv) {
	    # We have a duplicated in this case when extver in the image
	    # header is INDEFL.

	    bval = bxtn_eq

	} else if (!bxtn && bxtv) {
	    # If the FKS_EXTNAME is not defined (i.e. EOS) and the FKS_EXTVER
	    # value is the same as the cached, then we have a match.

	    bval = bxtv_eq 

	} else
	    bval = false
	
	if (bval)
	    return (YES)
	else
	    return (NO)
end


# FXF_SKIP_XTN -- Skip over a FITS extension.  The procedure will read the
# current extension header and calculates the respectives offset for later
# usage.

procedure fxf_skip_xtn (im, group, cfit, hoff, poff, extn, extv, spool)

pointer im			#I image descriptor
int	group			#I groupheader number to read
pointer cfit			#I cached fits descriptor
pointer hoff			#I extension header offset
pointer poff			#I extension data offset
pointer extn			#I points to the array of extname
pointer extv			#I points to the arrays of extver

pointer sp, lfit, fit, hdrfile
bool    streq()
int	spool, in, nrec1440, totpix, i, k, cindx
long	offset
errchk	fxf_load_header
int	strcmp()

include "fxfcache.com"

begin
	call smark (sp)
	call salloc (lfit, LEN_FITBASE, TY_STRUCT)
        call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)

	call seek (spool, BOFL)
	fit = IM_KDES(im)

	# Allocate more memory for offsets in case we are pass MAX_OFFSETS.
	if (group >= FIT_NUMOFFS(cfit)) {
	    FIT_NUMOFFS(cfit) = FIT_NUMOFFS(cfit) + MAX_OFFSETS 
	    call realloc (hoff, FIT_NUMOFFS(cfit), TY_INT)
	    call realloc (poff, FIT_NUMOFFS(cfit), TY_INT)
	    call realloc (extn, FIT_NUMOFFS(cfit)*LEN_CARD, TY_CHAR)
	    call realloc (extv, FIT_NUMOFFS(cfit), TY_INT)

	    offset = FIT_NUMOFFS(cfit) - MAX_OFFSETS
	    call amovki (INDEFL, Memi[extv+offset], MAX_OFFSETS)
	    call amovki (-1, Memi[poff+offset], MAX_OFFSETS)

	    do i = 0,  MAX_OFFSETS-1  {
		k = (offset+i)*LEN_CARD
		Memc[extn+k] = EOS
	    }
	 
	    # Update the fits structure with the new pointer values
	    call fpathname (IM_HDRFILE(im), Memc[hdrfile], SZ_PATHNAME)
	    fit = IM_KDES(im)
	    do cindx = 1, rf_cachesize {
	        if (rf_fit[cindx] == NULL)
	            next
	        if (streq (Memc[hdrfile], rf_fname[1,cindx])) {
	            rf_pextn[cindx] = extn
		    rf_pextv[cindx] = extv
		    rf_hdrp[cindx] = hoff
		    rf_pixp[cindx] = poff
		    FIT_HDRPTR(fit) = hoff
		    FIT_PIXPTR(fit) = poff
	        }
	    }
	}
		
	in = IM_HFD(im)
	offset = Memi[hoff+group]

	call seek (in, offset)
	lfit = IM_KDES(im)
	FIT_IM(lfit) = im
	call fxf_load_header (in, lfit, spool, nrec1440, totpix)

	# Record the first group that has NAXIS !=0 and is an IMAGE.
	if (FIT_GROUP(cfit) == -1) {
	    if (FIT_NAXIS(lfit) != 0 &&
		    strcmp ("IMAGE", FIT_EXTTYPE(lfit)) == 0)
		FIT_GROUP(cfit) = group
	}

	Memi[poff+group] = Memi[hoff+group] + nrec1440
	# The offset for the beginning of next group.
	Memi[hoff+group+1] = Memi[poff+group] + totpix

	# Mark next group pixel offset in case we are at EOF.
	Memi[poff+group+1] = -1
	call strcpy (FIT_EXTNAME(lfit), Memc[extn+LEN_CARD*group], LEN_CARD) 
	Memi[extv+group] = FIT_EXTVER(lfit)

	call sfree (sp)
end


# FXF_LOAD_HEADER -- Load a FITS header from a file descriptor into a
# spool file.

procedure fxf_load_header (in, fit, spool, nrec1440, datalen)

int	in			#I input FITS header file descriptor
pointer fit			#I FITS descriptor
int	spool			#I spool output file descriptor
int	nrec1440		#O number of 1440 char records output
int	datalen			#O length of data area in chars

int	ncols
pointer lbuf, sp, im, stime, fb, ttp
int	totpix, nchars, nbytes, index, ncards, simple, i, pcount, junk
int	fxf_read_card(), fxf_ctype(), ctoi(), strsearch()
bool	fxf_fpl_equald()
errchk	syserr, syserrs

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (stime, LEN_CARD, TY_CHAR)
	call salloc (fb, FITS_BLOCK_BYTES, TY_CHAR)

	FIT_BSCALE(fit) = 1.0d0
	FIT_BZERO(fit) = 0.0d0
	FIT_EXTNAME(fit) = EOS
	FIT_EXTVER(fit) = INDEFL
	im = FIT_IM(fit)

	# Read successive lines of the FITS header. 
	nrec1440 = 0
	pcount = 0
	ncards = 0

	repeat {
	    # Get the next input line.
	    nchars = fxf_read_card (in, Memc[fb], Memc[lbuf], ncards)
	    if (nchars == EOF) {
		call close (spool)
		call syserrs (SYS_FXFRFEOF, IM_NAME(im))
            }
	    ncards = ncards + 1

	    # A FITS header card already has 80 chars, just add the newline.
	    Memc[lbuf+LEN_CARD] = '\n'
	    Memc[lbuf+LEN_CARD+1] = EOS

	    # Process the header card.
	    switch (fxf_ctype (Memc[lbuf], index)) {
	    case KW_END:
		nrec1440 = FITS_LEN_CHAR(ncards*40)
		break
	    case KW_SIMPLE:
		call strcpy ("SIMPLE", FIT_EXTTYPE(fit), SZ_EXTTYPE)
		call fxf_getb (Memc[lbuf], simple)
		FIT_EXTEND(fit) = NO_KEYW
		if (simple == NO)
		    call syserr (SYS_FXFRFSIMPLE)
	    case KW_EXTEND:
		call putline (spool, Memc[lbuf])
		call fxf_getb (Memc[lbuf], FIT_EXTEND(fit)) 
	    case KW_XTENSION:
		FIT_XTENSION(fit) = YES 
		call fxf_gstr (Memc[lbuf], FIT_EXTTYPE(fit), SZ_EXTTYPE)
	    case KW_EXTNAME:
		call fxf_gstr (Memc[lbuf], FIT_EXTNAME(fit), LEN_CARD)
		call putline (spool, Memc[lbuf])
	    case KW_EXTVER:
		call fxf_geti (Memc[lbuf], FIT_EXTVER(fit))
		call putline (spool, Memc[lbuf])
	    case KW_ZCMPTYPE:
		call fxf_gstr (Memc[lbuf], FIT_EXTSTYPE(fit), SZ_EXTTYPE)
	    case KW_PCOUNT:
		call fxf_geti (Memc[lbuf], pcount)
		call putline (spool, Memc[lbuf])
		FIT_PCOUNT(fit) = pcount
	    case KW_INHERIT:
		call fxf_getb (Memc[lbuf], FIT_INHERIT(fit))
		call putline (spool, Memc[lbuf])
	    case KW_BITPIX:
		call fxf_geti (Memc[lbuf], FIT_BITPIX(fit))
	    case KW_DATATYPE:
		call fxf_gstr (Memc[lbuf], FIT_DATATYPE(fit), SZ_DATATYPE)
	    case KW_NAXIS:
		if (index == 0) {
		    call fxf_geti (Memc[lbuf], FIT_NAXIS(fit))
		    if (FIT_NAXIS(fit) < 0 )
			call syserr (SYS_FXFRFBNAXIS)
	  	} else
		    call fxf_geti (Memc[lbuf], FIT_LENAXIS(fit,index))
	    case KW_BSCALE:
		call fxf_getd (Memc[lbuf], FIT_BSCALE(fit))
		# If BSCALE is like 1.00000011 reset to 1.0.
		if (fxf_fpl_equald (1.0d0, FIT_BSCALE(fit), 4))
		    FIT_BSCALE(fit) = 1.0d0
		call putline (spool, Memc[lbuf])
	    case KW_BZERO:
		call fxf_getd (Memc[lbuf], FIT_BZERO(fit))
		# If BZERO is like 0.00000011 reset to 0.0.
		if (fxf_fpl_equald (0.0d0, FIT_BZERO(fit), 4))
		    FIT_BZERO(fit) = 0.0d0
		call putline (spool, Memc[lbuf])
	    case KW_DATAMAX:
		call fxf_getr (Memc[lbuf], FIT_MAX(fit))
		call putline (spool, Memc[lbuf])
	    case KW_DATAMIN:
		call fxf_getr (Memc[lbuf], FIT_MIN(fit))
		call putline (spool, Memc[lbuf])
	    case KW_TFIELDS:
		# Allocate space for TFORM and TTYPE keyword values
		call fxf_geti (Memc[lbuf], ncols)
		FIT_TFIELDS(fit) = ncols
		if (FIT_TFORMP(fit) != NULL) {
		    call mfree (FIT_TFORMP(fit), TY_CHAR)
		    call mfree (FIT_TTYPEP(fit), TY_CHAR)
	        }
		call calloc (FIT_TFORMP(fit), ncols*LEN_FORMAT, TY_CHAR)
		call calloc (FIT_TTYPEP(fit), ncols*LEN_OBJECT, TY_CHAR)
	    case KW_TFORM:
		call fxf_gstr (Memc[lbuf], Memc[stime], LEN_CARD)
		if (index == 1) {
		    # PLMAXLEN is used to indicate the maximum line list
		    # length for PLIO masks in bintables.  The syntax
		    # "PI(maxlen)" is used in bintables to pass the max
		    # vararray length for a column.

		    i = strsearch (Memc[stime], "PI(") 
		    if (i > 0)
			junk = ctoi (Memc[stime], i, FIT_PLMAXLEN(fit))
		}
	    case KW_TTYPE:
		ttp = FIT_TTYPEP(fit) + (index-1)*LEN_OBJECT
		call fxf_gstr (Memc[lbuf], Memc[ttp], LEN_CARD)
	    case KW_OBJECT:
		# Since only OBJECT can go into the header and IM_TITLE has its
		# values as well, we need to save both to see which one has
		# changed at closing time.

		call fxf_gstr (Memc[lbuf], FIT_OBJECT(fit), LEN_CARD)
		if (FIT_OBJECT(fit) == EOS)
		    call strcpy ("        ", FIT_OBJECT(fit), SZ_KEYWORD)
		call strcpy (FIT_OBJECT(fit), FIT_TITLE(fit), LEN_CARD)
		call strcpy (FIT_OBJECT(fit), IM_TITLE(im), LEN_CARD)
		call putline (spool, Memc[lbuf])
	    case KW_IRAFTLM:
		call fxf_gstr (Memc[lbuf], Memc[stime], LEN_CARD)
		call fxf_date2limtime (Memc[stime], FIT_MTIME(fit))
		call putline (spool, Memc[lbuf])
	    case KW_DATE:
		call fxf_gstr (Memc[lbuf], Memc[stime], LEN_CARD)
		call fxf_date2limtime (Memc[stime], IM_CTIME(im))
		call putline (spool, Memc[lbuf])
	    default:
		call putline (spool, Memc[lbuf])
	    }
	}

	# Calculate the length of the data area of the current extension,
	# measured in SPP chars and rounded up to an integral number of FITS
	# logical blocks.

	if (FIT_NAXIS(fit) != 0) {
	    totpix =  FIT_LENAXIS(fit,1)
	    do i = 2, FIT_NAXIS(fit)
		totpix = totpix * FIT_LENAXIS(fit,i)

	    # Compute the size of the data area (pixel matrix plus PCOUNT)
	    # in bytes.  Be careful not to overflow a 32 bit integer.

	    nbytes = (totpix + pcount) * (abs(FIT_BITPIX(fit)) / NBITS_BYTE)

	    # Round up to fill the final 2880 byte FITS logical block.
	    nbytes = ((nbytes + 2880-1) / 2880) * 2880

	    datalen = nbytes / SZB_CHAR

	} else
	    datalen = 0

	call sfree (sp)
end


# FXF_MERGE_W_UA --  Merge a spool user area with the im_userarea.

procedure fxf_merge_w_ua (im, userh, fitslen) 

pointer im		#I image descriptor
int	userh		#I pointer to user area spool
int	fitslen		#I length in chars of the user area

bool	inherit
pointer sp, lbuf, ua, ln
int	elen, elines, nbl, i, k
int	sz_userarea, merge, len_hdrmem, fit, clines, ulines
bool	fxf_is_blank()
int	strlen()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (ln, LEN_UACARD, TY_CHAR)

	fit = IM_KDES(im)

	# FIT_INHERIT has the logically combined value of the fkinit inherit's
	# value, if any; the ksection value, if any and the INHERIT value in
	# the extension header.

	inherit = (FIT_INHERIT(fit) == YES)
	inherit = (inherit && (FIT_GROUP(fit) != 0))

	# Reallocate the image descriptor to allow space for the spooled user
	# FITS cards plus a little extra for new parameters.

	sz_userarea = fitslen + SZ_EXTRASPACE
	# Add size of main header if necessary.
	if (inherit)
	    sz_userarea = sz_userarea + FIT_CACHEHLEN(fit)

	IM_HDRLEN(im) = LEN_IMHDR +
	    (sz_userarea - SZ_EXTRASPACE + SZ_MII_INT-1) / SZ_MII_INT
	len_hdrmem = LEN_IMHDR +
	    (sz_userarea+1 + SZ_MII_INT-1) / SZ_MII_INT

	if (IM_LENHDRMEM(im) < len_hdrmem) {
	    IM_LENHDRMEM(im) = len_hdrmem
	    call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	}


	# Copy the extension header to the USERAREA if not inherit or copy
	# the global header plus the extension header if inherit is set.

	if (fitslen > 0) {
	    ua = IM_USERAREA(im)
	    elen = fitslen

	    if (inherit) {
		# First, copy those cards in the global header that 
		# are not in the current extension header.

		clines = strlen (Memc[FIT_CACHEHDR(fit)])
		ulines = strlen (Memc[userh])
		clines = clines / LEN_UACARD
		ulines = ulines / LEN_UACARD
		merge = YES
		call fxf_match_str (FIT_CACHEHDR(fit),
		    clines, userh, ulines, merge, ua)
		elen = LEN_UACARD * ulines
	    }

	    # Append the extension header to the UA.
	    elines = elen / LEN_UACARD
	    k = userh
	    nbl = 0

	    do i = 1, elines {
		call strcpy (Memc[k], Memc[ln], LEN_UACARD)
		if (fxf_is_blank (Memc[ln]))
		    nbl = nbl + 1
		else {
		    # If there are blank cards, add them.
		    if (nbl > 0)
			call fxf_blank_lines (nbl, ua)
		    call amovc (Memc[ln], Memc[ua], LEN_UACARD)
		    ua = ua + LEN_UACARD
		}
		k = k + LEN_UACARD
	    }

	    Memc[ua] = EOS
	}	
	call sfree (sp)
end


# FXF_STRCMP_LWR -- Compare 2 strings in lower case mode.

int procedure fxf_strcmp_lwr (s1, s2)

char s1[ARB], s2[ARB]		#I strings to be compare for equality

int	istat
pointer sp, l1, l2
int	strcmp()

begin
	call smark (sp)
	call salloc (l1, LEN_CARD, TY_CHAR)
	call salloc (l2, LEN_CARD, TY_CHAR)

	call strcpy (s1, Memc[l1], LEN_CARD)
	call strcpy (s2, Memc[l2], LEN_CARD)
	call strlwr(Memc[l1])
	call strlwr(Memc[l2])
	istat = strcmp (Memc[l1], Memc[l2])

	call sfree (sp)
	return (istat)
end


# FXF_DATE2LIMTIME -- Convert the IRAF_TLM string (used to record the IMIO
# time of last modification of the image) into a long integer limtime
# compatible with routine cnvtime().  The year must be 1980 or later.
# The input date string has one of the following forms:
#
# Old format:			"hh:mm:ss (dd/mm/yyyy)"
# New (Y2K/ISO) format:		"YYYY-MM-DDThh:mm:ss

procedure fxf_date2limtime (datestr, limtime)

char	datestr[ARB]	#I fixed format date string
long	limtime		#O output limtime (LST seconds from 1980.0)

double	dsec
int	hours,minutes,seconds,day,month,year
int     status, iso, flags, ip, m, d, y
int	dtm_decode_hms(), btoi(), ctoi()
long    gmttolst()
double  jd

begin
	iso = btoi (datestr[3] != ':')
	status = OK

	if (iso == YES) {
	    status = dtm_decode_hms (datestr,
		year,month,day, hours,minutes,dsec, flags)

	    # If the decoded date string is old style FITS then the HMS
	    # values are indefinite, and we need to set them to zero.

	    if (and(flags,TF_OLDFITS) != 0) {
		hours = 0
		minutes = 0
		seconds = 0
	    } else {
 		if (IS_INDEFD(dsec)) {
                    hours   = 0
                    minutes = 0
                    seconds = 0
		} else
                    seconds = nint(dsec)
            }
	} else {
	    ip = 1;  ip = ctoi (datestr,     ip, hours)
	    ip = 1;  ip = ctoi (datestr[4],  ip, minutes)
	    ip = 1;  ip = ctoi (datestr[7],  ip, seconds)
	    ip = 1;  ip = ctoi (datestr[11], ip, day)
	    ip = 1;  ip = ctoi (datestr[14], ip, month)
	    ip = 1;  ip = ctoi (datestr[17], ip, year)
	}

	if (status == ERR || year < 1980) {
	    limtime = 0
	    return
	}

	seconds = seconds + minutes * 60 + hours * 3600

        # Calculate the Julian day from jan 1, 1980. Algorithm taken
	# from astutil/asttools/asttimes.x.

        y = year
	if (month > 2)
	    m = month + 1
	else {
	    m = month + 13
	    y = y - 1
	}

	# Original: jd = int (JYEAR * y) + int (30.6001 * m) + day + 1720995
        # -723244.5 is the number of days to add to get 'jd' from jan 1, 1980.

	jd = int (365.25 * y) + int (30.6001 * m) + day - 723244.5
	if (day + 31 * (m + 12 * y) >= 588829) {
	    d = int (y / 100)
	    m = int (y / 400)
	    jd = jd + 2 - d + m
	}
	jd = jd - 0.5 
        day = jd

	limtime = seconds + day * 86400
	if (iso == YES)
	    limtime = gmttolst (limtime)
end


# FIT_MATCH_STR -- FITS header pattern matching algorithm.  Match mostly one
# line of lenght LEN_UACARD with the buffer pointed by str; if pattern is not
# in str, put it in the 'out' buffer.

procedure fxf_match_str (pat, plines, str, slines, merge, po)

pointer pat		#I buffer with pattern
int	plines		#I number of pattern
pointer str		#I string to compare the pattern with
int	slines		#I number of lines in str
int	merge		#I flag to indicate merging or unmerge
pointer po		#I matching pattern accumulation pointer

char	line[LEN_UACARD]
pointer sp, pt, tpt, tst, ps, pkp
int	nbl, l, k, j, i, strncmp(), nbkw, nsb, cmplen
int	stridxs()

begin
	call smark (sp)
	call salloc (tpt, LEN_UACARD_100+1, TY_CHAR)
	call salloc (tst, LEN_UACARD_5+1, TY_CHAR)

	Memc[tpt] = EOS
	Memc[tst] = EOS

	# The temporary buffer is non blank only when it has a blank
	# keyword following by a comentary:

	#1) '	       '		   / Comment to the block of keyw
	#2) KEYWORD = Value

	nbl = 0
	nbkw = 0
	pt = pat - LEN_UACARD

	for (k=1;  k <= plines;  k=k+1) {
	    pt = pt + LEN_UACARD
	    call strcpy (Memc[pt], line, LEN_UACARD)

	    # Do not pass these keywords if merging.
	    if (merge == YES) {
		if (strncmp (line, "COMMENT ", 8) == 0 ||
		    strncmp (line, "HISTORY ", 8) == 0 ||
		    strncmp (line, "OBJECT  ", 8) == 0 ||
		    strncmp (line, "EXTEND  ", 8) == 0 ||
		    strncmp (line, "ORIGIN  ", 8) == 0 ||
		    strncmp (line, "IRAF-TLM", 8) == 0 ||
		    strncmp (line, "DATE    ", 8) == 0 ) {

		    next 
	        }
	    }	
	    if (line[1] == ' ') {
	        call fxf_accum_bufp (line, tpt, nbkw, nbl)
	        next
	    }

	    if (Memc[tpt] != EOS) {  
		nbkw = nbkw + 1
	        call strcat (line, Memc[tpt], LEN_UACARD_100)
	        Memc[tst] = EOS

	        # Now that we have a buffer with upto 100 lines, we take its 
	        # last 5 card and we are going to compare it with upto 5 
	        # lines (that can contain blank lines in between).

	        pkp = tpt + LEN_UACARD*(nbkw-1)
	        ps = str - LEN_UACARD
	        nsb = 0

		do j = 1, slines {
		    ps = ps + LEN_UACARD
		    call strcpy (Memc[ps], line, LEN_UACARD)

		    if (line[1] == ' ') {
			call fxf_accum_buft (line, tst, nsb)
			next

		    } else if (Memc[tst] != EOS) {
			nsb = nsb + 1
			call strcat (line, Memc[tst], LEN_UACARD_5)

			# To begin compare the first character in the
			# keyword line.

			if (Memc[pkp] == line[1]) {
			    if (strncmp (Memc[pkp-LEN_UACARD*(nsb-1)],
				    Memc[tst], LEN_UACARD*nsb) == 0) {
				nsb = 0
				break
			    }
			}	  

			nsb = 0
			Memc[tst] = EOS
		    }
		}

	        if (j == slines+1) {
		    if (nbl > 0)
		        call fxf_blank_lines (nbl, po)
		    i = tpt
		    do l = 1, min(100, nbkw) {
		        call amovc (Memc[i], Memc[po], LEN_UACARD)
		        i = i + LEN_UACARD
		        po = po + LEN_UACARD
		    }
	        } else {
		    pt = pt - LEN_UACARD	# push back last line
		    k = k - 1
	        }

	        Memc[tpt] = EOS	
	        nbkw = 0
	        nbl = 0	

	    } else {
		# One line to compare.
		ps = str - LEN_UACARD
		cmplen = min (stridxs("=", Memc[pt]), LEN_UACARD)
		if (cmplen == 0)
		    cmplen = LEN_UACARD

#	        if (merge == YES)
#		    cmplen = SZ_KEYWORD

	        do j = 1, slines {
		    ps = ps + LEN_UACARD
		    if (Memc[ps] == Memc[pt]) {
			if (merge == NO)
			    cmplen = LEN_CARD 
		        if (strncmp (Memc[ps], Memc[pt], cmplen) == 0) {
			    nbl = 0
			    break
			}
		    }   
	        }

	        if (j == slines+1) {
		    if (nbl > 0)
		        call fxf_blank_lines (nbl, po)

		    call amovc (line, Memc[po], LEN_UACARD)
		    po = po + LEN_UACARD
		    nbl = 0
	        } 
	    }
	}

	call sfree (sp)
end


# FXF_ACCUM_BUFP -- Accumulate blank keyword cards (No keyword and a / card
# only) and the blank lines in between.

procedure fxf_accum_bufp (line, tpt, nbkw, nbl)

char	line[LEN_UACARD]	#I input card from the pattern buffer
pointer tpt			#I pointer to the buffer
int	nbkw			#U number of blank keyword card
int	nbl			#U number of blank card before the 1st bkw

char	keyw[SZ_KEYWORD]		
bool	fxf_is_blank()	

begin
	call strcpy (line, keyw, SZ_KEYWORD)

	if (fxf_is_blank (line)) {
	    # Accumulate blank cards in between bkw cards.
	    if (nbkw > 0 && nbkw < 100) {
		call strcat (line, Memc[tpt], LEN_UACARD_100)
		nbkw = nbkw + 1
	    } else if (nbkw >= 100) {
		nbkw = nbkw - 1
	    } else
		nbl = nbl + 1

	} else if (fxf_is_blank (keyw)) {
	    nbkw = nbkw + 1

	    # We have a blank keyword, but the card is not blank, maybe it is
	    # a '/ comment' card.  Start accumulating upto 100 blank kwy lines.

	    if (nbkw < 100)
		call strcat (line, Memc[tpt], LEN_UACARD_100)
	    else
		nbkw = nbkw - 1
	}
end


# FXF_ACCUM_BUFT -- Accumulate blank keyword keeping track of the blank cards.

procedure fxf_accum_buft (line, tst, nsb)

char	line[LEN_UACARD]	#I input card from the target buffer
pointer tst			#I pointer to output buffer
int	nsb			#U number of consecutives blank cards

char	keyw[SZ_KEYWORD]
bool	fxf_is_blank()	

begin
	call strcpy (line, keyw, SZ_KEYWORD)

	if (fxf_is_blank (line)) {
	    if (nsb > 0 && nsb < 5) {
		call strcat (line, Memc[tst], LEN_UACARD_5)
		nsb = nsb + 1
	    } else if (nsb > 4)
		nsb = nsb - 1 
	} else if (fxf_is_blank (keyw)) {
	    # We want to pick the last blank kwy only.
	    call strcpy (line, Memc[tst], LEN_UACARD_5)
	    nsb = 1
	}
end


# FXF_BLANK_LINES -- Write a number of blank lines into output buffer.

procedure fxf_blank_lines (nbl, po)

int	nbl			#U number of blank lines to write
pointer po			#I output buffer pointer

char	blk[1]
int	i

begin
	blk[1] = ' '
	do i = 1, nbl {
	    call amovkc (blk[1], Memc[po], LEN_UACARD)
	    po = po +  LEN_UACARD
	    Memc[po-1] = '\n'
	}
	nbl = 0
end


# FXF_IS_BLANK -- Determine is the string is blank.

bool procedure fxf_is_blank (line)

char line[ARB]			#I input string
int  i

begin
	for (i=1; IS_WHITE(line[i]); i=i+1)
	   ;

	if (line[i] == NULL || line[i] == '\n')
	    return (true)
	else
	    return (false)
end


# FXF_FORM_MESSG -- Form string from extname, extver.

procedure fxf_form_messg (fit, messg)

pointer fit			#I fits descriptor
char	messg[ARB]		#O string

begin
	if (!IS_INDEFL (FKS_EXTVER(fit))) {
	    call sprintf (messg, LEN_CARD, "'%s,%d'")
		call pargstr (FKS_EXTNAME(fit))
		call pargi (FKS_EXTVER(fit)) 
	} else {
	    call sprintf (messg, LEN_CARD, "'%s'")
		call pargstr (FKS_EXTNAME(fit))
	}			
end
