# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<syserr.h>
include <error.h>
include	<imhdr.h>
include	<imio.h>
include <finfo.h>
include	<fio.h>
include	<fset.h>
include <mii.h>
include <time.h>
include <mach.h>
include	"fxf.h"

# FXFUPDHDR.X -- Routines to update the header of an image extension on
# disk.

define  SZ_DATE       10
define  SZ_TIMEDATE   21


# FXF_UPDHDR -- Update the FITS header file.  This is done by writing an
# entire new header file and then replacing the old header file with the
# new one.  This is necessary since the header file is a text file and text
# files cannot be randomly updated.

procedure fxf_updhdr (im, status)

pointer	im			#I image descriptor
int	status			#O return status

pointer	sp, fit, mii
pointer	outname, fits_file, tempfile
bool    adjust_header, overwrite, append
int	nchars_ua, hdr_fd, group, hdr_off, size, npad, grp_pix_off
int	acmode, junk, in_fd, diff, hdr_acmode, in_off, nchars
int	read(), fxf_hdr_offset(), strncmp(), access()
int	open(), fnroot(), fstatl(), fnldir()
bool	fnullfile()

errchk  open, read, write, fxf_header_diff, fxf_write_header, fxf_make_adj_copy
errchk  set_cache_time, syserr, syserrs, imerr

begin
	call smark (sp)
	call salloc (mii, FITS_BLOCK_CHARS, TY_INT)
	call salloc (fits_file, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_PATHNAME, TY_CHAR)
	call salloc (tempfile, max(SZ_PATHNAME,SZ_FNAME*2), TY_CHAR)

	acmode = IM_ACMODE(im)
	fit = IM_KDES(im)
	status = OK

	# For all intents and purposes the APPEND access mode is the same
	# as NEW_IMAGE under the FK.  Let's simplify the code as the user
	# has requested APPEND.

        if (acmode == APPEND)
	    acmode = NEW_IMAGE

	if (acmode == READ_ONLY)
	    call imerr (IM_NAME(im), SYS_IMUPIMHDR)

	if (fnullfile (IM_HDRFILE(im))) {
	    call sfree (sp)
	    return
	}

	group = FIT_GROUP(fit)

	if (FIT_EXTTYPE(fit) != EOS && group != -1) {
	    if (strncmp (FIT_EXTTYPE(fit), "IMAGE", 5) != 0 &&
		    strncmp (FIT_EXTTYPE(fit), "SIMPLE", 6) != 0) {
	       call syserr (SYS_FXFUPHBEXTN)
	    }
	}

	if (FKS_OVERWRITE(fit) == YES) {
	    if (group == 0) {
		# We are overwriting the main unit.
	        FIT_NEWIMAGE(fit) = YES
	    }

	    group = -1
	    acmode = NEW_IMAGE

	    if (IM_PFD(im) == NULL) 
	        call fxf_overwrite_unit (fit, im, status)

	    call strcpy (IM_PIXFILE(im), Memc[fits_file], SZ_FNAME)

	} else
	    call strcpy (IM_HDRFILE(im), Memc[fits_file], SZ_FNAME)
	
	# Calculate the header offset corresponding to group number 'group'. 
	hdr_off = fxf_hdr_offset (group, fit, IM_PFD(im), acmode)

	# If the pixfile has not been created, open new one.  This could
	# happen if the don't write any pixels to the data portion of the file.

	if (IM_PFD(im) == NULL && (acmode == NEW_COPY || acmode == NEW_IMAGE)) {
	    FIT_NAXIS(fit) = 0
	    if (FIT_NEWIMAGE(fit) == YES)
	        hdr_acmode = NEW_FILE
	    else {
	        # We want to append a new extension with no data.
	        hdr_acmode = READ_WRITE
	    }
	} else {
	    call close(IM_PFD(im))	
	    hdr_acmode = READ_WRITE
	}

	append = (acmode == NEW_IMAGE || acmode == NEW_COPY)

	# Calculate header difference. The difference between the original
	# header length at open time and now. The user could have added or
	# deleted header keywords.

	call fxf_header_diff (im, group, acmode, hdr_off, diff, nchars_ua)

	# Adjust header only when we need to expand. We fill with trailing 
	# blanks in case diff .gt. 0. (Reduce header size).

	adjust_header = (diff < 0)
	if (adjust_header && FIT_EXPAND(fit) == NO) {
	    call syserr (SYS_FXFUPHEXP)
	    adjust_header = false
	}

	overwrite = ((diff < 0 || diff >= FITS_BLOCK_CHARS) &&
	    FKS_OVERWRITE(fit) == YES)

	if (adjust_header || overwrite) { 
	    # We need to change the size of header portion in the middle of
	    # the file. The best thing to do is to make a copy in the output
	    # filename directory.

	    nchars = fnldir (IM_PIXFILE(im), Memc[tempfile], SZ_FNAME)
	    junk = fnroot (IM_PIXFILE(im), Memc[tempfile+nchars], SZ_FNAME)
	    call mktemp (Memc[tempfile], Memc[outname], SZ_PATHNAME)
	    call strcat (".fits", Memc[outname], SZ_PATHNAME)
	    in_fd = open (Memc[fits_file], READ_ONLY, BINARY_FILE)
	    hdr_fd = open (Memc[outname], NEW_FILE, BINARY_FILE)

	    if (append)
	        grp_pix_off = FIT_PIXOFF(fit)
	    else
	        grp_pix_off = Memi[FIT_PIXPTR(fit)+group]

	    call fxf_make_adj_copy (in_fd, hdr_fd,
		hdr_off, grp_pix_off, nchars_ua)

	    # Do not write any trailing blanks in write_header().
	    diff = 0

	} else {
	    hdr_fd = open (Memc[fits_file], hdr_acmode, BINARY_FILE)
	    IM_PFD(im) = NULL
	    IM_HFD(im) = NULL
	}

	if (FIT_NEWIMAGE(fit) == YES)
	    call seek (hdr_fd, BOF)
	else if (hdr_off != 0)
	    call seek (hdr_fd, hdr_off)
	   
	if (acmode == NEW_COPY)
	    call fxf_setbitpix (im, fit)

	# Now write default cards and im_userarea to disk.
	call fxf_write_header (im, fit, hdr_fd, diff)

 	size = fstatl(hdr_fd,F_FILESIZE)
	npad = FITS_BLOCK_CHARS - mod(size,FITS_BLOCK_CHARS)

	# If we are appending a new extension, we need to write padding to
	# 2880 bytes blocks at the end of the file.

	if (mod(npad,FITS_BLOCK_CHARS) > 0 &&
		(FIT_NEWIMAGE(fit) == YES || append)) {

	    call amovki (0, Memi[mii], npad)
	    call flush (hdr_fd)
	    call seek (hdr_fd, EOF)
	    call write (hdr_fd, Memi[mii], npad)
	}
	call flush (hdr_fd)

	# Now open the original file and skip to the beginning of (group+1)
	# to begin copying into hdr_fd. (end of temporary file in tmp$).

	if (FKS_OVERWRITE(fit) == YES) {
	    if (overwrite) {
		call close (in_fd)
		if (access (IM_PIXFILE(im), 0, 0) == YES)
		    call delete (IM_PIXFILE(im))
		call strcpy (Memc[outname], IM_PIXFILE(im), SZ_FNAME)
	    }		

	    call imgcluster (IM_NAME(im), Memc[tempfile], SZ_PATHNAME)
	    in_fd = open (IM_HDRFILE(im), READ_ONLY, BINARY_FILE)
	    group = FIT_GROUP(fit)

	    in_off = Memi[FIT_HDRPTR(fit)+group+1]
	    call seek (hdr_fd, EOF)
	    call seek (in_fd, in_off)
	    size = FITS_BLOCK_CHARS

	    while (read (in_fd, Memi[mii], size) != EOF) 
		call write (hdr_fd, Memi[mii], size)

	    call close (hdr_fd)
	    call close (in_fd)

	    call fxf_ren_tmp (IM_PIXFILE(im),IM_HDRFILE(im))

	    # Change the acmode so we can change the modification and
	    # this way reset the cache for this file.

	    IM_ACMODE(im) = READ_WRITE
	    call fxf_over_delete(im)

	} else {
	    if (adjust_header || overwrite)
	        call close (in_fd)
	    call close (hdr_fd)

	    if (adjust_header) {
	        if (access (IM_PIXFILE(im), 0, 0) == YES)
		    call delete (IM_PIXFILE(im))
		call rename (Memc[outname], IM_PIXFILE(im))
            }	
	}

	# Make sure we reset the modification time for the cached header
	# since we have written a new version. This way the header will
	# be read from disk next time the file is accessed.
  
	if (FIT_GROUP(fit) == 0 || FIT_GROUP(fit) == -1)
	   call fxf_set_cache_time (im)

	# See if we need to add or change the value of EXTEND in the PHU.
	if (FIT_EXTEND(fit) == NO_KEYW || FIT_EXTEND(fit) == NO)
	    call fxf_update_extend (im)

	call sfree (sp)
end


# FXF_HDR_OFFSET -- Function to calculate the header offset for group number
# 'group'.

int procedure fxf_hdr_offset (group, fit, pfd, acmode)

int	group			#I extension number
pointer	fit			#I fits descriptor
pointer	pfd			#I pixel file descriptor
int	acmode			#I image acmode

int	hdr_off

begin
	if (FIT_NEWIMAGE(fit) == YES)
	    return (0)

	# Look for the beginning of the current group.
	if (group == -1) {
	    # We are appending or creating a new FITS IMAGE.
	    hdr_off = FIT_EOFSIZE(fit)
	} else
	    hdr_off = Memi[FIT_HDRPTR(fit)+group]

	# If pixel file descriptor is empty for a newcopy or newimage
	# in an existent image then the header offset is EOF.

	if (pfd == NULL && (acmode == NEW_COPY || acmode == NEW_IMAGE))
	    hdr_off = EOF

	return (hdr_off)
end


# FXF_HEADER_DIFF -- Get the difference between the original header at open
# time and the one at closing time.

procedure fxf_header_diff (im, group, acmode, hdr_off, diff, ualen)

pointer	im			#I image descriptor
int	group			#I extension number
int	acmode			#I emage acmode
int	hdr_off			#I header offset for group
int	diff			#O difference
int	ualen			#O new header length

char	temp[LEN_CARD]
pointer	hoff, poff, sp, pb, tb
int	ua, fit, orig_hdr_size, pixoff, clines, ulines, len
int     merge, usize, excess, nheader_cards, rp, inherit, kmax, kmin
int     strlen(), imaccf(), imgeti(), strcmp(), idb_findrecord()
int	btoi()
bool    imgetb()

errchk  open, fcopyo

begin
	fit = IM_KDES(im)
	inherit = NO

	FIT_INHERIT(fit) = FKS_INHERIT(fit)

	# In READ_WRITE mode get the UA value of INHERIT only if it has
	# change after _open().

	if (acmode == READ_WRITE) {
	    if (imaccf (im, "INHERIT") == YES) {
		inherit = btoi (imgetb (im, "INHERIT"))
		if (inherit != FKS_INHERIT(fit))
		    FIT_INHERIT(fit) = inherit
	    }
	}

	# Allow inheritance only for extensions.
	inherit = FIT_INHERIT(fit)
	if (FIT_GROUP(fit) == 0) {
	    inherit = NO
  	    FIT_INHERIT(fit) = inherit
	}
	# Scale the pixel offset to be zero base rather than the EOF base.
	if (FIT_NEWIMAGE(fit) == NO) {
	    pixoff = FIT_PIXOFF(fit) - FIT_EOFSIZE(fit)
	} else {
	    if ((hdr_off == EOF || hdr_off == 0)&&
		(IM_NDIM(im) == 0 || FIT_NAXIS(fit) == 0)) {
	        diff = 0
	        return
	    }
	    pixoff = FIT_PIXOFF(fit) - 1
	}

	ua = IM_USERAREA(im)

	if (FIT_NEWIMAGE(fit) == NO && inherit == YES) {
	    # Form an extension header by copying cards in the UA that
	    # do not belong in the global header nor in the old
	    # extension header if the image is open READ_WRITE.

	    len = strlen (Memc[ua])
	    ulines = len / LEN_UACARD
	    clines = FIT_CACHEHLEN(fit) / LEN_UACARD

	    call smark (sp)
	    call salloc (tb, len+1, TY_CHAR)

	    # Now select those lines in UA that are not in fit_cache and
	    # put them in 'pb'.

	    pb = tb
	    merge = NO
	    call fxf_match_str (ua, ulines,
		FIT_CACHEHDR(fit), clines, merge, pb)
	    Memc[pb] = EOS
	    ualen = strlen (Memc[tb])
	    call sfree (sp)

	    # Now copy the buffer pointed by 'pb' to UA.
	    call strcpy (Memc[tb], Memc[ua], ualen)
	}

	# See also fitopix.x for an explanation of this call.
	call fxf_mandatory_cards (im, nheader_cards)

	kmax = idb_findrecord (im, "DATAMAX", rp)
	kmin = idb_findrecord (im, "DATAMIN", rp)

	if (IM_LIMTIME(im) < IM_MTIME(im)) {
	    # Keywords should not be in the UA.
	    if (kmax > 0)
	        call imdelf (im, "DATAMAX")
	    if (kmin > 0)
	        call imdelf (im, "DATAMIN")

	} else {
	    # Now update the keywords.  If they are not in the UA we need
	    # to increase the number of mandatory cards.

	    if (kmax == 0)
		nheader_cards = nheader_cards + 1
	    if (kmin == 0)
		nheader_cards = nheader_cards + 1
        }	     

	# Determine if OBJECT or IM_TITLE have changed. IM_TITLE has
	# priority.

	if (strcmp (IM_TITLE(im), FIT_TITLE(fit)) != 0)
	    call strcpy (IM_TITLE(im), FIT_OBJECT(fit), LEN_CARD)
	else {
	    iferr (call imgstr(im, "OBJECT", temp, LEN_CARD))
	        temp[1] = EOS
	    if (strcmp (FIT_OBJECT(fit), temp) != 0)
	        call strcpy (temp, FIT_OBJECT(fit), LEN_CARD)
	}

	# If there is no OBJECT keyword, don't create one.
	if (FIT_OBJECT(fit) == EOS)
	    nheader_cards = nheader_cards - 1

	# Too many mandatory cards if we are using the PHU in READ_WRITE mode.
	# Because fxf_mandatory_cards gets call with FIT_NEWIMAGE set to NO,
	# i.e. an extension. (12-9=3)

	if (FIT_XTENSION(fit) == NO && FIT_NEWIMAGE(fit) == NO)
	    nheader_cards = nheader_cards - 3

	if (FIT_NEWIMAGE(fit) == NO && FIT_XTENSION(fit) == YES) {

	    # Now take EXTNAME and EXTVER keywords off the UA if they are in
	    # there. The reason being they can be out of order.

	    iferr (call imgstr (im, "EXTNAME", FIT_EXTNAME(fit), LEN_CARD)) {
		FIT_EXTNAME(fit) = EOS
		if (FKS_EXTNAME(fit) != EOS) {
		   call strcpy (FKS_EXTNAME(fit), FIT_EXTNAME(fit), LEN_CARD)
		} else {
		   # We will not create EXTNAME keyword in the output header
		   nheader_cards = nheader_cards - 1
		}
	    } else {
		call imdelf (im, "EXTNAME")
		nheader_cards = nheader_cards + 1
	    }

	    if (imaccf (im, "EXTVER") == YES) {
		FIT_EXTVER(fit) = imgeti (im, "EXTVER")
		call imdelf (im, "EXTVER")
		nheader_cards = nheader_cards + 1
	    }
	    if (imaccf (im, "PCOUNT") == YES) {
		call imdelf (im, "PCOUNT")
		nheader_cards = nheader_cards + 1
	    }
	    if (imaccf (im, "GCOUNT") == YES) {
		call imdelf (im, "GCOUNT")
		nheader_cards = nheader_cards + 1
	    }
	 
	    if (IS_INDEFL(FIT_EXTVER(fit)) && !IS_INDEFL(FKS_EXTVER(fit)))
		FIT_EXTVER(fit) = FKS_EXTVER(fit)
	}

	usize = strlen (Memc[ua]) 
	ualen = (usize / LEN_UACARD + nheader_cards) * LEN_CARD
	ualen = FITS_LEN_CHAR(ualen / 2)

	# Have we go over the FITS header area already allocated?
	if (acmode == READ_WRITE || acmode == WRITE_ONLY) {
	    hoff = FIT_HDRPTR(fit)
	    poff = FIT_PIXPTR(fit)
	    orig_hdr_size = Memi[poff+group] - Memi[hoff+group]
	    diff = orig_hdr_size - ualen
	} else if ((hdr_off == EOF || hdr_off == 0) && 
	    (IM_NDIM(im) == 0 || FIT_NAXIS(fit) == 0)) {
	    diff = 0
	} else
	    diff = pixoff - ualen

        if (diff < 0 && FIT_EXPAND(fit) == NO) {
	    # We need to reduce the size of the UA becuase we are not
	    # going to expand the header.

	    excess = mod (nheader_cards * 81 + usize, 1458)
	    excess = excess + (((-diff-1400)/1440)*1458)
	    Memc[ua+usize-excess] = EOS
	}
end


# FXF_WRITE_HDR -- Procedure to write header unit onto the PHU or EHU.

procedure fxf_write_header (im, fit, hdr_fd, diff)

pointer	 im		  	#I image structure
pointer	 fit     		#I fits structure
int	 hdr_fd  		#I FITS header file descriptor
int	 diff			#I header size difference opix -> wrhdr time

pointer	sp, spp, mii, rp, uap
char	temp[SZ_FNAME] 
char    card[LEN_CARD], blank, keyword[SZ_KEYWORD], datestr[SZ_DATE] 
int	imaccf(), strlen(), fxf_ua_card(), idb_findrecord()
int	n, i, sz_rec, up, nblanks, acmode, tm[LEN_TMSTRUCT], nbk
bool	xtension, ext_append
bool	fxf_fpl_equald()
errchk  write 

begin
	call smark (sp)
	call salloc (spp, 2880, TY_CHAR)
	call salloc (mii, 1440, TY_INT)

	# Write out the standard, reserved header parameters.
	n = spp
	blank = ' '
	acmode = FIT_ACMODE(fit)
	ext_append = ((acmode == NEW_IMAGE || acmode == NEW_COPY) &&
	    (FKS_EXTNAME(fit) != EOS || !IS_INDEFL (FKS_EXTVER(fit))))

	xtension = (FIT_XTENSION(fit) == YES)
	if (FIT_NEWIMAGE(fit) == YES)
	    xtension = false

	if (xtension)
	    call fxf_akwc ("XTENSION", "IMAGE", 5, "Image extension", n)
	else
	    call fxf_akwb ("SIMPLE", YES, "Fits standard", n)

	if (FIT_NAXIS(fit) == 0 || FIT_BITPIX(fit) == 0) 
	    call fxf_setbitpix (im, fit)

	call fxf_akwi ("BITPIX", FIT_BITPIX(fit), "Bits per pixel", n)
	call fxf_akwi ("NAXIS", FIT_NAXIS(fit), "Number of axes", n)

	do i = 1, FIT_NAXIS(fit) {
	    call fxf_encode_axis ("NAXIS", keyword, i)
	    call fxf_akwi (keyword, FIT_LENAXIS(fit,i), "Axis length", n)
	}

	if (xtension) {
	    call fxf_akwi ("PCOUNT", 0, "No 'random' parameters", n)
	    call fxf_akwi ("GCOUNT", 1, "Only one group", n)
	} else {
	    if (imaccf (im, "EXTEND") == NO)
	        call fxf_akwb ("EXTEND", NO, "File may contain extensions", n)
	    FIT_EXTEND(fit) = YES
	}

	     
	# Do not write BSCALE and BZERO if they have the default 
	# values (1.0, 0.0).

	if (IM_PIXTYPE(im) == TY_USHORT) {
	    call fxf_akwd ("BSCALE", 1.0d0,
		"REAL = TAPE*BSCALE + BZERO", NDEC_REAL, n)
	    call fxf_akwd ("BZERO", 32768.0d0,  "", NDEC_REAL, n)

	} else if (FIT_PIXTYPE(fit) != TY_REAL &&
	    FIT_PIXTYPE(fit) != TY_DOUBLE && IM_ACMODE(im) != NEW_COPY) {

	    # Write the keywords only if they have non_default values.
	    if (!fxf_fpl_equald(1.0d0, FIT_BSCALE(fit), 4) ||
		!fxf_fpl_equald(0.0d0, FIT_BZERO(fit), 4) ) {

	        if (imaccf (im, "BSCALE") == NO) {
		    call fxf_akwd ("BSCALE",  FIT_BSCALE(fit),
			"REAL = TAPE*BSCALE + BZERO", NDEC_REAL, n)
	        }
	        if (imaccf (im, "BZERO") == NO)
		    call fxf_akwd ("BZERO", FIT_BZERO(fit),  "", NDEC_REAL, n)
	    }
	}

	uap = IM_USERAREA(im)

	if (idb_findrecord (im, "ORIGIN", rp) == 0)  {
	    call strcpy (FITS_ORIGIN, temp, LEN_CARD)
	    call fxf_akwc ("ORIGIN",
		temp, strlen(temp), "FITS file originator", n)
	} else if (rp - uap > 10*81) {
	    # Out of place; do not change the value.
	    call imgstr (im, "ORIGIN", temp, LEN_CARD)
	    call fxf_filter_keyw (im, "ORIGIN") 
	    call fxf_akwc ("ORIGIN",
		temp, strlen(temp), "FITS file originator", n)
	}

	if (xtension) {
	    if (FIT_EXTNAME(fit) != EOS) {
	        call strcpy (FIT_EXTNAME(fit), temp, LEN_CARD)
	        call fxf_akwc ("EXTNAME",
		    temp, strlen(temp), "Extension name", n)
	    }
	    if (!IS_INDEFL (FIT_EXTVER(fit))) {
	        call fxf_akwi ("EXTVER",
		    FIT_EXTVER(fit), "Extension version", n)
	    }
	    if (idb_findrecord (im, "INHERIT", rp) > 0) {
		# See if keyword is at the begining of the UA
		if (rp - uap > 11*81) {
		    call fxf_filter_keyw (im, "INHERIT")
		    call fxf_akwb ("INHERIT",
			FIT_INHERIT(fit), "Inherits global header", n)
		} else if (acmode != READ_WRITE)
		   call imputb (im, "INHERIT", FIT_INHERIT(fit))
	    } else
		call fxf_akwb ("INHERIT",
		    FIT_INHERIT(fit), "Inherits global header", n)
	}

	call fxf_encode_date (datestr, SZ_DATE)
	if (idb_findrecord (im, "DATE", rp) == 0) {
	    call fxf_akwc ("DATE",
		datestr, SZ_DATE, "Date FITS file was generated", n)
	} else { 
	    # See if the keyword is out of order.
	    if (rp - uap > 12*81) {  
		call fxf_filter_keyw (im, "DATE")
		call fxf_akwc ("DATE",
		    datestr, SZ_DATE, "Date FITS file was generated", n)
	    } else
	        call impstr (im, "DATE", datestr) 
	}

	call brktime (IM_MTIME(im), tm)
	call sprintf (temp, LEN_CARD, "%02d:%02d:%02d (%02d/%02d/%d)")
	    call pargi (TM_HOUR(tm))
	    call pargi (TM_MIN(tm))
	    call pargi (TM_SEC(tm))
	    call pargi (TM_MDAY(tm))
	    call pargi (TM_MONTH(tm))
	    call pargi (TM_YEAR(tm))

	if (idb_findrecord (im, "IRAF-TLM", rp) == 0) {
	    call fxf_akwc ("IRAF-TLM",
		temp, SZ_TIMEDATE, "Time of last modification", n)
	} else if (rp - uap > 13*81) {  
	    call fxf_filter_keyw (im, "IRAF-TLM")
	    call fxf_akwc ("IRAF-TLM",
		temp, SZ_TIMEDATE, "Time of last modification", n)
	} else 
	    call impstr (im, "IRAF-TLM", temp) 

	# Create DATA(MIN,MAX) keywords only if they have the real 
	# min and max of the data.

	if (IM_LIMTIME(im) >= IM_MTIME(im)) {
	    if (idb_findrecord (im, "DATAMIN", rp) == 0) {
		call fxf_akwr ("DATAMIN",
		    IM_MIN(im), "Minimum data value", NDEC_REAL, n)
	    } else
		call imputr (im, "DATAMIN", IM_MIN(im))

	    if (idb_findrecord (im, "DATAMAX", rp) == 0) {
		call fxf_akwr ("DATAMAX",
		    IM_MAX(im), "Maximum data value",NDEC_REAL, n)
	    } else
		call imputr (im, "DATAMAX", IM_MAX(im))
	}

	if (FIT_OBJECT(fit) != EOS) {
	    if (idb_findrecord (im, "OBJECT", rp) == 0) {
	        call fxf_akwc ("OBJECT", FIT_OBJECT(fit), 
		    strlen (FIT_OBJECT(fit)), "Name of the object observed", n)
	    } else if (rp - uap > 14*81) {  
	        call fxf_filter_keyw (im, "OBJECT")
	        call fxf_akwc ("OBJECT", FIT_OBJECT(fit),
		    strlen (FIT_OBJECT(fit)), "Name of the object observed", n)
	   } else 
	        call impstr (im, "OBJECT", FIT_OBJECT(fit))
	}

	# Write the UA now.
	up = 1
	nbk = 0
	n = n - spp
	sz_rec = 1440

	while (fxf_ua_card (fit, im, up, card) == YES) {
	    call amovc (card, Memc[spp+n], LEN_CARD)
	    n = n + LEN_CARD

	    if (n == 2880) {
		nbk = nbk + 1
		call miipak (Memc[spp], Memi[mii], sz_rec*2, TY_CHAR, MII_BYTE)
		call write (hdr_fd, Memi[mii], sz_rec)
		n = 0
	    }
	}

	# Write the last record.
	nblanks = 2880 - n
	call amovkc (blank, Memc[spp+n], nblanks)
	rp = spp+n+nblanks-LEN_CARD

	# If there are blocks of trailing blanks, write them now.
	if (diff > 0) {
	    if (n > 0) {
		call miipak (Memc[spp], Memi[mii], sz_rec*2, TY_CHAR, MII_BYTE)
		call write (hdr_fd, Memi[mii], sz_rec)
	    }
	    nbk = diff / 1440     

	    do i = 1, nbk-1 {
		call amovkc (blank, Memc[spp], 2880)
		call write (hdr_fd, Memc[spp], sz_rec)
	    }
	    call amovkc (blank, Memc[spp], 2880)
	    rp = spp+2880-LEN_CARD
	}
	   
	call amovc ("END", Memc[rp], 3)
	call miipak (Memc[spp], Memi[mii], sz_rec*2, TY_CHAR, MII_BYTE)
	call write (hdr_fd, Memi[mii], sz_rec)

	call flush (hdr_fd)
	call  sfree (sp)
end


# FXF_UA_CARD  -- Fetch a single line from the user area, trim newlines and
# pad with blanks to size LEN_CARD in order to create an unknown keyword card.
# At present user area information is assumed to be in the form of FITS card
# images, less then or equal to 80 characters and delimited by a newline.

int procedure fxf_ua_card (fit, im, up, card)

pointer fit			#I points to the fits descriptor
pointer	im			#I pointer to the IRAF image
int	up			#I next character in the unknown string
char	card[ARB]		#O FITS card image

char	cval
int	stat, diff
char	chfetch()
int	strmatch()

begin
	if (chfetch (UNKNOWN(im), up, cval) == EOS)
	    return (NO)
	else {
	    up = up - 1
	    stat = NO

	    while (stat == NO)  {
	        diff = up
	        call fxf_make_card (UNKNOWN(im), up, card, 1, LEN_CARD, '\n')
	        diff = up - diff
		if (card[1] == EOS)
		    break

	        if (strmatch (     card, "^GROUPS  ") != 0)
		    stat = NO
	        else if (strmatch (card, "^GCOUNT  ") != 0)
		    stat = NO
	        else if (strmatch (card, "^PCOUNT  ") != 0)
		    stat = NO
	        else if (strmatch (card, "^BLOCKED ") != 0)
		    stat = NO
	        else if (strmatch (card, "^PSIZE   ") != 0)
		    stat = NO
	        else
		    stat = YES
	    }

	    return (stat)
	}
end


# FXF_SETBITPIX -- Set the FIT_BITPIX to the pixel datatype value.

procedure fxf_setbitpix (im, fit)

pointer	im			#I image descriptor 
pointer fit			#I fit descriptor

int	datatype
errchk	syserr, syserrs

begin
	datatype = IM_PIXTYPE(im)

	switch (datatype) {
	case TY_SHORT, TY_USHORT:
	   FIT_BITPIX(fit) = FITS_SHORT
	case TY_INT, TY_LONG:
	   FIT_BITPIX(fit) = FITS_LONG
	case TY_REAL:
	   FIT_BITPIX(fit) = FITS_REAL
	case TY_DOUBLE:
	   FIT_BITPIX(fit) = FITS_DOUBLE
	default:
	   call flush (STDOUT)
	   call syserr (SYS_FXFUPHBTYP)
	}
end


# FXF_MAKE_ADJ_COPY -- Copy a FITS file into a new one, changing the size
# of one header unit.

procedure fxf_make_adj_copy (in_fd, out_fd, hdr_off, pixoff, chars_ua)

int	in_fd		#I input FITS descriptor
int	out_fd		#I output FITS descriptor
int	hdr_off		#I offset to be beginning of the ua to be resized
int	pixoff		#I offset to be pixel area following hdroff
int	chars_ua	#I size of the new UA (user area) in units of chars

pointer	mii, sp
int	nk, nblocks, junk, size_ua
errchk  read, write
int	read()

begin
	call smark (sp)
	call salloc (mii, FITS_BLOCK_CHARS, TY_INT)

	# Number of 1440 chars block up to the beginning of the UA to change.
	nblocks = hdr_off / FITS_BLOCK_CHARS

	# Copy  everything up to hdroff.
	call seek (in_fd, BOF)
	do nk = 1, nblocks {
	    junk = read (in_fd, Memi[mii], FITS_BLOCK_CHARS)
	    call write (out_fd, Memi[mii], FITS_BLOCK_CHARS)
	}

	# Size of the new UA.
	size_ua = FITS_LEN_CHAR(chars_ua)
	nblocks = size_ua / FITS_BLOCK_CHARS
	
	# Put a blank new header in the meantime.
	call amovki( 0, Memi[mii], FITS_BLOCK_CHARS)
	do nk = 1, nblocks
	    call write (out_fd, Memi[mii], FITS_BLOCK_CHARS)

	# Position after the current input header to continue 
	# copying.

	call flush (out_fd)
	call seek (in_fd, pixoff)
	call fcopyo (in_fd, out_fd)
	call flush (out_fd)

	call sfree (sp)
end


# FXF_SET_CACHE_MTIME -- Procedure to reset the modification time on the
# cached entry for the file pointed by 'im'.

procedure fxf_set_cache_time (im)

pointer im			#I image descriptor

pointer sp, hdrfile
long    fi[LEN_FINFO]
int	finfo(), cindx
errchk	syserr, syserrs
bool    streq()

include "fxfcache.com"

begin
	call smark (sp)
	call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)

	call fpathname (IM_HDRFILE(im), Memc[hdrfile], SZ_PATHNAME)
	if (finfo (Memc[hdrfile], fi) == ERR)
	    call syserrs (SYS_FOPEN, IM_HDRFILE(im))

	# Search the header file cache for the named image.
	do cindx = 1, rf_cachesize {
	    if (rf_fit[cindx] == NULL)
	        next

	    if (streq (Memc[hdrfile], rf_fname[1,cindx])) {
		# Reset cache
		if (IM_ACMODE(im) == READ_WRITE) {
		    # Invalidate entry.
		    rf_fname[1,cindx] = EOS
		} else {
		    # While we are appending we want to keep the cache entry
		    # in the slot.
		    rf_mtime[cindx] = FI_MTIME(fi)
		}
		break
	    }
	}

	call sfree (sp)
end	


# FXF_REN_TMP -- Rename a temporary file.

procedure fxf_ren_tmp (in, out)

char 	in[ARB]			#I name of teh temporary file
char    out[ARB]		#O new name

int	junk, protect()

begin
	if (protect (out, QUERY_PROTECTION) == YES) {
	    iferr (junk = protect (out, REMOVE_PROTECTION))
		call erract (EA_ERROR)
	    iferr (junk = protect (in, SET_PROTECTION))
		call erract (EA_ERROR)
	}

	iferr (call delete (out))
	    call erract (EA_ERROR)
	iferr (call rename (in, out))
	    call erract (EA_ERROR)
end


# FXF_OVER_TMP -- Rename an entry from the cache.

procedure fxf_over_delete (im)

pointer im			#I image descriptor

pointer fname, sp
bool	streq()
int	cindx
include "fxfcache.com"

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	call fpathname (IM_HDRFILE(im), Memc[fname], SZ_PATHNAME)

        # Remove the image from the FITS cache if found.
	do cindx=1, rf_cachesize {
	    if (rf_fit[cindx] == NULL)
		next
	    if (streq (Memc[fname], rf_fname[1,cindx])) {
		call mfree (rf_pextv[cindx], TY_INT)
		call mfree (rf_pextn[cindx], TY_CHAR)
		call mfree (rf_pixp[cindx], TY_INT)
		call mfree (rf_hdrp[cindx], TY_INT)
		call mfree (rf_fit[cindx], TY_STRUCT)
		call mfree (rf_hdr[cindx], TY_CHAR)
	    }
	}

	call sfree (sp)
end


# FXF_UPDATE_EXTEND -- Add or change the value of the EXTEND keyword in PHU.
# Sometimes the input PHU has not been created by the FK and the EXTEND keyw
# might not be there as the standard tells when an extension is appended
# to a file.

procedure fxf_update_extend (im)

pointer	im			#I image descriptor	

int	fd, fdout, i, nch, nc
char	line[LEN_CARD], tmp[SZ_FNAME], blank
int	open(), naxis, read(), strncmp(), note(), fnroot()

begin
	fd = open (IM_HDRFILE(im), READ_WRITE, BINARY_FILE)

	# Look for EXTEND keyword and change its value in place.
	nc = 0
	while (read (fd, line, 40) != EOF) {
	    nc = nc + 1
	    call achtbc (line, line, LEN_CARD)
	    if (strncmp ("EXTEND  ", line, 8) == 0) {
		line[30] = 'T'
	        call seek (fd, note(fd)-40)
		call achtcb (line, line, LEN_CARD)
		call write (fd, line, 40)
		call close (fd)
		return
	    } else if (strncmp ("END     ", line, 8) == 0) 
		break
	}

	# The EXTEND card is not in the header. Insert it after the
	# last NAXISi in a temporary file, rename after this.
	
	i = fnroot (IM_HDRFILE(im), tmp, SZ_FNAME)
	call mktemp (tmp, tmp, SZ_FNAME)
	fdout = open (tmp, NEW_FILE, BINARY_FILE)

	call seek (fd, BOF)
	do i = 0, nc-2 {
	    nch = read (fd, line, 40)
	    call write (fdout, line, 40)
	    call achtbc(line, line, LEN_CARD)
	    if (strncmp ("NAXIS   ", line, 8) == 0) 
	        call fxf_geti (line, naxis)
	    else if (strncmp ("NAXIS", line, 5)  == 0){
		if ((line[6] - '0') == naxis) {
		    # Now create the EXTEND card in the output file.
		    call fxf_encodeb ("EXTEND", YES, line, 
		        "File may contain extensions")
		    call achtcb (line, line , LEN_CARD)
		    call write (fdout, line, 40)
	        }
	    }
	}

	if (mod (nc, 36) == 0) {
	    # We have to write one END card and 35 blank card.
	    blank = ' '
	    call amovkc (blank, line, 80)
	    call amovc ("END", line, 3)
	    call achtcb (line, line , LEN_CARD)
	    call write (fdout, line, 40)
	    call amovkc (blank, line, 80)
	    call achtcb (line, line , LEN_CARD)
	    for (i=1;  i < 36;  i=i+1)
		call write (fdout, line, 40)
	} else {
	    nch = read (fd, line, 40)
	    call write (fdout, line, 40)
	}

	# Read one more line to synchronize.
	nch = read (fd, line, 40)

	# Copy the rest of the file. 
	call fcopyo (fd, fdout)
	
	call close (fd)
	call close (fdout)
	call fxf_ren_tmp (tmp, IM_HDRFILE(im))
end
