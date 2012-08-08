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

define  SZ_DATESTR    24


# FXF_UPDHDR -- Update the FITS header file.  This is done by writing an
# entire new header file and then replacing the old header file with the
# new one.  This is necessary since the header file is a text file and text
# files cannot be randomly updated.

procedure fxf_updhdr (im, status)

pointer	im			#I image descriptor
int	status			#O return status

pointer	sp, fit, mii, poff
pointer	outname, fits_file, tmp1, tmp2
bool    adjust_header, overwrite, append
int	i, nchars_ua, hdr_fd, group, hdr_off, size
int	npad, nlines, pixoff, grp_pix_off, nbks
int	acmode, in_fd, diff, hdr_acmode, in_off, nchars, subtype
int	read(), fxf_hdr_offset(), access(), strncmp()
int	open(), fstatl(), fnldir(), strlen(), stridxs()
bool	fnullfile()

errchk  open, read, write, fxf_header_diff, fxf_write_header, fxf_make_adj_copy
errchk  fxf_set_cache_time, syserr, syserrs, imerr
errchk  fxf_expandh, fxf_not_incache, fxf_ren_tmp, fxf_update_extend
long	clktime()

begin
	call smark (sp)
	call salloc (mii, FITS_BLOCK_CHARS, TY_INT)
	call salloc (fits_file, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_PATHNAME, TY_CHAR)
	call salloc (tmp1, max(SZ_PATHNAME,SZ_FNAME*2), TY_CHAR)
	call salloc (tmp2, max(SZ_PATHNAME,SZ_FNAME*2), TY_CHAR)

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

        subtype = 0
	if ((FKS_SUBTYPE(fit) == FK_PLIO || 
		(strncmp("PLIO_1", FIT_EXTSTYPE(fit), 6) == 0)) && 
		(IM_PL(im) != NULL))
	    subtype = FK_PLIO

	if (FIT_EXTTYPE(fit) != EOS && group != -1) {
	    if (strncmp (FIT_EXTTYPE(fit), "IMAGE", 5) != 0 &&
		    strncmp (FIT_EXTTYPE(fit), "SIMPLE", 6) != 0 &&
		    subtype == 0) {
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
	        call fxf_overwrite_unit (fit, im)

	    call strcpy (IM_PIXFILE(im), Memc[fits_file], SZ_FNAME)

	} else
	    call strcpy (IM_HDRFILE(im), Memc[fits_file], SZ_FNAME)
	
	# Calculate the header offset corresponding to group number 'group'. 
	FIT_IM(fit) = im
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

	# PLIO
	if (subtype == FK_PLIO && append) 
	    diff = 0

	# Adjust header only when we need to expand. We fill with trailing 
	# blanks in case diff .gt. 0. (Reduce header size).

	adjust_header = (diff < 0)
	if (adjust_header && FIT_EXPAND(fit) == NO) {
	    call syserr (SYS_FXFUPHEXP)
	    adjust_header = false
	}

	overwrite = (FKS_OVERWRITE(fit) == YES)
	if (adjust_header || overwrite) {
	    # We need to change the size of header portion in the middle of
	    # the file. The best thing to do is to make a copy in the output
	    # filename directory.

	    i = strlen (IM_PIXFILE(im))
	    nchars = fnldir (IM_PIXFILE(im), Memc[outname], SZ_PATHNAME)
	    if (nchars > 80 && i > 100) {
		i = stridxs ("!", Memc[outname])
	        call strcpy ("tmp$", Memc[outname+i], SZ_PATHNAME-i)
	    }
	    call strcpy (Memc[outname], Memc[tmp2], SZ_FNAME)
            call mktemp ("fx", Memc[tmp1], SZ_PATHNAME)
	    call strcat (".fits", Memc[tmp1], SZ_PATHNAME)
	    call strcat ("A", Memc[outname], SZ_PATHNAME)
	    call strcat (Memc[tmp1], Memc[outname], SZ_PATHNAME)
	    call strcat ("B", Memc[tmp2], SZ_PATHNAME)
	    call strcat (Memc[tmp1], Memc[tmp2], SZ_PATHNAME)
	    in_fd = open (Memc[fits_file], READ_ONLY, BINARY_FILE)
	    if (access (Memc[outname], 0, 0) == YES)
		call delete (Memc[outname])
	    hdr_fd = open (Memc[outname], NEW_FILE, BINARY_FILE)

            # Now expand the current group at least one block of 36 cards
	    # and guarantee that the other groups in the file will have at
	    # least 'nlines' of blank cards at the end of the header unit.

	    nlines= FKS_PADLINES(fit)
	    IM_HFD(im) = in_fd

	    if (adjust_header && acmode != NEW_COPY && 
		FIT_XTENSION(fit) == YES) {
		nbks = -diff/1440     # number of blocks to expand
		call fxf_expandh (in_fd, hdr_fd, nlines, group, nbks,
		    hdr_off, pixoff)
		nchars_ua = pixoff - hdr_off
		# Reload PHU from file if necessary
	        call fxf_not_incache(im)
		poff = FIT_PIXPTR(fit)
		Memi[poff+group] = pixoff
	    } else {
		if (append)
		    grp_pix_off = FIT_PIXOFF(fit)
		else {
		    # Reload PHU from file if necessary
	            call fxf_not_incache(im)
		    grp_pix_off = Memi[FIT_PIXPTR(fit)+group]
		}
		call fxf_make_adj_copy (in_fd, hdr_fd,
		    hdr_off, grp_pix_off, nchars_ua)
	    }
	    diff = 0
	    group = -1

	    # Reset the time so we can read a fresh header next time.
	    call fxf_set_cache_time (im, overwrite)
	} else {
	    hdr_fd = open (Memc[fits_file], hdr_acmode, BINARY_FILE)
	    # Do not clear if we are creating a Bintable with type PLIO_1.
	    if (subtype != FK_PLIO)
	        IM_PFD(im) = NULL
	    IM_HFD(im) = NULL
	}

	if (FIT_NEWIMAGE(fit) == YES)
	    call seek (hdr_fd, BOF)
	else if (hdr_off != 0)
	    call seek (hdr_fd, hdr_off)
	   
	if (acmode == NEW_COPY)
	    call fxf_setbitpix (im, fit)

	# Lets changed the value of FIT_MTIME that will be used as the mtime for
	# this updated file. This time them will be different in other
	# executable's FITS cache, hence rereading the PHU. 
	# We need to use FIT_MTIME since it reflec the value of keyword
	# IRAF_TLM which could have just recently been modified, hence adding
	# the 4 seconds.
	
        if (abs(FIT_MTIME(fit) - clktime(long(0))) > 60)
	    FIT_MTIME(fit) = clktime(long(0))

	# We cannot use clktime() directly since the previuos value
	# of FIT_MTIME might already have a 4 secs increment.

        FIT_MTIME(fit) = FIT_MTIME(fit) + 4

	# Now write default cards and im_userarea to disk.
	nchars_ua = nchars_ua + diff
	call fxf_write_header (im, fit, hdr_fd, nchars_ua, group)

 	size = fstatl (hdr_fd, F_FILESIZE)
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

	    in_fd = open (IM_HDRFILE(im), READ_ONLY, BINARY_FILE)
	    group = FIT_GROUP(fit)
	    call fxf_not_incache (im)
	    in_off = Memi[FIT_HDRPTR(fit)+group+1]
	    call seek (hdr_fd, EOF)
	    call seek (in_fd, in_off)
	    size = FITS_BLOCK_CHARS

	    while (read (in_fd, Memi[mii], size) != EOF) 
		call write (hdr_fd, Memi[mii], size)

	    call close (hdr_fd)
	    call close (in_fd)

	    call fxf_ren_tmp (IM_PIXFILE(im), IM_HDRFILE(im), Memc[tmp2], 1, 1)

	    # Change the acmode so we can change the modification and
	    # this way reset the cache for this file.

	    IM_ACMODE(im) = READ_WRITE
	    call fxf_over_delete(im)

	} else {
	    if (adjust_header || overwrite)
	        call close (in_fd)
	    call close (hdr_fd)

            # If the header has been expanded then rename the temp file
	    # to the original name.
	    if (adjust_header)
		call fxf_ren_tmp (Memc[outname], IM_PIXFILE(im),
		    Memc[tmp2], 1, 1)
	}

	# Make sure we reset the modification time for the cached header
	# since we have written a new version. This way the header will
	# be read from disk next time the file is accessed.

        if (IM_ACMODE(im) == READ_WRITE || overwrite)  {
           # The modification time of a file in the cache can be different
	   # from another mod entry in another executable. We need to make
	   # sure that the mod time has changed in more than a second so that
	   # the other executable can read the header from disk and not
	   # from the cache entry. The FIT_MTIME value has already been
	   # changed by adding 4 seconds. (See above).

           call futime (IM_HDRFILE(im), NULL, FIT_MTIME(fit))
#           call futime (IM_HDRFILE(im), NULL, clktime(long(0))+4)
	}

	if (FIT_GROUP(fit) == 0 || FIT_GROUP(fit) == -1)
	   call fxf_set_cache_time (im, false)

	# See if we need to add or change the value of EXTEND in the PHU.
	if (FIT_XTENSION(fit) == YES && 
	   (FIT_EXTEND(fit) == NO_KEYW || FIT_EXTEND(fit) == NO)) {
		call fxf_update_extend (im)
	   }

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
	} else {
	    call fxf_not_incache (FIT_IM(fit))
	    hdr_off = Memi[FIT_HDRPTR(fit)+group]
        }
	
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
int	ua, fit, hdr_size, pixoff, clines, ulines, len, padlines
int     merge, usize, excess, nheader_cards, rp, inherit, kmax, kmin
int     strlen(), imaccf(), imgeti(), strcmp(), idb_findrecord()
int	btoi(), strncmp()
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

	    # Check if the file is still in cache. We need CACHELEN and
            # CACHEHDR. 

            call fxf_not_incache (im)

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

	    # Now copy the buffer pointed by 'pb' to UA.
	    call strcpy (Memc[tb], Memc[ua], ualen)

	    call sfree (sp)
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

        # If FIT_OBJECT is empty, then there was no OBJECT card at read
	# time. If OBJECT is present now, then it was added now. If OBJECT
	# was present but not now, the keyword was deleted.

	temp[1] = EOS
	if (imaccf (im, "OBJECT") == YES) {
	    call imgstr (im, "OBJECT", temp, LEN_CARD)
	    # If its value is blank, then temp will be NULL
	    if (temp[1] == EOS)
		call strcpy ("       ", temp, LEN_CARD)
	}

	if (temp[1] != EOS)
	    call strcpy (temp, FIT_OBJECT(fit), LEN_CARD)
	else
	    nheader_cards = nheader_cards - 1

	if (FIT_OBJECT(fit) == EOS) {
	    if (strcmp (IM_TITLE(im), FIT_TITLE(fit)) != 0) {
		call strcpy (IM_TITLE(im), FIT_OBJECT(fit), LEN_CARD)
		# The OBJECT keyword will be added.
		nheader_cards = nheader_cards + 1
	    }
	} else {
	    # See if OBJECT has been deleted from UA.
	    if (temp[1] == EOS)
		FIT_OBJECT(fit) = EOS
	    if (strcmp (IM_TITLE(im), FIT_TITLE(fit)) != 0)
		call strcpy (IM_TITLE(im), FIT_OBJECT(fit), LEN_CARD)
	}


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

	# Finally if we are updating a BINTABLE with a PLIO_1 mask we need
	# to add 3 to the mandatory cards since TFIELDS, TTYPE1, nor
	# TFORM1 are included.   ### Ugh!!
	# Also add the Z cards.

	if (strncmp ("PLIO_1", FIT_EXTSTYPE(fit), 6) == 0)
	    nheader_cards = nheader_cards + 3 + 6 + IM_NDIM(im)*2

	# Compute current header size rounded to a header block.
	usize = strlen (Memc[ua]) 
	len = (usize / LEN_UACARD + nheader_cards) * LEN_CARD 
	len = FITS_LEN_CHAR(len / 2)

	# Ask for more lines if the header can or needs to be expanded.
	padlines = FKS_PADLINES(fit)

	# Here we go over the FITS header area already allocated?
	if (acmode == READ_WRITE || acmode == WRITE_ONLY) {
	    call fxf_not_incache(im)
	    hoff = FIT_HDRPTR(fit)
	    poff = FIT_PIXPTR(fit)
	    hdr_size = Memi[poff+group] - Memi[hoff+group]
	    ualen = len
	    diff = hdr_size - ualen
	    # If the header needs to be expanded add on the pad lines.
	    if (diff < 0) {
		ualen = (usize/LEN_UACARD + nheader_cards + padlines) * LEN_CARD
		ualen = FITS_LEN_CHAR(ualen / 2)
	    }
	    diff = hdr_size - ualen
	} else if ((hdr_off == EOF || hdr_off == 0) && 
	    (IM_NDIM(im) == 0 || FIT_NAXIS(fit) == 0)) {
	    hdr_size = len
	    ualen = len
	} else {
	    hdr_size = pixoff
	    # The header can expand so add on the pad lines.
	    ualen = (usize / LEN_UACARD + nheader_cards + padlines) * LEN_CARD
	    ualen = FITS_LEN_CHAR(ualen / 2)
	    diff = hdr_size - ualen
        }

        if (diff < 0 && FIT_EXPAND(fit) == NO) {
	    # We need to reduce the size of the UA becuase we are not
	    # going to expand the header.
	    excess = mod (nheader_cards * 81 + usize, 1458)
	    excess = excess + (((-diff-1400)/1440)*1458)
	    Memc[ua+usize-excess] = EOS
	    usize = strlen (Memc[ua]) 
	    ualen = (usize / LEN_UACARD + nheader_cards) * LEN_CARD 
	    ualen = FITS_LEN_CHAR(ualen / 2)
	}
end


# FXF_WRITE_HDR -- Procedure to write header unit onto the PHU or EHU.

procedure fxf_write_header (im, fit, hdr_fd, nchars_ua, group)

pointer	 im		  	#I image structure
pointer	 fit     		#I fits structure
int	 hdr_fd  		#I FITS header file descriptor
int	 nchars_ua		#I header size
int	group			#I group number

char	temp[SZ_FNAME] 
bool	xtension, ext_append
pointer	sp, spp, mii, rp, uap
char    card[LEN_CARD], blank, keyword[SZ_KEYWORD], datestr[SZ_DATESTR] 
int	iso_cutover, n, i, sz_rec, up, nblanks, acmode, nbk, len, poff, diff
int     pos, pcount, depth, subtype, maxlen, ndim

long	clktime()
int	imaccf(), strlen(), fxf_ua_card(), envgeti()
int	idb_findrecord(), strncmp(), btoi()
bool	fxf_fpl_equald(), imgetb(), itob()
long    note()
errchk  write 

begin
	call smark (sp)
	call salloc (spp, FITS_BLOCK_CHARS*5, TY_CHAR)
	call salloc (mii, FITS_BLOCK_CHARS, TY_INT)

	# Write out the standard, reserved header parameters.
	n = spp
	blank = ' '
	acmode = FIT_ACMODE(fit)
	ext_append = ((acmode == NEW_IMAGE || acmode == NEW_COPY) &&
	    (FKS_EXTNAME(fit) != EOS || !IS_INDEFL (FKS_EXTVER(fit))))

	xtension = (FIT_XTENSION(fit) == YES)
	if (FIT_NEWIMAGE(fit) == YES)
	    xtension = false

	subtype =0
	if ((FKS_SUBTYPE(fit) == FK_PLIO || 
		(strncmp("PLIO_1", FIT_EXTSTYPE(fit), 6) == 0)) &&
		IM_PL(im) != NULL) {

	    subtype = FK_PLIO
	    ext_append = true
	}

	# PLIO.  Write BINTABLE header for a PLIO mask.
	if (subtype == FK_PLIO) {

	    if (IM_PFD(im) != NULL) {
		call fxf_plinfo (im, maxlen, pcount, depth)

		# If we old heap has change in size, we need to
		# resize it.

		if (acmode == READ_WRITE && pcount != FIT_PCOUNT(fit))
		    call fxf_pl_adj_heap (im, hdr_fd, pcount)
	    } else {
		pcount = FIT_PCOUNT(fit)
		depth = DEF_PLDEPTH
	    }

	    ndim = IM_NDIM(im)
	    call fxf_akwc ("XTENSION", "BINTABLE", 8, "Mask extension", n)
	    call fxf_akwi ("BITPIX", 8, "Bits per pixel", n)
	    call fxf_akwi ("NAXIS", ndim, "Number of axes", n)
	    call fxf_akwi ("NAXIS1", 8, "Number of bytes per line", n)
	    do i = 2, ndim {
		call fxf_encode_axis ("NAXIS", keyword, i)
	        call fxf_akwi (keyword, IM_LEN(im,i), "axis length", n)
	    }
	    call fxf_akwi ("PCOUNT", pcount, "Heap size in bytes", n)
	    call fxf_akwi ("GCOUNT", 1, "Only one group", n)

	    if (imaccf (im, "TFIELDS") == NO)
		call fxf_akwi ("TFIELDS", 1, "1 Column field", n)
	    if (imaccf (im, "TTYPE1") == NO) {
		call fxf_akwc ("TTYPE1", "COMPRESSED_DATA", 16,
		    "Type of PLIO_1 data", n)
	    }
	    call sprintf (card, LEN_CARD, "PI(%d)")
		call pargi(maxlen) 
	    call fxf_filter_keyw (im, "TFORM1")
	    len = strlen (card)
	    call fxf_akwc ("TFORM1", card, len, "Variable word array", n)

	} else {
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
		    i = NO
		else {
		   # Keyword exists but it may be in the wrong position. 
		   # Remove it and write it now.

		   i = btoi (imgetb (im, "EXTEND"))
		   call fxf_filter_keyw (im, "EXTEND") 
		}
		if (FIT_EXTEND(fit) == YES)
		    i = YES
		call fxf_akwb ("EXTEND", i, "File may contain extensions", n)
		FIT_EXTEND(fit) = YES
	    }
	}

        # Delete BSCALE and BZERO just in case the application puts them
        # in the UA after the pixels have been written. The keywords
        # should not be there since the FK does not allow reading pixels
        # with BITPIX -32 and BSCALE and BZERO. If the application
        # really wants to circumvent this restriction the code below
        # will defeat that. The implications are left to the application.
        # This fix is put in here to save the ST Hstio interface to be
        # a victim of the fact that in v2.12 the BSCALE and BZERO keywords
        # are left in the header for the user to see or change. Previous
        # FK versions, the keywords were deleted from the UA.

        if ((IM_PIXTYPE(im) == TY_REAL || IM_PIXTYPE(im) == TY_DOUBLE)
                && (FIT_TOTPIX(fit) > 0 && FIT_BITPIX(fit) <= 0)) {

             call fxf_filter_keyw (im, "BSCALE")
             call fxf_filter_keyw (im, "BZERO")
        }

	# Do not write BSCALE and BZERO if they have the default 
	# values (1.0, 0.0).

	if (IM_PIXTYPE(im) == TY_USHORT) {
	    call fxf_filter_keyw (im, "BSCALE") 
	    call fxf_akwd ("BSCALE", 1.0d0,
		"REAL = TAPE*BSCALE + BZERO", NDEC_REAL, n)
	    call fxf_filter_keyw (im, "BZERO") 
	    call fxf_akwd ("BZERO", 32768.0d0,  "", NDEC_REAL, n)
	} else if (FIT_PIXTYPE(fit) != TY_REAL &&
	    FIT_PIXTYPE(fit) != TY_DOUBLE && IM_ACMODE(im) != NEW_COPY) {
	    # Now we have TY_SHORT or TY_(INT,LONG).
	    # Check the keywords only if they have non_default values.

	    # Do not add the keywords if they have been deleted.
	    if (!fxf_fpl_equald(1.0d0, FIT_BSCALE(fit), 4)) {
	        if ((imaccf (im, "BSCALE") == NO) && 
		     fxf_fpl_equald (1.0d0, FIT_BSCALE(fit), 4)) {
		    call fxf_akwd ("BSCALE",  FIT_BSCALE(fit),
			"REAL = TAPE*BSCALE + BZERO", NDEC_REAL, n)
	        }
	    }
	    if (!fxf_fpl_equald(0.0d0, FIT_BZERO(fit), 4) ) {
	        if (imaccf (im, "BZERO") == NO &&
		    fxf_fpl_equald (1.0d0, FIT_BZERO(fit), 4))
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
	    # Update the cache in case these values have changed
	    # in the UA.
	    call fxf_set_extnv (im)

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
		    call imputb (im, "INHERIT", itob(FIT_INHERIT(fit)))
	    } else {
		call fxf_akwb ("INHERIT",
		    FIT_INHERIT(fit), "Inherits global header", n)
	    }
	}

	# Dates after iso_cutover use ISO format dates.
	iferr (iso_cutover = envgeti (ENV_ISOCUTOVER))
	    iso_cutover = DEF_ISOCUTOVER

	# Encode the "DATE" keyword (records create time of imagefile).
	call fxf_encode_date (clktime(long(0)), datestr, SZ_DATESTR,
	    "ISO", iso_cutover)
	len = strlen (datestr)

	if (idb_findrecord (im, "DATE", rp) == 0) {
	    # Keyword is not in the UA, created with current time
	    call fxf_akwc ("DATE",
		datestr, len, "Date FITS file was generated", n)
	} else { 
	    if (acmode == READ_WRITE) {
		# Keep the old DATE, change only the IRAF-TLM keyword value
		call imgstr (im, "DATE", datestr, SZ_DATESTR)
	    }
	    # See if the keyword is out of order.
	    if (rp - uap > 12*81) {  
		call fxf_filter_keyw (im, "DATE")

		call fxf_akwc ("DATE",
		    datestr, len, "Date FITS file was generated", n)
	    } else
	        call impstr (im, "DATE", datestr) 
	}

	# Encode the "IRAF_TLM" keyword (records time of last modification).
	if (acmode == NEW_IMAGE || acmode == NEW_COPY) {
	    FIT_MTIME(fit) = IM_MTIME(im)
	}

	call fxf_encode_date (FIT_MTIME(fit), datestr, SZ_DATESTR, "TLM", 2010)
#	call fxf_encode_date (clktime(long(0))+4, datestr, SZ_DATESTR, "TLM", 2010)
	len = strlen (datestr)

	if (idb_findrecord (im, "IRAF-TLM", rp) == 0) {
	    call fxf_akwc ("IRAF-TLM",
		datestr, len, "Time of last modification", n)
	} else if (rp - uap > 13*81) {  
	    call fxf_filter_keyw (im, "IRAF-TLM")
	    call fxf_akwc ("IRAF-TLM",
		datestr, len, "Time of last modification", n)
	} else 
	    call impstr (im, "IRAF-TLM", datestr) 

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

	# Write Compression keywords for PLIO BINTABLE.
#	if (subtype == FK_PLIO && IM_PFD(im) != NULL && ext_append) {
	if (subtype == FK_PLIO) { 
	    call fxf_akwb ("ZIMAGE", YES, "Is a compressed image", n)
	    call fxf_akwc ("ZCMPTYPE", "PLIO_1", 6, "IRAF image masks", n) 
	    call fxf_akwi ("ZBITPIX", 32, "BITPIX for uncompressed image",n)

	    # We use IM_NDIM and IM_LEN here because FIT_NAXIS and _LENAXIS
	    # are not available for NEW_IMAGE mode.

	    ndim = IM_NDIM(im)
	    call fxf_akwi ("ZNAXIS", ndim, "NAXIS for uncompressed image",n)
	    do i = 1, ndim {
	        call fxf_encode_axis ("ZNAXIS", keyword, i)
	        call fxf_akwi (keyword, IM_LEN(im,i),  "Axis length", n)
	    }
	    call fxf_encode_axis ("ZTILE", keyword, 1)
	    call fxf_akwi (keyword, IM_LEN(im,1), "Axis length", n)
	    do i = 2, ndim {
		call fxf_encode_axis ("ZTILE", keyword, i)
		call fxf_akwi (keyword, 1, "Axis length", n)
	    }
	    call fxf_encode_axis ("ZNAME", keyword, 1)
	    call fxf_akwc (keyword, "depth", 5, "PLIO mask depth", n)
	    call fxf_encode_axis ("ZVAL", keyword, 1)
	    call fxf_akwi (keyword, depth, "Parameter value", n)
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
	if (n > 0)
	    nbk = nbk + 1
	diff = nchars_ua - nbk * 1440
	if (diff > 0) {
	    if (n > 0) {
		call miipak (Memc[spp], Memi[mii], sz_rec*2, TY_CHAR, MII_BYTE)
		call write (hdr_fd, Memi[mii], sz_rec)
	    }

	    if (group < 0) {
		# We are writing blocks of blanks on a new_copy
		# image which has group=-1 here. Use diff.

		nbk = diff / 1440
	    } else {
		pos = note (hdr_fd)
		call fxf_not_incache(im)
		poff = FIT_PIXPTR(fit)
		nbk = (Memi[poff+group] - pos)
		nbk = nbk / 1440
	    }
	    call amovkc (blank, Memc[spp], 2880)
	    call miipak (Memc[spp], Memi[mii], sz_rec*2, TY_CHAR, MII_BYTE)
	    do i = 1, nbk-1
		call write (hdr_fd, Memi[mii], sz_rec)
	   
	    call amovkc (blank, Memc[spp], 2880)
	    rp = spp+2880-LEN_CARD
	}

	call amovc ("END", Memc[rp], 3)
	call miipak (Memc[spp], Memi[mii], sz_rec*2, TY_CHAR, MII_BYTE)
	call write (hdr_fd, Memi[mii], sz_rec)
	# PLIO: write the mask data to the new extension.
	if (subtype == FK_PLIO && IM_PFD(im) != NULL) {
	    call fxf_plwrite (im, hdr_fd)
	    IM_PFD(im) = NULL
	}

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
# of a fits header.

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

procedure fxf_set_cache_time (im, overwrite)

pointer im			#I image descriptor
bool    overwrite		#I invalidate entry if true

pointer sp, hdrfile, fit
long    fi[LEN_FINFO]
int	finfo(), cindx
errchk	syserr, syserrs
bool    streq()

include "fxfcache.com"

begin
	call smark (sp)
	call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)

	fit = IM_KDES(im)

	call fpathname (IM_HDRFILE(im), Memc[hdrfile], SZ_PATHNAME)
	if (finfo (Memc[hdrfile], fi) == ERR)
	    call syserrs (SYS_FOPEN, IM_HDRFILE(im))

	# Search the header file cache for the named image.
	do cindx = 1, rf_cachesize {
	    if (rf_fit[cindx] == NULL)
	        next

	    if (streq (Memc[hdrfile], rf_fname[1,cindx])) {
		# Reset cache
		if (IM_ACMODE(im) == READ_WRITE || overwrite) {
		    # Invalidate entry.
		    call mfree (rf_pextv[cindx], TY_INT)
		    call mfree (rf_pextn[cindx], TY_CHAR)
		    call mfree (rf_pixp[cindx], TY_INT)
		    call mfree (rf_hdrp[cindx], TY_INT)
		    call mfree (rf_fit[cindx], TY_STRUCT)
		    call mfree (rf_hdr[cindx], TY_CHAR)
		    rf_fname[1,cindx] = EOS
		    rf_mtime[cindx] = 0
	     	    rf_fit[cindx] = NULL

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


# FXF_SET_EXTNV -- Procedure to write UA value of EXTNAME and EXTVER
# into the cache slot.

procedure fxf_set_extnv (im)

pointer im			#I image descriptor

pointer fit, sp, hdrfile
int	cindx, ig, extn, extv
errchk	syserr, syserrs
bool    bxtn, bxtv
bool    streq()

include "fxfcache.com"

begin
	fit = IM_KDES(im)
	ig = FIT_GROUP(fit)

	call smark (sp)
	call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)

	# Search the header file cache for the named image.
	do cindx = 1, rf_cachesize {
	    if (rf_fit[cindx] == NULL)
	        next

	    if (streq (Memc[hdrfile], rf_fname[1,cindx])) {
		bxtn = (FIT_EXTNAME(fit) != EOS)
		bxtv = (!IS_INDEFL (FIT_EXTVER(fit)))
		# Reset cache
		if (IM_ACMODE(im) == READ_WRITE) {
		    if (bxtn) {
			extn = rf_pextn[cindx]
		        # Just replace the value
			call strcpy (FIT_EXTNAME(fit), Memc[extn+LEN_CARD*ig],
			    LEN_CARD)
		    }
		    if (bxtv) {
			extv = rf_pextv[cindx]
		        # Just replace the value
			Memi[extv+ig] = FIT_EXTVER(fit)
		    }
		}
		break
	    }
	}

	call sfree (sp)
end	


# FXF_REN_TMP -- Rename input file to output file.
#
# The output file may already exists in which case it is replaced.
# Because this operation is critical it is heavily error checked and
# has retries to deal with networking cases.

procedure fxf_ren_tmp (in, out, tmp, ntry, nsleep)

char 	in[ARB]			#I file to replace output
char    out[ARB]		#O output file (replaced if it exists)
char	tmp[ARB]		#I temporary name for in until rename succeeds
int	ntry			#I number of retries for rename
int	nsleep			#I Number of seconds to sleep before retry

int	i, stat, err, access(), protect(), errget()
bool	replace, prot
pointer	errstr

errchk	access, protect, rename, delete, salloc

begin
#call eprintf ("fxf_ren_tmp (%s, %s, %s, %d %d)\n")
#call pargstr (in)
#call pargstr (out)
#call pargstr (tmp)
#call pargi (ntry)
#call pargi (nsleep)
	err = 0; errstr = NULL

	iferr {
	    # Move original output out of the way.
	    # Don't delete it in case of an error.
	    replace = (access (out, 0, 0) == YES)
	    prot = false
	    if (replace) {
	        prot = (protect (out, QUERY_PROTECTION) == YES)
		if (prot)
		    stat = protect (out, REMOVE_PROTECTION)
		do i = 0, max(0,ntry) {
#call eprintf ("1 rename (%s, %s)\n")
#call pargstr (out)
#call pargstr (tmp)
		    ifnoerr (call rename (out, tmp)) {
			err = 0
		        break
		    }
		    if (errstr == NULL)
		        call salloc (errstr, SZ_LINE, TY_CHAR)
		    err = errget (Memc[errstr], SZ_LINE)
		    if (err == 0)
		        err = SYS_FMKCOPY
		    call tsleep (nsleep)
		}
		if (err > 0)
		    call error (err, Memc[errstr])
	    }

	    # Now rename the input to the output.
	    do i = 0, max(0,ntry) {
#call eprintf ("2 rename (%s, %s)\n")
#call pargstr (in)
#call pargstr (out)
		ifnoerr (call rename (in, out)) {
		    err = 0
		    break
		}
		if (errstr == NULL)
		    call salloc (errstr, SZ_LINE, TY_CHAR)
		err = errget (Memc[errstr], SZ_LINE)
		if (err == 0)
		    err = SYS_FMKCOPY
		call tsleep (nsleep)
	    }
	    if (err > 0)
		call error (err, Memc[errstr])
	    if (prot)
		stat = protect (out, SET_PROTECTION)

	    # If the rename has succeeded delete the original data.
	    if (replace) {
#call eprintf ("delete (%s)\n")
#call pargstr (tmp)
	        call delete (tmp)
	    }
	} then
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
		rf_fit[cindx] = NULL
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

pointer sp, hdrfile, tmp1, tmp2
int	fd, fdout, i, nch, nc, cfit
char	line[LEN_CARD], blank, cindx
bool	streq()
int	open(), naxis, read(), strncmp(), fnroot()
long    note()
errchk	open, fxf_ren_tmp

include "fxfcache.com"
define	cfit_ 91

begin
	call smark (sp)
	call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)

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
		goto cfit_
	    } else if (strncmp ("END     ", line, 8) == 0) 
		break
	}

	# The EXTEND card is not in the header. Insert it after the
	# last NAXISi in a temporary file, rename after this.
	
	call salloc (tmp1, SZ_FNAME, TY_CHAR)
	i = fnroot (IM_HDRFILE(im), Memc[tmp1], SZ_FNAME)
	call mktemp (Memc[tmp1], Memc[tmp1], SZ_FNAME)

	fdout = open (Memc[tmp1], NEW_FILE, BINARY_FILE)

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

	call salloc (tmp2, SZ_FNAME, TY_CHAR)
	call strcpy (Memc[tmp1], Memc[tmp2], SZ_FNAME)
	call strcat ("A", Memc[tmp2], SZ_FNAME)
	call fxf_ren_tmp (Memc[tmp1], IM_HDRFILE(im), Memc[tmp2], 1, 1)

cfit_
	# Now reset the value in the cache
	call fpathname (IM_HDRFILE(im), Memc[hdrfile], SZ_PATHNAME)

	# Search the header file cache for the named image.
	do cindx = 1, rf_cachesize {
	    if (rf_fit[cindx] == NULL)
	        next

	    if (streq (Memc[hdrfile], rf_fname[1,cindx])) {
		# Reset cache
		cfit = rf_fit[cindx]
		FIT_EXTEND(cfit) = YES
		break
	    }
	}

	call sfree (sp)
end
