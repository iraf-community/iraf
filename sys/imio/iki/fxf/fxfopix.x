# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include <syserr.h>
include <error.h>
include	<imhdr.h>
include	<imio.h>
include <mach.h>
include	"fxf.h"
include	<fset.h>

define	MIN_BUFSIZE  512


# FXF_OPIX -- Open (or create) the pixel storage file.

procedure fxf_opix (im, status)

pointer	im			#I image descriptor
int	status 			#O return status

pointer sp, fn, fit
char    pathname[SZ_PATHNAME]
int	compress, blklen, pixoff, filesize
int	i, hdr_size, sz_pixfile, sz_fitfile, junk, npix
extern  fxfzop(), fxfzrd(), fxfzwr(), fxfzwt(), fxfzst(), fxfzcl()
int	strncmp(), fxf_header_size(), fxf_totpix()
int     strlen(), fopnbf(), fstatl(), itoc()

include	<szpixtype.inc>

define  err_ 91
define  endowr_ 92

begin
	call smark (sp)
	call salloc (fn, SZ_PATHNAME, TY_CHAR)

	status = OK
	fit = IM_KDES(im)

	compress = YES
	blklen   = 1
	pixoff   = 1

	# Tell IMIO where the pixels are.  Append the 'fit' mem descriptor
	# to filename so that low level zfiofit routines can use it.

        call strcpy (IM_HDRFILE(im), Memc[fn], SZ_PATHNAME)
	call strcat ("_", Memc[fn], SZ_PATHNAME)
	i = strlen (Memc[fn])
	junk = itoc (fit, Memc[fn+i], SZ_PATHNAME) 
        iferr (call fpathname (Memc[fn], pathname, SZ_PATHNAME))
	    goto err_

	if (FKS_OVERWRITE(fit) == YES) {
	    call fxf_overwrite_unit (fit, im)
	    goto endowr_
	}

	switch (IM_ACMODE(im)) {
	case READ_ONLY, READ_WRITE, WRITE_ONLY:
	    # Turn on IEEE mapping on input only.
	    call ieegnanr (FIT_SVNANR(fit))
	    call ieegmapr (FIT_SVMAPRIN(fit), FIT_SVMAPROUT(fit))
	    call ieegnand (FIT_SVNAND(fit))
	    call ieegmapd (FIT_SVMAPDIN(fit), FIT_SVMAPDOUT(fit))
	    call ieesnanr (0.0)
	    call ieemapr (YES, NO)
	    call ieesnand (0.0D0)
	    call ieemapd (YES, NO)

	    # If the FIT datatype is BYTE or SHORT with scaling then
	    # convert to TY_SHORT and TY_REAL respectively before
	    # releasing the data to the upper level calls. This is
	    # because IMIO does not support BYTE datatype and the need
	    # to scale 16 bits to 32 bits.
	    
	    # Do not open pixel portion if it is empty or is not
	    # an IMAGE type.

	    if ((strncmp (FIT_EXTTYPE(fit), "IMAGE", 5) != 0 &&
		 strncmp (FIT_EXTTYPE(fit), "SIMPLE", 6) != 0) || 
		 IM_NDIM(im) <= 0) {

	        goto err_
	    }

	    FIT_IM(fit) = im
	    iferr (IM_PFD(im) = fopnbf (pathname, IM_ACMODE(im),
		    fxfzop, fxfzrd, fxfzwr, fxfzwt, fxfzst, fxfzcl)) {
	        IM_PFD(im) = NULL
	        goto err_
	    }
	    
	    FIT_TOTPIX(fit) = fxf_totpix(im)
	    filesize = fstatl (IM_PFD(im), F_FILESIZE)
	    FIT_PFD(fit) = IM_PFD(im)

	case NEW_COPY, NEW_IMAGE, APPEND:
	    # See if the application has set the number of dimensions.
	    call fxf_chk_ndim (im)
	    FIT_PIXTYPE(fit) = IM_PIXTYPE(im)
	    npix = fxf_totpix (im)
	    FIT_NAXIS(fit) = IM_NDIM(im)
	    call amovi (IM_LEN(im,1), FIT_LENAXIS(fit,1), IM_NDIM(im))

	    call fxf_discard_keyw (im)
	    FIT_TOTPIX(fit) = npix

	    # Do not allow BSCALE and BZERO in the UA when making a new copy or
	    # new image if bitpix is negative. Except for ushort

	    if (IM_PIXTYPE(im) != TY_USHORT) {
		call fxf_filter_keyw (im, "BSCALE")
		call fxf_filter_keyw (im, "BZERO")
	    }

	    # Hdr_size is in char units. (i.e. 1440 chars per FITS block).
	    hdr_size = fxf_header_size (im)

            # Reset the scaling parameter because in NEW_COPY mode there
	    # should not be scaled pixels.  The previous call will get these
	    # values from the input image.

	    FIT_BSCALE(fit) = 1.0d0
	    FIT_BZERO(fit) = 0.0d0

	    sz_pixfile = npix * pix_size[IM_PIXTYPE(im)]

	    # The pixel file needs to be a multiple of 1440 chars.
	    sz_pixfile = FITS_LEN_CHAR (sz_pixfile)
	    sz_fitfile = sz_pixfile + hdr_size

	    if (FIT_NEWIMAGE(fit) == YES)
	       call falloc (IM_PIXFILE(im), sz_fitfile) 
	      
	    FIT_IM(fit) = im

	    iferr (IM_PFD(im) = fopnbf (pathname, READ_WRITE,
		    fxfzop, fxfzrd, fxfzwr, fxfzwt, fxfzst, fxfzcl)) {
		IM_PFD(im) = NULL
		call erract (EA_FATAL)
		goto err_
	    }

	    FIT_PFD(fit) = IM_PFD(im)
	    filesize = fstatl (IM_PFD(im), F_FILESIZE)
	    FIT_EOFSIZE(fit) = filesize + 1

	    if (FIT_NEWIMAGE(fit) == NO) {
	        # Now we are appending a new IMAGE extension.
	        # Write a blank header in order to append the
	        # pixels after it.

	        pixoff = filesize + hdr_size + 1

		# Update the offset for the blank write to follow which uses
		# a local file driver tied to the IM_PFD descriptor and not
		# the normal FIO.
                FIT_PIXOFF(fit) = pixoff

	        # Update filesize
	        filesize = filesize + sz_fitfile
	        call fxf_write_blanks (IM_PFD(im), hdr_size)
	    } else 
	        pixoff = hdr_size + 1

            FIT_PIXOFF(fit) = pixoff
	    call imioff (im, pixoff, compress, blklen)

	    IM_HFD(im) = NULL

	default:
	    call imerr (IM_NAME(im), SYS_IMACMODE)
	}

endowr_
	FIT_PFD(fit) = IM_PFD(im)
	FIT_HFD(fit) = IM_HFD(im)

	# The following statement is to pass the datatype at the low
	# level fits read and write routines. The datatype value can
	# change after the image is open. Hopefully the value of 'im'
	# will remain static.
	
	FIT_IM(fit) = im
	status = OK

 	call sfree (sp)
	return
err_
	status = ERR
        call sfree (sp)
end
 

# FXF_HEADER_SIZE -- Function to calculate the header size that would go 
# into the output file extension.

int procedure fxf_header_size (im)

pointer im 				#I Image descriptor

bool	inherit
int	merge, hdr_size
pointer op, fit, sp, tb, pb
int	nheader_cards, ualen, ulines, clines
int	strlen()

begin
	fit = IM_KDES(im)
        inherit = false

	# Fks_inherit is a combined value.
        if (FKS_INHERIT(fit) == YES)
	    inherit = true

	call fxf_mandatory_cards (im, nheader_cards)

	if (FIT_NEWIMAGE(fit) == NO && inherit) {
	    # See if current UA keywords are in the global header, if not
	    # there put it in a spool file. At the end, the spool file size is
	    # the output extension header size to be use in fitupdhdr.

	    # Check if the file is still in cache. We need CACHELEN and
            # CACHEHDR. 

            call fxf_not_incache (im)

	    op = IM_USERAREA(im)
	    ualen = strlen (Memc[op])
	    ulines = ualen / LEN_UACARD
	    clines = FIT_CACHEHLEN(fit) / LEN_UACARD

	    call smark (sp)
	    call salloc (tb, ualen+1, TY_CHAR)

	    merge = NO
	    pb = tb

	    # Now select those lines from the UA (pointed by op) that are
	    # not in the cache and accumulate them in 'pb'.

	    call fxf_match_str (op, ulines, FIT_CACHEHDR(fit), clines,merge,pb)
	    Memc[pb+1] = EOS
	    ualen = strlen (Memc[tb])

	    call sfree (sp)

	} else {
	    op = IM_USERAREA(im)
	    ualen = strlen (Memc[op])
	}

	ulines = ualen / LEN_UACARD + nheader_cards + FKS_PADLINES(fit)

	##### Note: PHULINES is not currently used, should be implemented
	##### Not clear to me if this code here is used for the PHU since
	##### it is in opix!

	# See if the application has set a minumum number of card for the UA.

	ulines = max (ulines, FKS_EHULINES(fit))

	# The user area contains new_lines (81 chars, LEN_UACARD).  Scale to
	# 80 chars (LEN_CARD).  Ualen is in bytes.

	ualen = ulines * LEN_CARD 

	# Calculate the number of header FITS blocks in chars.
	hdr_size = FITS_LEN_CHAR (ualen / 2)

	return (hdr_size)
end


# FXF_BYTE_SHORT -- This routine is obsolete and has been deleted, but is
# being preserved for the V2.11.2 patch so that a new shared library version
# does not have to be created.  It can be deleted in the next major release.

procedure fxf_byte_short (im, fname)

pointer	im
char	fname[ARB]

begin
end


# FXF_WRITE_BLANKS --Procedure to append a blank header to an existing
# file, preparing to write data after it.

procedure fxf_write_blanks (fd, size)

int	fd			#I File descriptor
int	size			#I New size (chars) to allocate.

pointer sp, bf
int	nblocks,i, fits_lenc

begin
	call smark (sp)

	# Length of a FITS block (2880) in chars.
	fits_lenc = FITS_BLOCK_BYTES/SZB_CHAR
	call salloc (bf, fits_lenc, TY_INT)
	call amovki (0, Memi[bf], fits_lenc)

	size = FITS_LEN_CHAR(size)
	nblocks = size / fits_lenc

	call seek (fd, EOF)
	do i = 1, nblocks 
	    call write (fd, Memi[bf], fits_lenc)

	call sfree (sp)
end


# FXF_MANDATORY_CARDS --  Count the required FITS header cards.
# The cards for the Main Unit are: SIMPLE, BITPIX, NAXIS, 
# EXTEND, ORIGIN, DATE, IRAF_TLM, OBJECT and END; 
# 'IM_NDIM(im)', DATAMIN and DATAMAX will be put out
# only if the LIMTIME > MTIME.
# would  take care of NAXISi. For an Extension unit, the cards are:
# XTENSION, BITPIX, NAXIS, PCOUNT, GCOUNT, ORIGIN, DATE, INHERIT,
# EXTNAME, IRAF_TLM, OBJECT and END; IM_NDIM(im) takes care of
# NAXISi. Same as above for DATAMIN, DATAMAX.
# If these cards are in the main header, reduce the number of
# mandatory cards that are going to be created at closing time
# (in fitupdhdr).
	  
procedure fxf_mandatory_cards (im, nheader_cards)

pointer im			#I im structure
int	nheader_cards		#O Number of mandatory cards in header.

pointer ua
int	ncards, rp, fit, acmode
int	idb_findrecord()

begin
	ua = IM_USERAREA(im)
	fit = IM_KDES(im)

	if (FIT_NEWIMAGE(fit) == YES)		# create a PHU
	    ncards = 9 + IM_NDIM(im) 
	else					# create an EHU
	    ncards = 12 + IM_NDIM(im) 

	if (idb_findrecord (im, "PCOUNT", rp) > 0) {
	    if (FIT_XTENSION(fit) == YES) 
	        ncards = ncards - 1
	    else
	        call fxf_filter_keyw (im, "PCOUNT")
	}
	if (idb_findrecord (im, "GCOUNT", rp) > 0) {
	    if (FIT_XTENSION(fit) == YES) 
	        ncards = ncards - 1
	    else
	        call fxf_filter_keyw (im, "GCOUNT")
	}
	if (idb_findrecord (im, "EXTNAME", rp) > 0) {
	    if (FIT_XTENSION(fit) == YES) 
	        ncards = ncards - 1
	    else
	        call fxf_filter_keyw (im, "EXTNAME")
	}
	if (idb_findrecord (im, "INHERIT", rp) > 0) {
	    if (FIT_XTENSION(fit) == YES) 
	        ncards = ncards - 1
	    else
	        call fxf_filter_keyw (im, "INHERIT")
	}
	if (idb_findrecord (im, "EXTEND", rp) > 0)  {
	    if (FIT_XTENSION(fit) == NO)  {
	        ncards = ncards - 1
	    } else {
		# Delete the keyword from the UA because EXTEND is not
		# recommended in XTENSION units.

	        call fxf_filter_keyw (im, "EXTEND")
	    }
	}

	if (idb_findrecord (im, "ORIGIN", rp) > 0)
	    ncards = ncards - 1
	if (idb_findrecord (im, "DATE", rp) > 0 ) 
	    ncards = ncards - 1
	if (idb_findrecord (im, "IRAF-TLM", rp) > 0) 
	    ncards = ncards - 1
	if (idb_findrecord (im, "OBJECT", rp) > 0) 
	    ncards = ncards - 1

	# See if we need to add one more mandatory card when an EXTVER value
	# was specified when appending a new extension.

	if (FIT_NEWIMAGE(fit) == NO && idb_findrecord(im,"EXTVER",rp) == 0) {
	    # Keyword does not exist.
	    acmode = IM_ACMODE(im)
	    if ((acmode == NEW_IMAGE || acmode == NEW_COPY) && 
		FKS_EXTVER(fit) != INDEFL )
	    ncards = ncards + 1
	}

	# We want to keep BSCALE and BZERO in the UA in case we are 
	# editing the values. Is up to the user or application
	# responsability to deal with the change in pixel value when reading.
	# If we are reading pixels the values will change according to the
	# input BSCALE and BZERO. If we are adding BSCALE and BZERO before 
	# accessing any pixels, these will get scale. If adding or
	# changing right before closing the image, the pixel value will be 
	# unchanged.

	# See if BSCALE and BZERO are in the UA for ushort, otherwise
	# increase the number.

	if (IM_PIXTYPE(im) == TY_USHORT) {
	    if (idb_findrecord (im, "BSCALE", rp) == 0) 
	        ncards = ncards + 1
	    if (idb_findrecord (im, "BZERO", rp) == 0) 
	        ncards = ncards + 1
        }
	nheader_cards = ncards
end


# FXF_OVERWRITE_UNIT -- Overwrite an existent extension. A temporary file
# is created that contains the current file upto the extension before the
# one to be overwrite.

procedure fxf_overwrite_unit (fit, im)

pointer	fit			#I Fits descriptor
pointer	im			#I Image descriptor

pointer sp, file, mii
int	pixoff, compress, blklen, sz_fitfile, i, group, filesize
int	junk, in_fd, out_fd, nblocks, nk, hdr_size, sz_pixfile
extern  fxfzop(), fxfzrd(), fxfzwr(), fxfzwt(), fxfzst(), fxfzcl()
int	fnroot(), open(), read(), fxf_totpix(), strncmp(), itoc()
int	strlen(), fopnbf(), fstatl(), fxf_header_size()

include	<szpixtype.inc>

errchk	syserr, syserrs
define  err_ 91

begin
	group = FIT_GROUP(fit)

	# Do not overwrite extensions that are not IMAGE.
	if (group != 0 && strncmp (FIT_EXTTYPE(fit), "IMAGE", 5) != 0 &&
		strncmp (FIT_EXTTYPE(fit), "SIMPLE", 6) != 0) {

	    call syserr (SYS_FXFOVRBEXTN)
	    return
	}

	call smark (sp)
	call salloc (file, SZ_FNAME, TY_CHAR)
	call salloc (mii, FITS_BLOCK_CHARS, TY_INT)
	
	junk = fnroot (IM_HDRFILE(im), Memc[file], SZ_FNAME)

	# Keep the temporary filename in IM_PIXFILE(im).
	call mktemp (Memc[file], IM_PIXFILE(im), SZ_PATHNAME)
	call strcat (".fits", IM_PIXFILE(im), SZ_PATHNAME)

	# If we want to overwrite the first group there is nothing
	# to copy first.

	if (group != 0) {
	    # Copy from the old file up to hdr_off[group] into a temporary file.
	    in_fd = open (IM_HDRFILE(im), READ_ONLY, BINARY_FILE)
	    out_fd = open (IM_PIXFILE(im), NEW_FILE, BINARY_FILE)
	    nblocks = Memi[FIT_HDRPTR(fit)+group]/ FITS_BLOCK_CHARS
	    do nk = 1, nblocks {
		junk = read (in_fd, Memi[mii], FITS_BLOCK_CHARS)
		call write (out_fd, Memi[mii], FITS_BLOCK_CHARS)
	    }
	    call close (in_fd)
	    call close (out_fd)
	}

	FIT_NAXIS(fit) = IM_NDIM(im)
	call amovi (IM_LEN(im,1), FIT_LENAXIS(fit,1), IM_NDIM(im))
	
	FIT_TOTPIX(fit) = fxf_totpix(im)

	# Do not allow BSCALE and BZERO in the UA when making a new copy or
	# new image if bitpix is negative. Except for ushort.

	if (IM_PIXTYPE(im) != TY_USHORT) {
	    call fxf_filter_keyw (im, "BSCALE")
	    call fxf_filter_keyw (im, "BZERO")
	}

	# The new copy header should not have the following keywords:
	# GROUPS, PSIZE and that could come from a GEIS file. 

	call fxf_discard_keyw (im)
	hdr_size = fxf_header_size (im)

	# Reset the scaling parameter because in NEW_COPY mode there
	# should not be scaled pixels. The previous call will get these
	# values from the input image.

	FIT_BSCALE(fit) = 1.0d0
	FIT_BZERO(fit) = 0.0d0

        call fpathname (IM_PIXFILE(im), Memc[file], SZ_PATHNAME)
	call strcat("_", Memc[file], SZ_PATHNAME)
	i = strlen(Memc[file])
	junk = itoc (fit, Memc[file+i], SZ_PATHNAME) 

	# The pixel file needs to be a multiple of 1440 chars.
	sz_pixfile = fxf_totpix(im) * pix_size[IM_PIXTYPE(im)]
	sz_pixfile = FITS_LEN_CHAR(sz_pixfile)
	sz_fitfile = sz_pixfile + hdr_size

	if (group == 0)
	   call falloc (IM_PIXFILE(im), sz_fitfile)

	FIT_IM(fit) = im
	iferr (IM_PFD(im) = fopnbf (Memc[file], READ_WRITE,
		fxfzop, fxfzrd, fxfzwr, fxfzwt, fxfzst, fxfzcl)) {

	    IM_PFD(im) = NULL
	    goto err_
	}

	filesize = fstatl (IM_PFD(im), F_FILESIZE)
	FIT_EOFSIZE(fit) = filesize + 1
	# Now write a blank header.
	if (group != 0) {
	    call amovki (0, Memi[mii], FITS_BLOCK_CHARS)
	    nblocks = hdr_size/FITS_BLOCK_CHARS
	    FIT_HFD(fit) = -1

	    call seek (IM_PFD(im), EOF)
	    do nk = 1, nblocks 
		call write (IM_PFD(im), Memi[mii], FITS_BLOCK_CHARS)
	    
	    pixoff = filesize + hdr_size + 1
	    filesize = filesize + sz_fitfile
	} else
	   pixoff = hdr_size + 1


	FIT_PIXOFF(fit) = pixoff
	IM_HFD(im) = NULL

	blklen = 1
	compress = YES
	call imioff (im, pixoff, compress, blklen)

        FIT_PFD(fit) = IM_PFD(im)
	FIT_HFD(fit) = IM_HFD(im)
		 
	call sfree (sp)
	return
err_
	call syserr (SYS_FXFOVRTOPN)
        call sfree (sp)
end


# TOTPIX -- Calculate the total number of pixels in the image.

int procedure fxf_totpix (im) 

pointer im			#I image descriptor
int	i, pix, ndim

begin
	ndim = IM_NDIM(im)
	if (ndim == 0)
	    return (0)

	pix = IM_LEN(im,1)
	do i = 2, ndim
	    pix = pix * IM_LEN(im,i)

	return (pix)
end


# FXF_DISCARD_FITS_KEYW -- Exclude certain keywords from a new copy image.

procedure fxf_discard_keyw (im)

pointer im 			#I image descriptor
pointer fit

begin
	fit = IM_KDES(im)

	call fxf_filter_keyw (im, "GROUPS")
	call fxf_filter_keyw (im, "PSIZE")
	call fxf_filter_keyw (im, "BLOCKED")
	call fxf_filter_keyw (im, "IRAFNAME")
	call fxf_filter_keyw (im, "IRAF-BPX")
	call fxf_filter_keyw (im, "IRAFTYPE")

	if (FIT_NEWIMAGE(fit) == NO)
	    call fxf_filter_keyw (im, "EXTEND")

	# Create a PHU.
	if (FIT_NEWIMAGE(fit) == YES) {
	    call fxf_filter_keyw (im, "PCOUNT")
	    call fxf_filter_keyw (im, "GCOUNT")
	    call fxf_filter_keyw (im, "INHERIT")
	    call fxf_filter_keyw (im, "EXTNAME")
	    call fxf_filter_keyw (im, "EXTVER")
	    call fxf_filter_keyw (im, "EXTLEVEL")
	}
end


# FXF_FILTER_KEYW -- Delete the names keyword from the userarea.

procedure fxf_filter_keyw (im, key)

pointer	im	 	 #I image descriptor
char	key[ARB] 	 #I keyword name to delete from USERAREA.

pointer rp
int     off
int     idb_findrecord(), stridxs()

begin
	# Verify that the named user field exists.
	if (idb_findrecord (im, key, rp) <= 0)
	    return

        # Delete the field.
        off = stridxs ("\n", Memc[rp])
	if (off > 0)
	    call strcpy (Memc[rp+off], Memc[rp], ARB)
	else
	    Memc[rp] = EOS
end


# FXF_FALLOC -- Preallocate space on disk by writing blanks.

procedure fxf_falloc (fname, size)

char	fname[ARB]	#I filename
int	size      	#I size to preallocate in chars

pointer sp, cp
int	nb,i, fd
errchk  open, write
int	open()

begin
	call smark (sp)
	call salloc (cp, FITS_BLOCK_CHARS, TY_CHAR)
	
        call amovkc (' ', Memc[cp], FITS_BLOCK_CHARS)
	nb = size / FITS_BLOCK_CHARS
	fd = open (fname, NEW_FILE, BINARY_FILE)

	do i = 1, nb
	    call write (fd, Memc[cp], FITS_BLOCK_CHARS)

	call flush (fd)
	call close (fd)
	call sfree (sp)
end


# FXF_CKH_NDIM -- Check that the application has indeed set the number
# of dimension, otherwise count the axes.

procedure fxf_chk_ndim (im)

pointer im		#I imio descriptor
int	ndim		#I number of dimension for image

begin
	ndim = IM_NDIM(im)

	# If ndim was not explicitly set, compute it by counting the number
	# of nonzero dimensions.

	if (ndim == 0) {
	    for (ndim=1;  IM_LEN(im,ndim) > 0 && ndim <= IM_MAXDIM; ndim=ndim+1)
	        ;
	    ndim = ndim - 1
	    IM_NDIM(im) = ndim
	}
end

        
# FXF_NOT_INCACHE -- Procedure to find whether the file is in the
# cache. It could happen that the slot with the entry might have been
# freed to make room for another file. We want to have valid pointers
# for FIT_CACHEHDR and FIT_CACHELEN since the calling routine will use them.

procedure fxf_not_incache (im)

pointer im                      #I image descriptor

int     cindx, group, sfit[4]
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

            if (streq (Memc[hdrfile], rf_fname[1,cindx])) {
                call sfree (sp)
                return      
            }
        }
        sfit[1]= FIT_NAXIS(fit)
        sfit[2] = FIT_INHERIT(fit)
        sfit[3] = FIT_PLMAXLEN(fit)
        sfit[4] = IM_CTIME(im)  

        group = max (0, FIT_GROUP(fit))

        call fxf_prhdr(im,group)

        FIT_NAXIS(fit) = sfit[1]
        FIT_INHERIT(fit) = sfit[2]
        FIT_PLMAXLEN(fit) = sfit[3]
        IM_CTIME(im) = sfit[4]

        call sfree (sp)
        return
end

