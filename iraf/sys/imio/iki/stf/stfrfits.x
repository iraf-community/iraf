# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<imhdr.h>
include	<imio.h>
include	<finfo.h>
include	<fset.h>
include	"stf.h"

# STF_RFITSHDR -- Read a STF FITS image header, processing all reserved GPB
# definition keywords into the STF descriptor in the image descriptor, and
# saving the remaining cards (excluding cards which GPB keyword names) in
# in a save buffer.
# 
# This routine implements a simple cache of FITS headers.  If a given header
# is already in the cache and the cached entry is up to date, the cached
# spool file containing the user FITS cards and the saved STF descriptor are
# returned immediately without need to access the header file on disk.
# Otherwise, the new header is read into the oldest cache slot and the cached
# entry returned in the usual fashion.  Any modifications to the header file
# which affect the file modify date will invalidate the cached entry.  Note
# that multiple processes may cache the same header, so it is not permitted
# to modify the cached entry once the header file has been read.
# 
# The following reserved keywords are recognized:
# 
# 	SIMPLE BITPIX DATATYPE NAXIS* GROUPS GCOUNT PCOUNT PSIZE 
# 	PTYPE* PDTYPE* PSIZE* 
# 
# All unrecognized cards, including HISTORY and COMMENT cards, blank lines,
# and any other garbage in the header, are preserved in the user area of the
# IMIO descriptor (i.e., in the spoolfile).  Certain of the standard reserved
# cards (GROUPS, GCOUNT, etc.) are saved in the IMIO user area for the sake
# of the user, although the real values of these parameters are maintained only
# in the STF descriptor.

procedure stf_rfitshdr (im, fits, fitslen)

pointer	im				#I image descriptor
pointer	fits				#O pointer to saved FITS cards
int	fitslen				#O length of FITS save area

long	fi[LEN_FINFO]
pointer	sp, pp, stf, o_stf, lbuf, op, hdrfile
int	in, index, nchars, spool, slot, user, i

bool	streq()
long	clktime(), fstatl()
int	envgeti(), stf_ctype(), finfo(), getline(), open(), stropen()
errchk	getline, putline, syserrs, open, seek, calloc, realloc
errchk	fpathname, malloc, stf_copyfits

bool	initialized			# CACHE definitions...
bool	reload				# reload cache
int	rf_refcount			# reference count
int	rf_cachesize			# number of cache slots
pointer	rf_stf[MAX_CACHE]		# STF descriptor
int	rf_lru[MAX_CACHE]		# lowest value is oldest slot
long	rf_time[MAX_CACHE]		# time when entry was cached
long	rf_mtime[MAX_CACHE]		# modify time of file in cache
int	rf_fits[MAX_CACHE]		# FITS data
int	rf_fitslen[MAX_CACHE]		# size of data area
char	rf_fname[SZ_PATHNAME,MAX_CACHE]	# header file pathname
data	initialized /false/

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (hdrfile, SZ_PATHNAME, TY_CHAR)

	# Initialize the header file cache on the first call.
	if (!initialized) {
	    rf_refcount = 0
	    do i = 1, MAX_CACHE
		rf_stf[i] = 0

	    iferr (rf_cachesize = envgeti (ENV_STFCACHE))
		rf_cachesize = DEF_CACHE
	    if (rf_cachesize > MAX_CACHE) {
		call eprintf ("A maximum of %d STF headers may be cached\n")
		    call pargi (MAX_CACHE)
		rf_cachesize = MAX_CACHE
	    } else if (rf_cachesize <= 0)
		rf_cachesize = 0
		
	    initialized = true
	}

	rf_refcount = rf_refcount + 1
	o_stf = IM_KDES(im)
	reload = false
	slot = 1

	# Get file system info on the desired header file.
	call fpathname (IM_HDRFILE(im), Memc[hdrfile], SZ_PATHNAME)
	if (finfo (Memc[hdrfile], fi) == ERR)
	    call syserrs (SYS_FOPEN, IM_HDRFILE(im))

	repeat {
	    # Search the header file cache for the named image.
	    do i = 1, max(1,rf_cachesize) {
		if (rf_stf[i] == NULL) {
		    slot = i
		    next
		}

		if (streq (Memc[hdrfile], rf_fname[1,i])) {
		    # File is in cache; is cached entry still valid?
		    if (FI_MTIME(fi) != rf_mtime[i]) {
			# File modify date has changed, reuse slot.
			slot = i
			break

		    } else if (!reload && clktime(rf_time[i]) < 2) {
			# The file modify date has not changed, but the cache
			# was loaded within the last clock "tick" (second),
			# so we cannot be sure that the file was not modified.
			# The cache must be reloaded, but set a flag so that
			# rf_time is not changed, so that when the cache entry
			# ages sufficiently it will be considered valid.

			reload = true
			slot = i
			break

		    } else {
			# Return the cached header.
			rf_lru[i] = rf_refcount
			call amovi (STF_CACHE(rf_stf[i]), STF_CACHE(o_stf),
			    STF_CACHELEN(rf_stf[i]))
			fits = rf_fits[i]
			fitslen = rf_fitslen[i]

			# Invalidate entry if cache is disabled.
			if (rf_cachesize <= 0)
			    rf_time[i] = 0

			call sfree (sp)
			return 				# IN CACHE
		    }

		} else {
		    # Keep track of least recently used slot.
		    if (rf_lru[i] < rf_lru[slot])
			slot = i
		}
	    }

	    # Either the image header is not in the cache, or the cached
	    # entry is invalid.  Prepare the given cache slot and read the
	    # header into it.

	    # Free old save buffer and descriptor.
	    if (rf_stf[slot] != NULL) {
		call mfree (rf_stf[slot], TY_STRUCT)
		call mfree (rf_fits[slot], TY_CHAR)
	    }

	    # Open the header file.
	    if (IM_HFD(im) == NULL)
		in = open (Memc[hdrfile], READ_ONLY, TEXT_FILE)
	    else {
		in = IM_HFD(im)
		call seek (in, BOFL)
	    }

	    # Allocate a spool file for the FITS data.
	    call sprintf (rf_fname[1,slot], SZ_PATHNAME, "STFHC#%d")
		call pargi (slot)
	    spool = open (rf_fname[1,slot], READ_WRITE, SPOOL_FILE)
	    call fseti (spool, F_BUFSIZE, FI_SIZE(fi))

	    # Allocate cache version of STF descriptor.
	    call calloc (stf, LEN_STFDES, TY_STRUCT)

	    # Initialize the cache entry.
	    call strcpy (Memc[hdrfile], rf_fname[1,slot], SZ_PATHNAME)
	    rf_stf[slot] = stf
	    rf_lru[slot] = rf_refcount
	    rf_mtime[slot] = FI_MTIME(fi)
	    if (!reload)
		rf_time[slot] = clktime (0)
	    reload = true

	    # Read successive lines of the FITS header.  Process reserved
	    # keywords into the STF descriptor and spool the remaining cards
	    # to the fits spool file.

	    repeat {
		# Get the next input line.
		nchars = getline (in, Memc[lbuf])
		if (nchars == EOF)
		    break

		# Block it out to 80 chars (plus newline) if it is not already.
		if (nchars != FITS_RECLEN + 1) {
		    for (op=nchars;  op <= FITS_RECLEN;  op=op+1)
			Memc[lbuf+op-1] = ' '
		    Memc[lbuf+FITS_RECLEN]   = '\n'
		    Memc[lbuf+FITS_RECLEN+1] = EOS
		}
		    
		# Process the header card.
		switch (stf_ctype (Memc[lbuf], index)) {
		case KW_BITPIX:
		    call stf_geti (Memc[lbuf], STF_BITPIX(stf))
		case KW_DATATYPE:
		    call stf_gets (Memc[lbuf], STF_DATATYPE(stf), SZ_DATATYPE)
		case KW_END:
		    break
		case KW_GCOUNT:
		    call stf_geti (Memc[lbuf], STF_GCOUNT(stf))
		case KW_GROUPS:
		    call stf_getb (Memc[lbuf], STF_GROUPS(stf))
		case KW_NAXIS:
		    call stf_geti (Memc[lbuf], STF_NAXIS(stf))
		case KW_NAXISN:
		    call stf_geti (Memc[lbuf], STF_LENAXIS(stf,index))
		case KW_PCOUNT:
		    call stf_geti (Memc[lbuf], STF_PCOUNT(stf))
		case KW_PDTYPE:
		    pp = STF_PDES(stf,min(index,MAX_PCOUNT))
		    call stf_gets (Memc[lbuf], P_PDTYPE(pp), SZ_PDTYPE)
		case KW_PSIZE:
		    call stf_geti (Memc[lbuf], STF_PSIZE(stf))
		case KW_PSIZEN:
		    pp = STF_PDES(stf,min(index,MAX_PCOUNT))
		    call stf_geti (Memc[lbuf], P_PSIZE(pp))
		case KW_PTYPE:
		    pp = STF_PDES(stf,min(index,MAX_PCOUNT))
		    call stf_gets (Memc[lbuf], P_PTYPE(pp), SZ_PTYPE)
		    call stf_getcmt (Memc[lbuf], P_COMMENT(pp), SZ_COMMENT)
		case KW_SIMPLE:
		    ;
		default:
		    call putline (spool, Memc[lbuf])
		}
	    }

	    # Close the header file if opened locally.
	    if (IM_HFD(im) == NULL)
		call close (in)

	    # Free any unneeded space in the STF descriptor.
	    if (STF_PCOUNT(stf) > 0) {
		call realloc (stf,
		    LEN_STFBASE + STF_PCOUNT(stf)*LEN_PDES, TY_STRUCT)
		rf_stf[slot] = stf
	    }

	    # Filter the spooled FITS cards to delete any cards which redefine
	    # GPB keywords.  Store the filtered FITS data in the cache.

	    call seek (spool, BOFL)
	    nchars = fstatl (spool, F_FILESIZE)
	    call malloc (fits, nchars, TY_CHAR)
	    user = stropen (Memc[fits], nchars, NEW_FILE)
	    call stf_copyfits (stf, spool, NULL, user)

	    rf_fits[slot] = fits
	    rf_fitslen[slot] = fstatl (user, F_FILESIZE)
	    call close (user)
	    call close (spool)
	}
end
