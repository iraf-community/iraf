include <mach.h>
include <imio.h>
include	<imhdr.h>
include "wfits.h"

# WFT_WGF_XDIM -- Write multigroup GEIS file as on fits file with an
# extra dimension for the groups and put the gp values into
# a temporary table file to be read after the image data has been entirely
# transfer to fits file.

procedure wft_wgi_xdim (im, fits_file, fits, fits_fd)

pointer	im		# image descriptor
char	fits_file[SZ_FNAME]
pointer	fits		# fits memory descriptor
int	fits_fd		# output fits file descriptor

size_t	sz_val
pointer	buf, ext, tp, cp, tempbuf
size_t	npix, nrecords, npix_record
long	v[IM_MAXDIM]
long	nlines, stat, j, l_val
int	gn, ndim, ngroups, pcount, i
real	datamax, datamin
double  dbscale
char	root[SZ_FNAME], extn[SZ_EXTN]
char	line[SZ_LINE], tname[SZ_FNAME]

int	gi_gstfval(), strcmp()
long	wft_get_image_line()
errchk  gi_opengr, wft_get_image_line, wft_ieee, giopn_table, wft_write_pixels
errchk  gi_ggpv, tbeptt, wft_write_last_record, tab_write_header
errchk  tab_write_data
include "wfits.com"

include	<nullptr.inc>

data	tempbuf /NULL/

begin

	ngroups = gi_gstfval(im, "GCOUNT")
	pcount  = gi_gstfval(im, "PCOUNT")

	# Reset dimensionality back to the original values
	# The values were changed in gi_imsetup
	if (ngroups > 1) {
	    ndim = IM_NDIM(im)
	    IM_NDIM(im) = ndim - 1
	    IM_LEN(im, ndim) = 1
	}
	
	npix = IM_LEN(im,1)
	nlines = 1
	do i = 2, IM_NDIM(im, i)
	    nlines = nlines * IM_LEN(im,i)
	
	# Open table to contain the gpb values
	# make a temporary filename in tmp for the table

	if (pcount > 0) {
	    call mktemp ("tmp$gf", tname, SZ_FNAME)
	    call strcat (".tab", tname, SZ_FNAME)

	    sz_val = LEN_EXTENSION
	    call calloc (ext, sz_val, TY_STRUCT)
	    sz_val = pcount
	    call calloc (EXT_PCUNDEF(ext), sz_val, TY_BOOL)
	    call calloc (EXT_PCOL(ext), sz_val, TY_POINTER)
	    call giopn_table (tname, im, tp, COLPTR(ext))
	}

	npix_record = len_record * FITS_BYTE / FITS_BITPIX(fits)
	if (ieee == YES && PIXTYPE(im) == TY_DOUBLE)
	    call wft_init_write_pixels (npix_record, TY_DOUBLE, 
					FITS_BITPIX(fits))
	else
	    call wft_init_write_pixels (npix_record, TY_LONG, 
					FITS_BITPIX(fits))

	if (tempbuf != NULL)
	    call mfree (tempbuf, TY_CHAR)
	if (ieee == YES && PIXTYPE(im) == TY_DOUBLE)
	    call malloc (tempbuf, SZ_DOUBLE * npix, TY_CHAR)
	else
	    call malloc (tempbuf, SZ_LONG * npix, TY_CHAR)


	# Loop through the groups
	do gn = 1, ngroups {

	    l_val = 1
	    sz_val = IM_MAXDIM
	    call amovkl (l_val, v, sz_val)

	    call gi_opengr (im, gn, datamin, datamax, NULLPTR)

	    do j = 1, nlines {

		# Get an image line.
		stat = wft_get_image_line (im, buf, v, PIXTYPE(im))
		if (stat == EOF )
		    return
		if (stat != npix) {
		    call flush (STDOUT)
		    call error (10, "WRT_IMAGE: Error writing IRAF image.")
		}

		# Scale the line.
		if (ieee == YES) {
		    call wft_ieee (buf, tempbuf, npix, PIXTYPE(im))
		} else {
		    if (SCALE(fits) == YES) {
			dbscale = 1.0d0 / BSCALE(fits)
			# arg2: incompatible pointer
			call wft_scale_line (buf, Memc[tempbuf], npix, 
					     dbscale, -BZERO(fits), 
					     PIXTYPE(im))
		    } else {
			# arg2: incompatible pointer
			call wft_long_line (buf, Memc[tempbuf], npix, 
					    PIXTYPE(im))
		    }
		}

		# write the pixels
		call wft_write_pixels (fits_fd, Memc[tempbuf], npix)
	    }

	    # Read gpb	
	    if (pcount > 0)
		cp = EXT_PCOL(ext)

	    do i = 1, pcount {
		# Get group parameter value in a string buffer
		call gi_ggpv (im, i, line)
		# write value to table
		l_val = gn
		call tbeptt (tp, Memp[cp+i-1], l_val, line)
	    }
	    if (datamin < datamax)
		IM_LIMTIME(im) = IM_MTIME(im) + 1
	    else
		IM_LIMTIME(im) = IM_MTIME(im) - 1
	}
	call mfree (tempbuf, TY_CHAR)

	# write the final record
	call wft_write_last_record (fits_fd, nrecords)
	if (long_header == YES) {
	    call printf ("%d  Data records(s) written\n")
	    call pargz (nrecords)
	}

	# Write now the temporary table
	if (pcount > 0) {

	    call iki_parse (IRAFNAME(fits), root, extn)
	    if (strcmp (extn, "hhh") == 0)
		call tbtext (root, IRAFNAME(fits), SZ_FNAME)
	    else 
		call strcat (".tab", IRAFNAME(fits), SZ_FNAME)

	    # Do not write tables in ieee format nor in binary
	    ieee = NO
	    ext_type = TABLE

	    # Reset flag to not write a dummy header on the output file
	    # i.e. Append the table
	    first_time = NO
	    call  strcpy (IRAFNAME(fits), EXTNAME(ext), SZ_FNAME)
	    call tab_write_header (tp, fits_file, ext, fits_fd)
	    if (short_header == YES)
		call prtbinfo_key (tp, IRAFNAME(fits))
	    call tab_write_data (tp, ext, fits_fd)
	    
	    call tbtclo (tp)
	    # Delete temporary table name
	    call delete (tname)
	    call mfree (EXT_PCOL(ext), TY_POINTER)
	    call mfree (EXT_PCUNDEF(ext), TY_BOOL)
	    call mfree (ext, TY_STRUCT)
	    ext_type = NULL
	    
	}

	# Reset extension flag. It was set to YES in wft_xdim_card
	extensions = NO
end

include	<imhdr.h>
include <fset.h>

# WFT_GF_OPENGR -- Procedure to open next group of the Geis file.
# At the same time we open the next FITS file, since we are writing
# one FITS file per group.

procedure wft_gi_opengr (im, gn, iraf_file, fits, fits_file, fits_fd)

pointer	im		# image descriptor
int	gn		# group number to offset to.
char	iraf_file[SZ_FNAME]	# Input filename
pointer	fits		# fits memory structure
char	fits_file[SZ_FNAME]	# Current output filename
int	fits_fd		# output file pointer for the first file

size_t	chars_rec
int	ngroups, tape, len
long	dev_blk
real	datamax, datamin
char	temp[SZ_FNAME], lb

int	gi_gstfval(), mtfile(), open(), mtopen(), stridx()
long	fstatl()

include	<nullptr.inc>

include "wfits.com"
define	prt_ 99

begin

	ngroups = gi_gstfval (im, "GCOUNT")

	call gi_opengr (im, gn, datamin, datamax, NULLPTR)
	
	# Set the IMIO min/max fields.  If the GPB datamin >= datamax the
	# values are invalidated by setting IM_LIMTIME to before the image
	# modification time.
	IM_MIN(im) = datamin
	IM_MAX(im) = datamax
	if (datamin < datamax)
	    IM_LIMTIME(im) = IM_MTIME(im) + 1
	else
	    IM_LIMTIME(im) = IM_MTIME(im) - 1

	# Reset OBJECT keyword value to reflect the group being
	# opened. (OBJECT has at this moment the value of IM_TITLE(im))
	# Notice that we are assuming that the initial name has
	# left bracket.
	lb = '['
	len = stridx (lb, OBJECT(im))
	call strcpy (OBJECT(im), temp, SZ_FNAME)
	call sprintf (temp[len], SZ_FNAME, "[%d]")
	call pargi(gn)
	call strcpy (temp, OBJECT(im), SZ_FNAME)
	
	# Append group information for the iraf_file in the IRAFNAME
	# buffer.
	if (gn == 1) {
	    call sprintf (IRAFNAME(fits), SZ_FNAME, "%s[1/%d]")
	    call pargstr (iraf_file)
	    call pargi (ngroups)
	} else {
	    call sprintf (IRAFNAME(fits), SZ_FNAME, "%s[%d]")
	    call pargstr (iraf_file)
	    call pargi (gn)
	}
	tape = mtfile(fits_file)
	# If we want to create IMAGE Xtensions we need to write on the
	# same FITS file, so go to print header info only.
	if (extensions == YES && tape == NO) {
	    blkfac = 1
	    #        if (gn == 1) fits_fd = open (fits_file, NEW_FILE, BINARY_FILE)
	    goto prt_
	}

        if (tape == YES) {
	    call sprintf (fits_file[stridx(lb,fits_file)], SZ_FNAME, "%s")
	    call pargstr ("[EOT]")
	    if (blkfac > 10)
		chars_rec = (blkfac * FITS_BYTE) / (SZB_CHAR * NBITS_BYTE)
	    else
		chars_rec = (blkfac * len_record * FITS_BYTE) / (SZB_CHAR *
								 NBITS_BYTE)
	    # mt is already open in fits_write.x

	    if (gn != 1 && extensions == NO) {
		call close (fits_fd)
		fits_fd = mtopen (fits_file, WRITE_ONLY, chars_rec)
	    }
	    dev_blk = fstatl (fits_fd, F_MAXBUFSIZE)
	    if (dev_blk != 0 && chars_rec > dev_blk) {
		call flush (STDOUT)
		call error (0, "Blocking factor too large for tape drive")
	    }
	    if (long_header == YES) {
		call printf ("File %d:")
		call pargi(file_number)
	    }
        } else if (extensions == NO ) {
	    blkfac = 1
	    if (gn > 1 )  call close (fits_fd)
		fits_fd = open (fits_file, NEW_FILE, BINARY_FILE)
        }
prt_
        if (long_header == YES) {
	    call printf ("%s[%d]")
	    call pargstr(iraf_file)
	    call pargi (gn)
	    call printf (" -> %s ")
	    call pargstr (temp)
        } 

	if (long_header == YES)
	    call printf ("\n")
end

include <tbset.h>
include "dfits.h"

# PRINT_KEY - Searches in the IM_USERAREA for a card that matchs a given
# keyword, extracts the data from that card and prints it according to a
# given format. Leading spaces, single quotes and comments are removed from
# the data.

procedure prtbinfo_key (tp, irafname)

pointer tp
char	irafname[ARB]

char	str[LEN_CARD]		# card data string
char    line[SZ_LINE]
int	nk, nch

int	strlen(), strmatch(), tbpsta()
long	tbpstl()
include "wfits.com"
include	"dfits.com"

begin
	# Search the keyword in the card table
	line[1] = EOS
        do nk = 1, nkeywords {
	    if (strmatch (Memc[key_table[nk]], "FILENAME") > 0)
		call strcpy (irafname, str, LEN_CARD)
	    else if (strmatch (Memc[key_table[nk]], "FITSNAME") > 0)
		if (ext_type == BINTABLE)
		    call strcpy ("  BINTABLE", str, LEN_CARD)
		else
		    call strcpy ("  TABLE", str, LEN_CARD)
	    else if (strmatch (Memc[key_table[nk]], "DIMENS") > 0) {
		str[1] = EOS
		call sprintf (str, LEN_CARD, "%dCx%dR")
		call pargi (tbpsta (tp, TBL_NCOLS))
		call pargl (tbpstl (tp, TBL_NROWS))
	    } else if (strmatch (Memc[key_table[nk]], "BITPIX") > 0) {
		if (ext_type == BINTABLE)
		    call strcpy ("8bin ", str, 4)
	        else
		    call strcpy ("8tab ", str, 4)
	    } else
		str[1] = EOS 
	    call print_string (line, str, Memc[fmt_table[nk]], opt_table[nk])
	}
	call printf ("%80.80s\n")
	call pargstr(line)
	nch = strlen (line)
	line[nch+1] = '\n'
	call put_in_log (line)
end
