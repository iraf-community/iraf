# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <error.h>
include <imhdr.h>
include <fset.h>
include <plset.h>
include	"rfits.h"

define	MAX_RANGES	100		# the maximum number of ranges

# RFT_READ_FITZ -- Convert a FITS file. An EOT is signalled by returning EOF.

int procedure rft_read_fitz (fitsfile, iraffile, pl, file_number)

char	fitsfile[ARB]		# FITS file name
char	iraffile[ARB]		# root IRAF file name
pointer	pl			# pointer to the file/extensions list
int	file_number		# the current file number

bool	strne()
int	fits_fd, stat, min_lenuserarea, ip, len_elist, oshort_header
int	olong_header, ext_count, ext_number, max_extensions, naxes
pointer	im, gim, sp, fits, axes, extensions, imname, gimname, gfname, str
pointer	himname
int	rft_read_header(), mtopen(), immap(), strlen(), envfind(), ctoi()
int	rft_ext_skip()
real	asumi()
errchk	smark, sfree, salloc, rft_read_header, rft_read_image, rft_find_eof()
errchk	rft_scan_file, mtopen, immap, imdelete, close, imunmap

include	"rfits.com"

begin
	# Open input FITS data.
	fits_fd = mtopen (fitsfile, READ_ONLY, 0)

	# Allocate memory for the FITS data structure and initialize the file
	# dependent  components of that structure.
	call smark (sp)
	call salloc (fits, LEN_FITS, TY_STRUCT)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (himname, SZ_FNAME, TY_CHAR)
	call salloc (gimname, SZ_FNAME, TY_CHAR)
	call salloc (gfname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Initialize.
	SIMPLE(fits) = NO
	EXTEND(fits) = NO
	GLOBALHDR(fits) = NO
	gim = NULL
	Memc[gfname] = EOS

	# Determine the length of the user area.
	if (envfind ("min_lenuserarea", Memc[imname], SZ_FNAME) > 0) {
	    ip = 1
	    if (ctoi (Memc[imname], ip, min_lenuserarea) <= 0)
		min_lenuserarea = LEN_USERAREA
	    else
		min_lenuserarea = max (LEN_USERAREA, min_lenuserarea)
	} else
	    min_lenuserarea = LEN_USERAREA

	# Store the current values of the header printing options.
	olong_header = long_header
	oshort_header = short_header

	# Get the extensions list for a given line and count the number of
	# extensions files.
	call salloc (axes, 2, TY_INT)
	call pl_gsize (pl, naxes, Memi[axes], stat)
	max_extensions = Memi[axes+1]
	call salloc (extensions, max_extensions, TY_INT)
	Memi[axes] = 1
	Memi[axes+1] = file_number
	call pl_glpi (pl, Memi[axes], Memi[extensions], 1, max_extensions,
	    PIX_SRC)
	len_elist = nint (asumi (Memi[extensions], max_extensions))

	# Loop over the extensions.
	ext_count = 1; stat = BOF
	do ext_number = 1, max_extensions {

	    if (stat == EOF)
		break
	    if (Memi[extensions+ext_number-1] == 0)
		next

	    # Locate the next extension to be read.
	    while (ext_count <= ext_number) {

	        # Create the IRAF image header. If only a header listing is
	        # desired or the image extension is to be skipped then map
		# the scratch image onto DEV$NULL (faster than a real file).
		# If more than one extension is to be read then append the
		# extension number to the input name.

	        if (make_image == NO || ext_count != ext_number) {
	            call strcpy ("dev$null", Memc[imname], SZ_FNAME)
		} else if (len_elist > 1 && ext_count == ext_number) {
		    call sprintf (Memc[imname], SZ_FNAME, "%s%04d")
			call pargstr (iraffile)
			call pargi (ext_number - 1)
		} else
		    call strcpy (iraffile, Memc[imname], SZ_FNAME) 
	        im = immap (Memc[imname], NEW_IMAGE, min_lenuserarea)
		call strcpy (IM_HDRFILE(im), Memc[himname], SZ_FNAME)

	        # Skip any extensions the user does not want. In order to do
	        # this we must read the header to see how big the data array
	        # to be skipped is.
		if (ext_count != ext_number) {

		    # Turn off header printing.
		    long_header = NO
		    short_header = NO

		    # Decode the header and skip the data.
	            iferr {
	                stat = rft_read_header (fits_fd, fits, im, gim)
			if (stat != EOF)
			    stat = rft_ext_skip (fits_fd, fits, im)
			if (stat == EOF) {
			    if (ext_count == 1) {
		                call printf ("File:  %s\n")
		                    call pargstr (fitsfile)
			    } else if (asumi(Memi[extensions],
			        ext_count - 1) < 1.0) {
		                call printf ("File:  %s\n")
		                    call pargstr (fitsfile)
			    }
			    if (ext_count > 1) {
		                call printf ("Extension: %d End of data\n")
		                    call pargi (ext_count - 1)
			    } else
	                        call printf ("    End of data\n")
			} else if (EXTEND(fits) == NO) {
		            call printf ("File: %s\n")
		                call pargstr (fitsfile)
	                    call printf ("Extension: 1 End of data\n")
			}
	            } then {
	                call flush (STDOUT)
	                call erract (EA_WARN)
	            }

		    # Restore the default header printing values.
		    long_header = olong_header
		    short_header = oshort_header

		# Read the extension the user specified. If the extension
		# is not the primary data or IMAGE skip the data and
		# continue.
		} else {

	            # Set up for printing a long or a short header.
	            if (long_header == YES || short_header == YES) {
			if (long_header == YES) {
			    if (ext_number == 1) {
				if (make_image == YES) {
		                    call printf ("File: %s  Image: %s")
		                        call pargstr (fitsfile)
					#call pargstr (Memc[imname])
					call pargstr (Memc[himname])
				} else {
		                    call printf ("File: %s")
		                        call pargstr (fitsfile)
				}
			    } else if (asumi (Memi[extensions],
			        ext_number -1) < 1.0) {
				if (make_image == YES) {
		                    call printf (
				        "File: %s\nExtension: %d  Image: %s")
		                        call pargstr (fitsfile)
		                        call pargi (ext_number - 1)
					#call pargstr (Memc[imname])
					call pargstr (Memc[himname])
				} else {
		                    call printf ("File: %s  Extension: %d")
		                        call pargstr (fitsfile)
		                        call pargi (ext_number - 1)
				}
			    } else {
				if (make_image == YES) {
		                    call printf ("Extension: %d  Image: %s")
		                        call pargi (ext_number - 1)
					#call pargstr (Memc[imname])
					call pargstr (Memc[himname])
				} else {
		                    call printf ("File: %s  Extension: %d")
					call pargstr (fitsfile)
		                        call pargi (ext_number - 1)
				}
			    }
			} else {
			    if (ext_number == 1) {
		                call printf ("File:  %s ")
		                    call pargstr (fitsfile)
			    } else if (asumi (Memi[extensions],
			        ext_number - 1) < 1.0) {
		                call printf ("File:  %s\nExtension: %d ")
		                    call pargstr (fitsfile)
		                    call pargi (ext_number - 1)
			    } else {
		                call printf ("Extension: %d ")
		                    call pargi (ext_number - 1)
			    }
			}
	                if (long_header == YES)
		            call printf ("\n")
	            }
	            call flush (STDOUT)

	            # Read header.  EOT is signalled by an EOF status from
	            # fits_read_header. Create an IRAF image if desired.

	            iferr {
	                stat = rft_read_header (fits_fd, fits, im, gim)
	                if (stat == EOF) {
	                    call printf ("End of data\n")
	                } else if (make_image == YES) {
			    if (XTENSION(fits) == EXT_PRIMARY ||
			        XTENSION(fits) == EXT_IMAGE) {
	                        call rft_read_image (fits_fd, fits, im)
			    } else if (EXTEND(fits) == YES) {
				stat = rft_ext_skip (fits_fd, fits, im)
				if (stat == EOF)
	                    	    call printf ("End of data\n")
			    } else if (EXTEND(fits) == NO && fe > 0.0) {
		                call rft_find_eof (fits_fd)
			    }
			} else {
			    if (EXTEND(fits) == YES) {
				stat = rft_ext_skip (fits_fd, fits, im)
				if (stat == EOF)
	                    	    call printf ("End of data\n")
			    } else if (EXTEND(fits) == NO && fe > 0.0)
		                call rft_scan_file (fits_fd, fits, im, fe)
			}
	            } then {
	                call flush (STDOUT)
	                call erract (EA_WARN)
	            }
		}


		# Deal with the global header issue. Save the global header
		# file name for possible future use.
		if (GLOBALHDR(fits) == YES) {
		    if (gim == NULL && XTENSION(fits) == EXT_PRIMARY) {
		        call mktemp ("tmp$", Memc[gimname], SZ_FNAME)
		        gim = immap (Memc[gimname], NEW_COPY, im)
		        call strcpy (IRAFNAME(fits), Memc[gfname], SZ_FNAME)
		    } else if (IRAFNAME(fits) == EOS)
		        call strcpy (Memc[gfname], IRAFNAME(fits), SZ_FNAME)

		}

	        # Close the output image.
	        call imunmap (im)

	        # Optionally restore the old IRAF name.
	        if (stat == EOF) {
	            call imdelete (Memc[imname])
		    break
	        } else if (make_image == NO || ext_number != ext_count) {
	            call imdelete (Memc[imname])
		} else if (XTENSION(fits) != EXT_PRIMARY && XTENSION(fits) !=
		    EXT_IMAGE) {
	            call imdelete (Memc[imname])
		    if (XTENSION(fits) != EXT_SPECIAL && ext_count ==
		        ext_number)
		        call printf ("    Skipping non-image data\n")
	        } else if (old_name == YES && strlen (IRAFNAME(fits)) != 0) {
	            iferr {
		        call imgimage (IRAFNAME(fits), IRAFNAME(fits), SZ_FNAME)
	                call imrename (Memc[imname], IRAFNAME(fits))
	            } then {
			if (len_elist > 1) {
			    call sprintf (Memc[str], SZ_FNAME, ".%d")
			        call pargi (ext_number - 1)
			    call strcat (Memc[str], IRAFNAME(fits), SZ_FNAME)
	                    iferr (call imrename (Memc[imname],
			        IRAFNAME(fits))) {
		                call printf (
				    "    Cannot rename image %s to %s\n")
		                    #call pargstr (Memc[imname])
		                    call pargstr (Memc[himname])
		                    call pargstr (IRAFNAME(fits))
	                        call flush (STDOUT)
	                        call erract (EA_WARN)
			    } else {
	                	call printf ("    Image %s renamed to %s\n")
		            	    #call pargstr (Memc[imname])
		                call pargstr (Memc[himname])
		                call pargstr (IRAFNAME(fits))
			    }
			} else {
		            call printf ("    Cannot rename image %s to %s\n")
		                #call pargstr (Memc[imname])
		                call pargstr (Memc[himname])
		                call pargstr (IRAFNAME(fits))
	                    call flush (STDOUT)
	                    call erract (EA_WARN)
			}
	            } else {
	                call printf ("    Image %s renamed to %s\n")
		            #call pargstr (Memc[imname])
		            call pargstr (Memc[himname])
		            call pargstr (IRAFNAME(fits))
	            }
	        } else if (EXTEND(fits) == NO && strne (Memc[imname],
		    iraffile)) {
	            iferr {
	                call imrename (Memc[imname], iraffile)
	            } then {
		        call printf ("    Cannot rename image %s to %s\n")
		            #call pargstr (Memc[imname])
		            call pargstr (Memc[himname])
		            call pargstr (iraffile)
	                call flush (STDOUT)
	                call erract (EA_WARN)
	            } else {
	                call printf (
			    "    No FITS extensions Image renamed to %s\n")
		            #call pargstr (Memc[imname])
		            call pargstr (iraffile)
		    }
		}

		if (EXTEND(fits) == YES && XTENSION(fits) == EXT_PRIMARY &&
		    len_elist == 1 && ext_number == 1) {
	            if (short_header == YES || long_header == YES) {
			if (long_header == NO)
			    call printf ("    ")
			call printf (
			    "Warning: FITS extensions may be present\n")
		    }
		}
	        if (long_header == YES)
	            call printf ("\n")

		ext_count = ext_count + 1
		if (EXTEND(fits) == NO || XTENSION(fits) == EXT_SPECIAL)
		    break
	    }

	    if (EXTEND(fits) == NO || XTENSION(fits) == EXT_SPECIAL)
		break
	}

	if (gim != NULL) {
	    call imunmap (gim)
	    call imdelete (Memc[gimname])
	}
	call close (fits_fd)
	call sfree (sp)

	if (ext_count == 1)
	    return (EOF)
	else
	    return (OK)
end


# RFT_FIND_EOF -- Read the FITS data file until EOF is reached.

procedure rft_find_eof (fd)

int	fd			# the FITS file descriptor

int	szbuf
pointer	sp, buf
int	fstati(), read()
errchk	read

begin
	# Scan through the file.
	szbuf = fstati (fd, F_BUFSIZE)
	call smark (sp)
	call salloc (buf, szbuf, TY_CHAR)
	while (read (fd, Memc[buf], szbuf) != EOF)
	    ;
	call sfree (sp)
end


# RFT_SCAN_FILE -- Determine whether it is more efficient to read the
# entire file or to skip forward to the next file if the parameter
# make_image was set to no.

procedure  rft_scan_file (fd, fits, im, fe)

int	fd			# the FITS file descriptor
pointer	fits			# pointer to the FITS descriptor
pointer	im			# pointer to the output image
real	fe			# maximum file size in Kb for scan mode

int	i, szbuf
pointer	sp, buf
real	file_size
int	fstati(), read()
errchk	read

begin
	# Compute the file size in Kb and return if it is bigger than fe.
	file_size = 1.0
	do i = 1, IM_NDIM(im)
	    file_size = file_size * IM_LEN(im,i)
	if (IM_NDIM(im) <= 0)
	    file_size = 0.0
	else
	    file_size = file_size * abs (BITPIX(fits)) / FITS_BYTE / 1.0e3
	if (file_size >= fe)
	    return

	# Scan through the file.
	szbuf = fstati (fd, F_BUFSIZE)
	call smark (sp)
	call salloc (buf, szbuf, TY_CHAR)
	while (read (fd, Memc[buf], szbuf) != EOF)
	    ;
	call sfree (sp)
end


# RFT_EXT_SKIP -- Compute the size of the data extension to be skipped
# and do the skipping.

int procedure rft_ext_skip (fits_fd, fits, im)

int	fits_fd			# fits file descriptor
pointer	fits			# pointer to the fits structure
pointer	im			# pointer to the output image

int	i, nbits, nblocks, sz_rec, blksize, stat
pointer	buf
int	fstati(), rft_getbuf()

begin
	# Compute the number of blocks to skip.
	nbits = NAXISN(im,1)
	do i = 2, NAXIS(im)
	    nbits = nbits * NAXISN(im,i)
	nbits = nbits + PCOUNT(fits)
	nbits = abs (BITPIX(fits)) * GCOUNT(fits) * nbits
	nblocks = int ((nbits + 23039) / 23040)

	sz_rec = FITS_RECORD / SZB_CHAR
	call malloc (buf, sz_rec, TY_CHAR)
	blksize = fstati (fits_fd, F_SZBBLK)
        if (mod (blksize, FITS_RECORD) == 0)
            blksize = blksize / FITS_RECORD
        else
            blksize = 1

	# Skip the blocks.
	do i = 1, nblocks {
	    stat = rft_getbuf (fits_fd, Memc[buf], sz_rec, blksize,
	        NRECORDS(fits))
	    if (stat == EOF)
		break
	}

	call mfree (buf, TY_CHAR)

	return (stat)
end
