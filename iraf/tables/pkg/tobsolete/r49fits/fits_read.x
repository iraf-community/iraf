include <error.h>
include <imhdr.h>
include <imio.h>
include <imset.h>
include <fio.h>
include <tbset.h>
include	"rfits.h"
#
# Revision History
#
# Nov 8 1990	Allow for dataless images to be created, with exception
#		of those fits header in which the keyword IRAFNAME is 
#		"null_image". A dataless fits file has NAXIS = 0.
#
#		Allow for FITS tables whose IRAFNAME content has an extension
#		'trl' (ascii trailer file of up 132 character per line) to
#		be converted directly to an ascii file.
#

# RFT_READ_FITZ -- Convert a FITS file. An EOT is signalled by returning EOF.

int procedure rft_read_fitz (fitsfile, template, iraffile, ext_number)

char	fitsfile[SZ_FNAME]	# FITS file name
char	iraffile[SZ_FNAME]	# IRAF file name
char	template[SZ_FNAME]	# Template filename
int	ext_number		# Index of the FITS extension to retrieve

char	root[SZ_FNAME], cluster[SZ_FNAME]
char	tempf[SZ_FNAME]
char	extn[SZ_EXTN]
char	tabfile[SZ_FNAME], seqfile[SZ_FNAME]
int	fits_fd, istat, stat, pos1, pos2, ntab, nch, junk
int	nread, fd_usr, ncols, first_im
bool    trl
pointer	im, imt, fits, tp, ext

int	rft_read_header(), mtopen(), strlen()
int	tab_read_header(), gstrmatch()
int	open(), gi_gstfval(), strncmp(), strcmp()
int	strldx(), tbpsta(), rft_image_ext(), fnroot()
bool	gi_geis()
pointer	tbtopn()

errchk	smark, sfree, salloc, rft_read_header, rft_read_image, mtopen
errchk	close, imunmap, frename, rft_opnim
	
include	"rfits.com"
include "tab.com"

define  read_extn_ 99
begin
	ext_type = 0
	stat = 0
	# Open input FITS data
	iferr( fits_fd = mtopen (fitsfile, READ_ONLY, 0)) {
	   call eprintf("\7 ERROR: File does not exist (%s)\n")
	       call pargstr(fitsfile)
	   return
	}

	# Allocate memory for program data structure
	call calloc (fits, LEN_FITS, TY_STRUCT)

	call pr_files (iraffile, fitsfile)

	call imgcluster (iraffile, cluster, SZ_FNAME)
	call iki_parse (cluster, root, extn)

	# 'gkey' can have the following values
	#  DEF_GPB:    Will create a default gp descriptor
	#  NONDEF_GPB: Will read the gp descriptor from a user supplied
	#	       template with the non_default gp keywords.
	#  NON_GPB:    Will not create a gp descriptor in the output SDAS
	#	       file. This value gets setup with the fits keyword
	#	       SDASMGNU, which indicates that the input fits file
	#	       contains a file with an extra dimension for the groups
	#	       and an attached table with the gp values.
	#  TO_MG:      Will create a multigroup Geis file from the input
	#	       FITS file and its attached table. The FITS header
	#	       should have the keyword SDASMGNU and OPSIZE to
	#	       accomplish this.
	#  IMH:	       If the output file is 'imh' type.


	if (strlen(template) != 0 && gkey == TO_MG) {
	   call eprintf("\n ****  Please revise your parameter file\n")
	   call error (1,"You cannot have a template header and xdimtogf=YES")
	}

	if (strlen(template) != 0)
	   gkey = NONDEF_GPB		

	# Open spool file to contain the fits header
	fd_usr = open ("fits_hdr", READ_WRITE, SPOOL_FILE)
	
	iferr {
	    nread = rft_read_header (fits_fd, fd_usr, fits)
        } then {
	    call erract (EA_WARN)
	    return
	}
	# If we want to read an extension only, we need to skip the main data
	# unit
	if (ext_number > 0) {
	   call skip_mdata (fits_fd, fits)
	   call close (fd_usr)
	   call printf(" ... main FITS unit skipped.\n")
	   goto read_extn_
	}
	# If we want to build a geis file, we should not have an imh
	# extension.
	if (strcmp(extn, "imh") == 0 && gkey == NONDEF_GPB)
           call error(1,"You cannot select the 'imh' extension and a template")

	# Change the output iraffile to the value of IRAFNAME
	if (old_name == YES)
	   call change_name (iraffile,fits)

	if (gkey == TO_MG && GCOUNT(fits) == -1)
	     gkey = DEF_GPB 
	if (nread != EOF) {
	    iferr {
		call rft_opnim (template, iraffile, fd_usr, fits,
				nread, im, imt)
		call close (fd_usr)
	        call rft_read_image (fits_fd, fits, im)
	    } then {
	        call erract (EA_WARN)
		return
	    }
	} else {
	    call printf ("End of data\n")
	    call mfree (fits, TY_STRUCT)
	    call close (fd_usr)
	    call close (fits_fd)
	    return (EOF)
	}

        if (NAXIS(fits) != 0) {
	
	   if (gi_geis (im)) {
	      # Update the wcs values since the STF_WCS routines did 
	      # not have the CRPIX nor the CRVAL's keywords in the user header.
              if (gkey == DEF_GPB)
	         call update_gpb (im,fits)
	      # Because of some bug in stf the value below does
	      # not get change to YES.
	      if (gkey == NON_GPB && gi_gstfval(im, "PCOUNT") == 0)
		 call gi_pstfval (im, "GROUPS", YES)
	   }
 	   if (gkey == NON_GPB || gkey == NONDEF_GPB)
	      call imunmap (imt)

	   first_im = YES
	   if (EXTEND(fits) == NO || ext_number == 0) {
	      iferr (call imunmap(im))
	    	   call erract(EA_WARN)

	   }

        } else {   # if nxis==0, we'll create a dataless image
	   # There is no image created if the fits file contains
	   # a dummy header with keyword IRAFNAME equals 'null_image'.
	   if (strncmp (IRAFNAME(fits), "null_image", 10) != 0) {
	      IM_PIXTYPE(im) = TY_SHORT
	      if (strcmp(extn,"imh") != 0)
	         call gi_update(im)
	      iferr (call imunmap(im))
	    	   call erract(EA_WARN)
#	   } else
	   }
	      first_im = NO
	}
	call print_header (fits, fitsfile, iraffile)

	call strcpy (iraffile, tempf, SZ_FNAME)

	# Do not read extensions if we wanted to read the main FITS unit
	# only.
	if (ext_number == 0) {
	   call mfree (fits, TY_STRUCT)
	   call close (fits_fd)
	   return (stat)
	}
	# If the above main header contains an extension flag, then
	# lets see if we find TABLE, BINTABLE or IMAGE.
 read_extn_
	if (EXTEND(fits) == YES) {

	   # No ieee for tables in FITS 'TABLE' format
	   ieee = NO
	   trl = false
	   FITSTYPE(fits) = EOS
	   call tbtext (root, tabfile, SZ_FNAME)
	   call strcpy (tabfile, seqfile, SZ_FNAME)
	   nch = gstrmatch (seqfile, ".tab", pos1, pos2)
	   ntab = 1
	   if (nch == 0) pos1 = strlen(tabfile)
	   call sprintf (seqfile[pos1], SZ_FNAME, "%02d.tab")
	        call pargi(ntab)
	   # look for more than one table in the current file

           repeat {
	      if (ext_number > 1)
	         call skip_extensions (ext_number, fits_fd)
	      tp = tbtopn (seqfile, NEW_FILE, 0)
	  
	      # Allocate space for the extension structure
	      call calloc (ext, LEN_EXTENSION, TY_STRUCT)

	      # Read FITS table header an user parameters if any, also
	      # create the table 'tbtcre'. Extension structure 'ext' is
	      # returned as well.
	      istat = tab_read_header (fits_fd, im, ext, tp)
	      if (ext_number >1 && istat == EOF) {
		 call eprintf("EOF encountered. Extension number specified")
		 call eprintf(" is greater than number of extensions.\n")
	      }
	      # Ready to read a table extension. 
	      if (ext_type != IMAGE && first_im == YES) {
		 if (gkey == DEF_GPB)
		    call update_gpb (im,fits)
		 call imunmap(im)
		 first_im = NO
	      }
	      # If we had read a fits header for a trailer file then.
	      if (EXT_TTYPE(ext) == TRAILER_FILE) {
		 trl = true
		 call strcpy ("trl", extn, SZ_EXTN)
	      } else if (EXT_TTYPE(ext) == TXT_FILE) {
		 trl = true
		 call strcpy ("txt", extn, SZ_EXTN)
	      }

	      # Istat will have the value below only after the
	      # first reading of the header. If the value is not
	      # encounter then it will read the whole table header
	      # before coming here.

	      if (istat == IMAGE) {
		 call tbtclo (tp)
		 if (gkey == IMH) {
		    call strcpy (root, tempf, SZ_FNAME)
		    call sprintf(tempf[strlen(tempf)+1],SZ_FNAME,"_%d.imh")
			 call pargi(ntab)
		    ntab = ntab + 1
		 } else {
		    call strcpy (seqfile, root, pos1+1)
		    call iki_mkfname (root, extn, iraffile, SZ_FNAME) 
		       if (gkey == DEF_GPB)
		          call strcpy (iraffile, tempf, SZ_FNAME)
		 }
		 junk = rft_image_ext (im, fits_fd, tempf, template, fits)
		 if (old_name == NO && gkey == DEF_GPB) {
	            ntab = ntab + 1
	            call sprintf (seqfile[pos1], SZ_FNAME, "%02d.tab")
	               call pargi(ntab)
	         }
		 first_im = NO
	      } else if (istat != EOF) {
		 # Now read the fits table data
		 if (trl) {
		    call tbtclo (tp)
		    call delete (seqfile)
		    nch = strldx(".", tabfile)
		    call strcpy (extn, tabfile[nch+1], SZ_FNAME)
		    # Open an ascii file to convert a fits table with
		    # a trailer file into an ascii file.
		    nch = strlen(seqfile)
		    call strcpy (extn, seqfile[nch-2], SZ_EXTN)
		    tp = open (seqfile, NEW_FILE, TEXT_FILE)
		    call tab_read_data (fits_fd, ext, tp)
		    call close (tp)
	         } else {
		    call tab_read_data (fits_fd, ext, tp)
		    ncols = tbpsta(tp, TBL_NCOLS)
		    call tbtclo (tp)
		 }
		  
		 # Print table information to STDOUT.
		 call prtab_info (ext, seqfile, ncols)
	      } else  # istat is EOF
		 call tbtclo (tp)

	      if (istat != IMAGE && istat != EOF) {
	         call strcpy (seqfile, tempf, SZ_FNAME)
	         ntab = ntab + 1
	         call sprintf (seqfile[pos1], SZ_FNAME, "%02d.tab")
	             call pargi(ntab)
	      }
	      # Now free up the memory allocated in ext.
	      call mfree(EXT_PNULL(ext),TY_CHAR)
	      call mfree (EXT_PSCAL(ext),TY_REAL)
	      call mfree (EXT_PZERO(ext), TY_REAL)
	      call mfree (EXT_PCW(ext), TY_INT)
	      call mfree (EXT_PCOL(ext), TY_INT)
	      call mfree (ext, TY_STRUCT)
	   } until (istat == EOF || ext_number > 0)
	   # The last extension was IMAGE.
	   if (im != NULL) {
	      if (gkey == DEF_GPB)
		 call update_gpb (im,fits)
	      call imunmap(im)
	      EXTEND(fits) = NO
	   }
           # Get rid of the sequential number if only 
	   # one table is present.
	   if (old_name == NO && ntab == 2 && ext_type != IMAGE) {
	      call frename (tempf, tabfile)
	      if (short_header == YES ) {
		 nch = fnroot (tabfile, root, SZ_FNAME)
		 if (!trl) {
	            call printf("%17t renamed to %s\n")
	  	        call pargstr(tabfile)
		 } else {
	            call printf("%17t renamed to %s.%s\n")
	  	        call pargstr(root)
		        call pargstr(extn)
	         }
	      }
	   }
	} # EXTEND(fits)

	call mfree (fits, TY_STRUCT)
	call close (fits_fd)
	return (stat)
end
# CHANGE_NAME -- Procedure to change the name of the temporary file name
#		'iraffile'  to the value of IRAFNAME(fits)

procedure change_name (iraffile,fits)
char	iraffile[SZ_FNAME]
pointer fits


char	root[SZ_FNAME]
char	nroot[SZ_FNAME], nextn[SZ_EXTN]
char	extn[SZ_EXTN]
int	k, strlen(), fnldir(), access()
int	strmatch(), strncmp(), len_name, strcmp(), cl_index, cl_size
pointer sp, bf,dp, tname

include "rfits.com"

begin
	call smark (sp)
	call salloc (bf, SZ_FNAME, TY_CHAR)
	call salloc (dp, SZ_FNAME, TY_CHAR)
	call salloc (tname, SZ_FNAME, TY_CHAR)
	call imgcluster (iraffile, Memc[bf], SZ_FNAME)
	call iki_parse (Memc[bf], root, extn)

	# Save iraffile.
	call strcpy (iraffile, Memc[tname], SZ_FNAME)
	len_name = strlen(IRAFNAME(fits))
	if (short_header == YES ) {
	   if (len_name == 0)
	      call strcpy (iraffile, IRAFNAME(fits), SZ_FNAME)
	   else if (NAXIS(fits) == 0 && len_name > 0)
	      call strcpy (IRAFNAME(fits), iraffile, SZ_FNAME)
		 
	}

 	if (len_name != 0 && strncmp (IRAFNAME(fits), "null_image", 10) != 0) {
	   # Get directory prefix from 'iraffile'.
	   k = fnldir (Memc[bf], Memc[dp], SZ_FNAME)
           call gparse (IRAFNAME(fits), Memc[bf], nroot, nextn,cl_size,cl_index)
	   call pesc_dash (Memc[bf])
	   if (nextn[1] == EOS)
	      call strcpy ("hhh", nextn, SZ_EXTN)
	   if (gkey == TO_MG)
		 if (strmatch (nroot, "_cvt") != 0)
		    nroot[strlen(nroot)-3] = EOS 
	   call strcat (nroot, Memc[dp], SZ_FNAME)
	   # Now copy 'hhh' new_extensio into 'imh' (extn).
	   if (strcmp(extn,nextn) != 0)  # Extensions are diferent
	      if (strcmp(extn,"imh") != 0 && strcmp(nextn,"imh") == 0)
		 call strcpy(extn, nextn, SZ_EXTN)
	   # Now the other way around.
	   if (gkey == IMH)
	      call strcpy("imh",nextn, SZ_EXTN)
	   if (cl_index > 0) {
	      call sprintf (Memc[bf],SZ_FNAME,"_%d")
		   call pargi(cl_index)
	      call strcat(Memc[bf], Memc[dp], SZ_FNAME)
	   }
	   call iki_mkfname (Memc[dp], nextn, iraffile, SZ_FNAME)
	   call cesc_dash (iraffile)
	   # See if the file already exists.
	   if (access (iraffile,0,0) == YES) {
	      call eprintf("\n Warning: Cannot rename %s to %s\n")
		   call pargstr(Memc[tname])
		   call pargstr(iraffile)
	      call strcpy (Memc[tname], iraffile, SZ_FNAME)
	   }
	} 
	call sfree(sp)
end

define	NONGPB_HDR	"fitsio$non_gpb.hhh"
define	LEN_CARDP1	81
# The following definition is necessary to open the template
# file 'non_gpb.hhh' which has the keywrod "GROUPS = F" 
# plus PCOUNT and GCOUNT to zero also.

procedure rft_opnim (template, iraffile, fd_usr, fits, nread, im, imt)

char	template[SZ_FNAME]	# template file name
char	iraffile[SZ_FNAME]	# output image name
int	fd_usr			# fits header spool file des.
pointer fits			# fits descriptor
int	nread			# number of header lines in the fits header
pointer	im			# output image descriptor
pointer	imt			# o: template image pointer

pointer	ua
bool	gi_geis()
int	i, fd, maxlines, max_lenuser, stropen()
int	strncmp(), cl_index,cl_size, strlen()
pointer	immap()
char	cluster[SZ_FNAME], tmp[SZ_FNAME], root[SZ_FNAME] 
errchk  immap

include "rfits.com"
include "tab.com"

begin	

	if (strncmp (IRAFNAME(fits), "null_image", 10) == 0)
	   return        

	# If template is specified, the user has chosen to create a
	# non_default gpb descriptor. This will reset any previous
	# value, e.g. gkey = NON_GPB if the keyword SDASMGCV was
	# present in the fits header.
	if (EXTEND(fits) == YES ||
	    ext_type == IMAGE ) { # We might have a FITS file with IMAGE
				   # extension
	   if (template[1] != EOS) gkey = NONDEF_GPB

	   # See if there is a groups specification in the output geis file;
	   # i.e. '[cl_index/cl_size]'.
           call gparse (IRAFNAME(fits), cluster, tmp, root,cl_size,cl_index)

           if (cl_size > 1 && gkey != IMH) {
	      gkey = -1
	      call sprintf (iraffile[strlen(iraffile)+1], SZ_FNAME, "[1/%d]")
			call pargi (cl_size)
	   } else if (cl_index > 1  && gkey != IMH) {
	      gkey = IMAGE
	      call sprintf (iraffile[strlen(iraffile)+1], SZ_FNAME, "[%d]")
		  call pargi (cl_index)
	      call print_header (fits, "", iraffile)
	   } else if (ext_type == IMAGE) { # We have a new IMAGE
	      call imunmap(im)
	      if (gkey != IMH)
	         gkey = DEF_GPB
	      if (old_name == YES)
		 call change_name (iraffile, fits)
	      call print_header (fits, "", iraffile)
	   }

	}


	# Create IRAF image header.
	if (gkey == NONDEF_GPB) {
	   imt = immap (template, READ_ONLY, 0)
	   im  = immap (iraffile, NEW_COPY, imt)
	} else if (gkey == NON_GPB) {
	   imt = immap (NONGPB_HDR, READ_ONLY, 0)
	   im  = immap (iraffile, NEW_COPY, imt)
	} else if (gkey == IMAGE) {
	   call gi_opengr (im, cl_index, IRAFMIN(fits), IRAFMAX(fits), 0)
	} else {
	   im = immap (iraffile, NEW_IMAGE, 0)
	}

	# reset the naxis things
	IM_NDIM(im) = NAXIS(fits)

	do i = 1, IM_NDIM(im)
	   IM_LEN(im,i) = NAXISN(fits,i)
	
	# Now copy the fits header lines onto the IM_USERAREA
	maxlines = (ARB - 3700)/LEN_CARD
	if (nread > maxlines) {
	   call printf ("=== %d fits header lines discarded\n")
		call pargi (nread - maxlines)
	   call printf ("Maximun number of lines is: %d\n")
		call pargi (maxlines)
	   nread = maxlines
	} 
	IM_LENHDRMEM(im) = nread*LEN_CARD + LEN_IMHDR + 81*32
	call realloc (im, IM_LENHDRMEM(im) + LEN_IMDES, TY_STRUCT)
	max_lenuser = (IM_LENHDRMEM(im) + LEN_IMDES - IMU)*SZ_STRUCT
	ua = IM_USERAREA(im) 
	fd = stropen (Memc[ua], max_lenuser, NEW_FILE)
	call seek (fd_usr, BOFL)

	if (gkey == DEF_GPB && gi_geis (im))
	   call rft_create_gpb (im, fd)

	call fcopyo (fd_usr, fd)
	call close (fd)

	iferr (	call imgstr (im, "TARGNAME", IM_TITLE(im), SZ_OBJECT))
	iferr (	call imgstr (im, "OBJECT", IM_TITLE(im), SZ_OBJECT))
	   IM_TITLE(im) = EOS
end


# PRINT_HEADER -- Routine to  print header information.

procedure print_header (fits, fitsfile, iraffile)

pointer fits
char	fitsfile[SZ_FNAME]
char	iraffile[SZ_FNAME]

char	root[SZ_FNAME]
char	extn[SZ_EXTN]
int	k, strlen(), itab
int	len_name
pointer sp, bf

include "rfits.com"

begin
	if (short_header == NO)
	   return

	call smark (sp)
	call salloc (bf, SZ_FNAME, TY_CHAR)
	call imgcluster (iraffile, Memc[bf], SZ_FNAME)
	call iki_parse (Memc[bf], root, extn)

	# set itab for tape or disk file
	itab = 16
	if (tape == YES)
	   itab = 5

	len_name = strlen(IRAFNAME(fits))
	if (len_name == 0)
	    call strcpy (iraffile, IRAFNAME(fits), SZ_FNAME)
	else if (NAXIS(fits) == 0 && len_name == 0)
	    call strcpy (IRAFNAME(fits), iraffile, SZ_FNAME)
		 

	if (fitsfile[1] == EOS) { 
	   call printf("%*t  ")
	   call pargi(itab)
	}
	call printf ("%-19.19s ")
	    call pargstr(iraffile)

	do k = 1, NAXIS(fits) {
	   call printf("%-5.5d")
	       call pargi(NAXISN(fits,k))
	}

	if (NAXIS(fits) == 0) 
	   call printf("%16t")
	if (NAXIS(fits) == 1) 
	   call printf("%11t")
	if (NAXIS(fits) == 2) 
	   call printf("%6t")
	call printf("%-2.2d %-8.8s ") 
	    call pargi(BITPIX(fits))
	    call pargstr(DATE(fits))
	   
	if (tape == YES)
	   call printf ("%-26.26s")
	else
	   call printf ("%-15.15s")
	call pargstr (OBJECT(fits))

	call printf("\n")

	# See if fitsfile and/or iraffile are too long and put
	# in the following line
	do k = 1, 3 {
	   if (strlen(fitsfile) > 16*k) {
	      call printf ("%-16.16s ")
		  call pargstr (fitsfile[16*k+1])
	      if (strlen(Memc[bf]) > 19*k) {
		 call printf ("%-19.19s ")
		     call pargstr (Memc[bf+19*k])
	      }
	      call printf ("\n")
	   } else
	      if (strlen(Memc[bf]) > 19*k) {
		 call printf ("%*t %-19.19s \n")
		     call pargi (itab)
		     call pargstr (Memc[bf+19*k])
	      }
	}
	call sfree(sp)
end

# PRTAB_INFO -- Procedure to print table information to STDOUT

procedure prtab_info (ext, seqfile, ncols)

pointer	ext			#fits descriptor
char	seqfile[SZ_FNAME]	#sequential table filename
int	ncols			#NUmber of columns in table.

int	strlen(), k, fnldir(), fnroot(), fnextn()
pointer sp, bf, rr, sf
char	extn[SZ_EXTN]
int	itab
bool    trl

include "rfits.com"

begin
	call smark(sp)
	call salloc (bf, SZ_FNAME, TY_CHAR)
	call salloc (sf, SZ_FNAME, TY_CHAR)

	# set tab value for tape or disk file
	itab = 17
	if (tape == YES)
	   itab = 5
	   
	trl = false
	if (EXT_TTYPE(ext) == TRAILER_FILE)
	   trl = true
	# save 'seqfile' 
	call strcpy (seqfile, Memc[sf], SZ_FNAME)
	if (old_name == YES && strlen (EXTNAME(ext)) != 0) {
	   call salloc (rr, SZ_FNAME, TY_CHAR)
	   # Escape dash '-' in name if any
	   call pesc_dash(EXTNAME(ext))
	   k = fnroot (EXTNAME(ext), Memc[rr], SZ_FNAME)
	   call cesc_dash(Memc[rr])
	   k = fnextn (EXTNAME(ext), extn, SZ_EXTN)
	   # a 'trl' file will always have the extension.
	   if (k == 0) 
	      call strcpy ("tab", extn, SZ_EXTN)
	   k = fnldir (Memc[sf], Memc[bf], SZ_FNAME)
	   call strcat (Memc[rr], Memc[bf], SZ_FNAME)
	   call iki_mkfname (Memc[bf], extn, EXTNAME(ext), SZ_FNAME)

	   iferr (call frename (seqfile, EXTNAME(ext) )) {
		  call printf ("Cannot rename %s to %s\n")
	  	       call pargstr (Memc[sf])
		       call pargstr (EXTNAME(ext))
	   } else if (short_header == YES) {
	          call printf ("%*t %-19.19s ")
		     call pargi (itab)
	  	     call pargstr (EXTNAME(ext))
		  call strcpy (EXTNAME(ext), Memc[sf], SZ_FNAME)
	   }
	} else {
	    if (short_header == YES) {
	       call printf ("%*t %-19.19s ")
		    call pargi (itab)
         	    call pargstr (Memc[sf])
	    }
	}
	if (short_header == YES) {
	   call printf ("%-4.4d %-5.5d")
		call pargi (EXT_ROWLEN(ext))
		call pargi (EXT_NROWS(ext))
	   call printf(" Ncols=%3d ")
		call pargi (ncols)

	   if (old_name == NO) {
	      if (trl) {
	         k = fnroot (EXTNAME(ext), Memc[bf], SZ_FNAME)
		 call strcat (".trl", Memc[bf], 4)
	      } else
	         call strcpy(EXTNAME(ext), Memc[bf], LEN_CARD)
	      if (Memc[bf] == EOS)
	         call strcpy("  ", Memc[bf], LEN_CARD)
	      if (tape == YES)
	         call printf("%-30.30s")
 	      else
                 call printf("%-22.22s")
	  	    call pargstr(Memc[bf])
	   }
	   call printf("\n")
	   # See if fitsfile and/or iraffile are too long and put
	   # in the following line
	   do k = 1, 3 {
	      if (strlen(Memc[sf]) > 19*k) {
	         call printf ("%*t %-19.19s \n")
		      call pargi (itab)
		      call pargstr (Memc[sf+19*k])
	      }
	   }
	}
	call sfree(sp)
end
define  X_BITPIX          Memi[$1]
define  X_NAXIS           Memi[$1+2]    
define  X_PCOUNT          Memi[$1+3]    
define  X_GCOUNT          Memi[$1+4]    
define  X_NAXISN          Memi[$1+5+$2-1]

# SKIP_EXTENSIONS -- Procedure to skip a number of extensions in the
#		     FITS file.
procedure skip_extensions (n_extensions, fits_fd)
int	n_extensions		# Number of extensions to skip
int	fits_fd			# FITS file descriptor

pointer xt,sp
char	card[LEN_CARD]
int	i, rft_init_read_pixels(), get_min_info()
int	stat, k

include "rfits.com"
begin
        call smark(sp)
	call salloc (xt, 103, TY_INT)   # 4+ naxisn could be up to 99

	do k = 1, n_extensions - 1 {
	   i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)
	   repeat {
	      stat = get_min_info(fits_fd, card,xt)
	      if (stat == EOF) {
#		 call printf ("EOF encountered\n")
		 return
	      }
           } until (stat == YES)   # stat == YES if END card encountered.
	   call skip_xdata (fits_fd, xt)
        }
	call sfree(sp)
end
int procedure get_min_info (fits_fd, card, xt)
int	fits_fd
char    card[ARB]
pointer xt

int	strmatch(), rft_read_pixels()
int	nchar, i, ctoi(), k, j, stat, nrec

begin
	stat = rft_read_pixels (fits_fd, card, LEN_CARD, nrec, 1)
	if (stat == EOF) return (EOF)

	i = COL_VALUE
	if (strmatch (card, "^END     ") != 0) {
	   return(YES)
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	   nchar = ctoi (card, i, X_BITPIX(xt))	
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	   nchar = ctoi (card, i, X_NAXIS(xt))	
	} else if (strmatch (card, "^PCOUNT  ") != 0) {
	   nchar = ctoi (card, i, X_PCOUNT(xt))	
	} else if (strmatch (card, "^GCOUNT  ") != 0) {
	   nchar = ctoi (card, i, X_GCOUNT(xt))	
        } else if (strmatch (card, "^NAXIS") != 0) {
	   k = strmatch (card, "^NAXIS")
	   nchar = ctoi (card, k, j)
	   nchar = ctoi (card, i, X_NAXISN(xt, j))
	}
	return(NO)
end
include <mach.h>
include <fset.h>
define  NB_DOUBLE   64

procedure skip_xdata (fits_fd, xt)

int	fits_fd		# FITS file descriptor
pointer	xt		# FITS data structure

int	i, npix, npix_record, blksize, nrec
long	nlines, il, pc
pointer	tempbuf,sp,pp

int	fstati(), bitpix, gc
data	tempbuf /NULL/
int	rft_init_read_pixels(), rft_read_pixels()
errchk	rft_init_read_pixels, rft_read_pixels

include "rfits.com"

begin
	if (X_NAXIS(xt) == 0) {
	    return
	}

	npix = X_NAXISN(xt, 1)
	nlines = 1
	do i = 2, X_NAXIS(xt)
	    nlines = nlines * X_NAXISN(xt, i)

	# FITS data is converted to type  LONG.  If BITPIX is not one
	# of the MII types then rft_read_pixels returns an ERROR.

	bitpix = abs (X_BITPIX(xt))
	if (tempbuf != NULL)
	    call mfree (tempbuf, TY_LONG)
	if (bitpix != NB_DOUBLE)
	   call malloc (tempbuf, npix, TY_LONG)
	else
	   call malloc (tempbuf, npix*2, TY_LONG)

	npix_record = len_record * FITS_BYTE / bitpix
	i = rft_init_read_pixels (npix_record, bitpix, LSBF, TY_LONG)
	blksize = fstati (fits_fd, F_SZBBLK)
	if (mod (blksize, 2880) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1

        gc = X_GCOUNT(xt)
	pc = X_PCOUNT(xt)
	if (pc > 0) {
	   call smark(sp)
	   call salloc (pp, pc, TY_LONG)
	}
	repeat {
	   # If there are PCOUNT pixel of GROUP info then read that.
	   # This is for Small Group of data format.
	   if (pc > 0) {
	      if (rft_read_pixels (fits_fd, Meml[pp], pc,
	         nrec, blksize) != pc)
	         call printf ("Error reading FITS data\n")
	   }

	   do il = 1, nlines {

	       # Read in image line
	       i = rft_read_pixels (fits_fd, Meml[tempbuf], npix,
	           nrec, blksize)
	       if (i != npix)
		call printf ("Error reading FITS data\n")

	   }
	   gc = gc - 1
	} until (gc < 1)
	if (pc > 0)
	   call sfree (sp)
end
#SKIP_MDATA -- skip main FITS data unit

procedure skip_mdata(fits_fd, fits)
int	fits_fd
pointer fits

pointer xt,sp
int	i

begin
        call smark(sp)
	call salloc (xt, 103, TY_INT)   # 4+ naxisn could be up to 99

	X_BITPIX(xt) = BITPIX(fits)
	X_NAXIS(xt) = NAXIS(fits)
	do i = 1, NAXIS(fits)
	  X_NAXISN(xt, i) = NAXISN(fits, i)

	X_GCOUNT(xt) = 1
	X_PCOUNT(xt) = 0

	call skip_xdata (fits_fd, xt)

	call sfree(sp)
end
procedure pr_files (iraffile, fitsfile)
char	iraffile[SZ_FNAME], fitsfile[ARB]

pointer	sp,pp
int	stridx(), len,k,id1,id2
include "rfits.com"
begin
	call smark(sp)
	call salloc(pp, SZ_FNAME, TY_CHAR)
	if (long_header == YES) {
	   call printf ("\n**** FILE: %s\n ")
		call pargstr (iraffile)
	}
	if (short_header == YES) {
	   if (tape == YES) {
	      # Get the file number from the 'fitsfile' name.
	      id1 = stridx ("[", fitsfile) + 1
	      id2 = stridx ("]", fitsfile) - 1
	      len = id2-id1+1
	      do k = 0, len-1
		 Memc[pp+k] = fitsfile[id1+k]
	      Memc[pp+len] = EOS
	      call printf ("%-5.5s")
		call pargstr (Memc[pp])
	   } else {
	      call printf("%-16.16s ")
		call pargstr (fitsfile)
	   }
	}
	call sfree(sp)
end
