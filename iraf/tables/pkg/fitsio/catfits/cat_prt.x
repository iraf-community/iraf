# Copyright restrictions apply - see tables$copyright.tables 
# 
# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include "../stwfits/dfits.h"
include "catf.h"

# CAT_PRINT_MAIN -  Output to stdout and/or the the log_file one
# line of information per input fits file according to the field
# specifications in the file format_file.
 
procedure cat_print_main (file_number, fits_file, fits)
 
int	file_number		# input file sequence
char    fits_file[SZ_FNAME]	# Input file name
pointer fits			# Internal information pointer

char	str[LEN_CARD]		# card data string
int	nk, i, nch
char	sdim[SZ_KEYWORD]
char    line[SZ_LINE]
 
int	strmatch(), itoc(), tape, mtfile(), strlen(),strcmp()
include	"../stwfits/dfits.com"
include "catfits.com"
 
begin
	# Search the keyword in the card table
	line[1] = EOS
	tape = mtfile(fits_file)
        do nk = 1, nkeywords {
	   if (strcmp (Memc[key_table[nk]], "EXT#") == 0) {
		 nch= itoc (EXT_NUMBER(fits), str, SZ_KEYWORD)
	   } else if (strmatch (Memc[key_table[nk]], "FILENAME") > 0) {
	      if (IRAFNAME(fits) != EOS)
		 call strcpy(IRAFNAME(fits), str, SZ_OBJECT)
	      else
		 call strcpy (CATV(fits,nk), str, SZ_OBJECT)
	      if (print_ext == NO && EXTEND(fits) != NO)
		 call strcat (":X", str, SZ_OBJECT)
	   } else if (strmatch (Memc[key_table[nk]], "FITSNAME") > 0) {
	      if (tape == NO)
	      	 call strcpy (fits_file, str, LEN_CARD)
	      else
	         nch= itoc (file_number, str, SZ_KEYWORD)
	   } else if (strmatch (Memc[key_table[nk]], "DIMENS") > 0) {
	      str[1] = EOS
	      do i = 1, NAXIS(fits) {
	         nch= itoc (NAXISN(fits,i), sdim, SZ_KEYWORD)
	         call strcat (sdim, str, LEN_CARD)
		 if (i != NAXIS(fits))
		    call strcat ("x", str, LEN_CARD)
	      }
	   } else if (strmatch (Memc[key_table[nk]], "BITPIX") > 0) {
	        nch= itoc (BITPIX(fits), str, SZ_KEYWORD)
		str[nch+1] = DATATYPE(fits)     # append single datatype code
		str[nch+2] = EOS
	   } else {
		call strcpy (CATV(fits,nk), str, SZ_OBJECT)
	   }
	   call print_string (line, str, Memc[fmt_table[nk]], opt_table[nk])
	}
	call printf("%80.80s\n")
            call pargstr(line)
	nch = strlen (line)
	line[nch+1] = '\n'
	if (log_fd != 0)
	   call putline (log_fd,line)
end



# CAT_PRINT_EXT  -- Print one liner for the extension.
 
procedure cat_print_ext (fits)
 
pointer fits

char	str[LEN_CARD]		# card data string
int	strlen(), nch, nk, strmatch(), strcmp()
int	itoc(), i
char    line[SZ_LINE], sdim[SZ_KEYWORD]
 
include	"../stwfits/dfits.com"
 
begin

	line[1] = EOS
        do nk = 1, nkeywords {
	   if (strcmp (Memc[key_table[nk]], "EXT#") == 0) {
		 nch= itoc (EXT_NUMBER(fits), str, SZ_KEYWORD)
	   } else if (strmatch (Memc[key_table[nk]], "FILENAME") > 0) {
		 call strcpy (IRAFNAME(fits), str, SZ_OBJECT)
	   } else if (strmatch (Memc[key_table[nk]], "FITSNAME") > 0) {
	       if (XTENSION(fits) == TABLE)
	      	 call strcpy ("  TABLE", str, LEN_CARD)
	       else if (XTENSION(fits) == BINTABLE)
	      	 call strcpy ("  BINTABLE", str, LEN_CARD)
	       else if (XTENSION(fits) == IMAGE)
	      	 call strcpy ("  IMAGE", str, LEN_CARD)
	       else
	      	 call strcpy ("   Other_ext", str, LEN_CARD)
	   } else if (strmatch (Memc[key_table[nk]], "BITPIX") > 0) {
	       if (XTENSION(fits) == IMAGE) {
	          nch= itoc (BITPIX(fits), str, SZ_KEYWORD)
		  str[nch+1] = DATATYPE(fits)     # append single datatype code
		  str[nch+2] = EOS
	       } else
		  str[1] = EOS
	   } else if (strmatch (Memc[key_table[nk]], "DIMENS") > 0) {
	       if (XTENSION(fits) == TABLE || XTENSION(fits) == BINTABLE) {
	          call sprintf (str, LEN_CARD, "%dFx%dR")
	   	       call pargi (NCOLS(fits))
	   	       call pargi (NAXISN(fits,2))
	       } else if (XTENSION(fits) == IMAGE) {
	          str[1] = EOS
	          do i = 1, NAXIS(fits) {
	             nch= itoc (NAXISN(fits,i), sdim, SZ_KEYWORD)
	             call strcat (sdim, str, LEN_CARD)
		     if (i != NAXIS(fits))
		        call strcat ("x", str, LEN_CARD)
		  }
	       }
	   } else if (strcmp (Memc[key_table[nk]], "EXTVER") == 0) {
		 if (EXTVER(fits) == INDEFI)
		    str[1]=EOS
	         else
		    nch= itoc (EXTVER(fits), str, SZ_KEYWORD)
	   } else {
		call strcpy (CATV(fits,nk), str, SZ_OBJECT)
	   }
	   call print_string (line, str, Memc[fmt_table[nk]], opt_table[nk])
	}

	call printf ("%80.80s\n")
	     call pargstr(line)
	nch = strlen (line)
	line[nch+1] = '\n'
	if (log_fd != 0)
	   call putline (log_fd,line)
end

# CAT_EXPLANATION -- print at the end of the log file and explanation
# of the keyword meanings. The information comes from the comment
# field in the file format_file.

procedure cat_explanation (fits)

pointer	fits
char 	line[SZ_LINE], temp[LEN_CARD], format_file[SZ_FNAME]
int	nk, nchar, strlen(), strmatch(), open(), getline(), fdd, stridx()

include	"../stwfits/dfits.com"
include "catfits.com"

begin
	call putline (log_fd, "\n")
	call putline (log_fd, "\n")
	call sysid (line, SZ_LINE)
	call putline (log_fd, line)
	call putline (log_fd, "\n")

        call clgstr ("format_file", format_file, SZ_FNAME)
	if (format_file[1] == EOS)
           call strcpy ("tables$pkg/fitsio/format.mip", format_file, SZ_FNAME)
        fdd = open (format_file, READ_ONLY, TEXT_FILE)

	# Ignore comments field if FILENAME, FITSNAME, DIMENS and BITPIX
	do nk = 1, nkeywords {
	   nchar = getline (fdd, temp) 
	   if (strmatch (Memc[key_table[nk]], "FILENAME") > 0) {
	      call strcpy ("FILENAME: Original input filename ",
			   line, SZ_LINE)
	      if (print_ext == NO && EXTEND(fits) != NO)
		 call strcat (", ':X' indicates a FITS extension is present",
				line, SZ_LINE)
	      call putline (log_fd, "\n")
	      call putline (log_fd, line)
	      call putline (log_fd, "\n")
	   } else if (strmatch (Memc[key_table[nk]], "FITSNAME") > 0) {
	      next
	   } else if (strmatch (Memc[key_table[nk]], "DIMENS") > 0) {
call strcpy ("  DIMENS: FITS dimensionality NAXIS1xNAXIS2x...",
			    line, SZ_LINE)
	      if (print_ext == YES && EXTEND(fits) != NO) {
call strcat (",\n          'nFxmR' indicates n columns and m rows are present",
			    line, SZ_LINE)
call strcat ("\n            in the table extension.", line, SZ_LINE)
	      }
	      call putline (log_fd, line)
	      call putline (log_fd, "\n")
	   } else if (strmatch (Memc[key_table[nk]], "BITPIX") > 0) {
call strcpy("  BITPIX: Bits per pixels; if a letter is appended to the value,",
		line, SZ_LINE)
call strcat ("\n          it indicates the original pixel datatype, as follows:",
		line, SZ_LINE)
	      call putline (log_fd, line)
	      call putline (log_fd, "\n")
	      call putline (log_fd, "          Ushort (unsigned 16 bits)\n")
	      call putline (log_fd, "          Short (16 bits)\n")
	      call putline (log_fd, "          Integer (32 bits data)\n")
	      call putline (log_fd, "          Real\n")
	      call putline (log_fd, "          Double\n")
	   } else {
	      nchar = strlen(Memc[key_table[nk]]) 
	      if (nchar != SZ_KEYWORD) {
		 call sprintf( line, SZ_LINE, "%*t%s: %s")
		      call pargi(SZ_KEYWORD-nchar+1)
	      } else
		 call sprintf( line, SZ_LINE, "%s: %s")
	      call pargstr(Memc[key_table[nk]]) 
	      nchar = stridx ("#", temp)  
	      if (nchar == 0) temp[1] = '\n'; temp[2] = EOS 
	      call pargstr(temp[nchar+1])
	      call putline (log_fd, line)
	   }
	}
        call close (fdd)
end
