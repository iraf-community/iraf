include <pkg/mef.h>

# MEF_DUMMYHDR -- Write a dummy Primary header Unit with no data to a new file.
# Optionaly a header file with user keywords can be used.

procedure mef_dummyhdr (out, hdrfname)

int	out		#I File descriptor
char	hdrfname[ARB]	#I Header filename

char    card[LEN_CARD]
pointer sp, path, op
int	n, nlines, i, nchars, FD
int	strlen(), open(), getline(), strncmp()

begin
	call smark(sp)
	call salloc (path, SZ_PATHNAME, TY_CHAR)

	n = 0
	call mef_encodeb ("SIMPLE", YES, card, "FITS STANDARD")
	call mef_pakwr (out, card)
	n = n + 1

	call mef_encodei ("BITPIX", 8, card, "Character information")
	call mef_pakwr (out, card)
	n = n + 1

	call mef_encodei ("NAXIS", 0, card, "No image data array present")
	call mef_pakwr (out, card)
	n = n + 1

	call mef_encodeb ("EXTEND", YES, card,
	                         "There maybe standard extensions")
	call mef_pakwr (out, card)
	n = n + 1

        call mef_encodec ("ORIGIN", FITS_ORIGIN, strlen(FITS_ORIGIN),
		           card, "FITS file originator")
	call mef_pakwr (out, card)
	n = n + 1

        call mef_encode_date (Memc[path], SZ_PATHNAME)
	call mef_encodec ("DATE", Memc[path], strlen(Memc[path]), 
			card, "Date FITS file was generated")
	call mef_pakwr (out, card)
	n = n + 1

	# Write a header file if one is given
	if (hdrfname[1] != EOS) {
	   fd = open (hdrfname, READ_ONLY, TEXT_FILE)
	   nchars = getline(fd, Memc[path])
	   repeat {
	       if ((strncmp (Memc[path], "SIMPLE", 6) == 0) ||
	           (strncmp (Memc[path], "BITPIX", 6) == 0) ||
	           (strncmp (Memc[path], "NAXIS",  5) == 0) )
	            nchars = getline(fd, Memc[path])
	       for (op=nchars-1; op <= LEN_CARD; op=op+1)
		   Memc[path+op] = ' '
	       Memc[path+LEN_CARD] = EOS
	       call mef_pakwr (out, Memc[path])
	       n = n + 1
	       if (n == 36)
	           n = 0
	       nchars = getline(fd, Memc[path])
	   } until (nchars == EOF)
	   call close (fd)
	}

        Memc[path] = ' '
	call amovkc (Memc[path], card, 80)
	call strcpy ("END", card, 3)
	card[4] = ' '                           # Clear EOS mark
	call mef_pakwr (out, card)

	n = n + 1

	call amovkc (" ", card, 80)
	nlines = 36 - n
	for (i=1; i<= nlines; i=i+1)
	   call mef_pakwr (out, card)

	call sfree (sp)
end
