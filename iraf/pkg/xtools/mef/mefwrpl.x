include <error.h>
include <pkg/mef.h>

define MEF_PLSIZE  MEF_CGROUP
# MEF_WRPL -- 

procedure mef_wrpl (mef, title, ctime,mtime, limtime, minval, 
   	      maxval,plbuf, naxis, axlen)

char    title[ARB]
long	ctime, mtime, limtime
real	minval, maxval
pointer	mef		#I input mef descriptor
short   plbuf		#I Pixel list buffer
int	naxis
long	axlen[ARB]

size_t	sz_val
pointer sp, ln, mii, hb
char    blank[1]
int	output_lines, i
size_t	npad
int	pcount, fd, nlines
bool    endk, new_outf
int	modi()
errchk  open, fcopyo

begin
	call smark (sp)
	sz_val = LEN_CARDNL
	call salloc (ln, sz_val, TY_CHAR)

	# Output file descriptor
	fd  = MEF_FD(mef)

	new_outf = false
	if (MEF_ACMODE(mef) == NEW_IMAGE)
	   new_outf = true

	output_lines = 0
	endk = false

	# Create a PHU
	if (new_outf) {
	    # Must create a dummy header if input extension is not image
	    Memc[ln] = EOS
	    call mef_dummyhdr (fd, Memc[ln])
	    new_outf = false
	} 

	call mef_wcardc ("XTENSION", "BINTABLE", "Extension type", fd)
	call mef_wcardi ("BITPIX", 8, "Default value", fd)
	call mef_wcardi ("NAXIS", 2, "Lines and cols", fd)
	call mef_wcardi ("NAXIS1", 8, "Nbytes per line", fd)
	call mef_wcardi ("NAXIS2", 1, "Nlines", fd)
	
	# Calculate the number of 2880 bytes block the heap will
	# occupy.

	pcount = ((MEF_PLSIZE(mef)+1439)/1440)*2880
	call mef_wcardi ("PCOUNT", pcount, "Heap size in bytes", fd)
	call mef_wcardi ("GCOUNT", 1, "1 Group", fd)
	call mef_wcardi ("TFIELDS", 1, "1 Column field", fd)
	call sprintf (Memc[ln], LEN_CARD, "PI(%d)")
	    call pargi(MEF_PLSIZE(mef))
	call mef_wcardc ("TFORM1", Memc[ln], "Variable word array", fd)
	call mef_wcardb ("INHERIT", NO, "No Inherit", fd)
        call mef_wcardc ("ORIGIN", FITS_ORIGIN, "FITS file originator", fd)
	call mef_wcardc ("EXTNAME", MEF_EXTNAME(mef), "", fd)
	call mef_wcardi ("EXTVER", MEF_EXTVER(mef), "", fd)
	call mef_wcardl ("CTIME", ctime, "", fd)
	call mef_wcardl ("MTIME", mtime, "", fd)
	call mef_wcardl ("LIMTIME", limtime, "", fd)
	call mef_wcardr ("DATAMIN", minval, "", fd)
	call mef_wcardr ("DATAMAX", maxval, "", fd)
	call mef_wcardc ("OBJECT", title, "", fd)

	call mef_wcardb ("CMPIMAGE", YES, "Is a compressed image", fd)
	call mef_wcardc ("CMPTYPE", "PLIO_1", "IRAF image masks", fd)
	call mef_wcardi ("CBITPIX", 32, "BITPIX for uncompressed image", fd)
	call mef_wcardi ("CNAXIS", naxis, "NAXIS for uncompressed image", fd)
	do i = 1, naxis {
	    call sprintf (Memc[ln], LEN_CARD, "NAXIS%d")
		call pargi(i)
	    call mef_wcardl ("CNAXIS", axlen[i], "axis length", fd)
	}
	
	hb = MEF_HDRP(mef)	
	output_lines = 23
	nlines = MEF_HSIZE(mef) / LEN_CARDNL

	for (i=1; i<= nlines; i=i+1) {
	    call mef_pakwr (fd, Memc[hb])
	    hb = hb + LEN_CARDNL
	}

        blank[1] = ' '
	sz_val = 80
	call amovkc (blank, Memc[ln], sz_val)
	call strcpy ("END", Memc[ln], 3)
	Memc[ln+3] = ' '                           # Clear EOS mark
	call mef_pakwr (fd, Memc[ln])

	output_lines = output_lines + nlines + 1 + naxis
	call mef_wrblank (fd, output_lines)

	sz_val = 1400
	call salloc (mii, sz_val, TY_INT)

	# Now write 2 integers as table data (nelem,offset)
	Memi[mii] = MEF_PLSIZE(mef)	 # Number of words in pl buff (2bytes)
	Memi[mii+1] = 0			 # Offset from start of heap

	npad = 1438
	call amovki (0, Memi[mii+2], npad)
	sz_val = 1440
	# arg2 : incompatible pointer
	call write (fd, Memi[mii], sz_val)

	# Write mask in heap area
	sz_val = MEF_PLSIZE(mef)*SZ_SHORT
	call write (fd, plbuf, sz_val)

	# Pad to 1440 characters block in case we want to append another
	# extension

	npad = 1440 - modi(MEF_PLSIZE(mef), 1440)

	call amovki (0, Memi[mii], npad)
	# arg2 : incompatible pointer
	call write (fd, Memi[mii], npad)
							 

	call sfree(sp)
end

procedure mef_wcardi (kname, kvalue, kcomm, fd)

char	kname[ARB]	#I Keyword name
int	kvalue	        #I Keyword value
char 	kcomm[ARB]	#I Card comment
int	fd	        #I file descriptor

size_t	sz_val
pointer sp, ln

begin

	call smark (sp)
	sz_val = LEN_CARDNL
	call salloc (ln, sz_val, TY_CHAR)

	call mef_encodei (kname, kvalue, Memc[ln], kcomm)
	call mef_pakwr (fd, Memc[ln])

	call sfree (sp)

end


procedure mef_wcardl (kname, kvalue, kcomm, fd)

char	kname[ARB]	#I Keyword name
long	kvalue	        #I Keyword value
char 	kcomm[ARB]	#I Card comment
int	fd	        #I file descriptor

size_t	sz_val
pointer sp, ln

begin

	call smark (sp)
	sz_val = LEN_CARDNL
	call salloc (ln, sz_val, TY_CHAR)

	call mef_encodel (kname, kvalue, Memc[ln], kcomm)
	call mef_pakwr (fd, Memc[ln])

	call sfree (sp)

end


procedure mef_wcardc (kname, kvalue, kcomm, fd)

char	kname[ARB]	#I Keyword name
char	kvalue[ARB]	#I Keyword value
char 	kcomm[ARB]	#I Card comment
int	fd   	        #I file descriptor

size_t	sz_val
pointer sp, ln
int	slen
int	strlen()

begin

	call smark (sp)
	sz_val = LEN_CARDNL
	call salloc (ln, sz_val, TY_CHAR)

	slen = strlen(kvalue)
	call mef_encodec (kname, kvalue, slen, Memc[ln], kcomm)
	call mef_pakwr (fd, Memc[ln])

	call sfree(sp)

end


procedure mef_wcardb (kname, kvalue, kcomm, fd)

char	kname[ARB]	#I Keyword name
int	kvalue	        #I Keyword value
char 	kcomm[ARB]	#I Card comment
int	fd	        #I file descriptor

size_t	sz_val
pointer sp, ln

begin

	call smark (sp)
	sz_val = LEN_CARDNL
	call salloc (ln, sz_val, TY_CHAR)

	call mef_encodeb (kname, kvalue, Memc[ln], kcomm)
	call mef_pakwr (fd, Memc[ln])

	call sfree(sp)

end

procedure mef_wcardr (kname, kvalue, kcomm, fd)

char	kname[ARB]	#I Keyword name
real	kvalue		#I Keyword value
char 	kcomm[ARB]	#I Card comment
int	fd	        #I file descriptor

size_t	sz_val
pointer sp, ln

begin

	call smark (sp)
	sz_val = LEN_CARDNL
	call salloc (ln, sz_val, TY_CHAR)

	call mef_encoder (kname, kvalue, Memc[ln], kcomm, 6)
	call mef_pakwr (fd, Memc[ln])

	call sfree(sp)

end

