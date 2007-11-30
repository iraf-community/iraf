include <error.h>
include <mach.h>
include <ctype.h> 
include <fset.h>
include <pkg/mef.h>

# MEFRDHR.X -- Routines to read FITS header units.
#
#     eof|stat = mef_rdhdr (mef, group, extname, extver)
# 	         mef_skip_data_unit (mef)
#	totpix = mef_totpix (mef)
#     eof|stat = mef_rdhdr_gn (mef,gn)
#     eof|stat = mef_rdhdr_exnv (mef,extname, extver)


# MEF_RDHR -- Read FITS header on a mef file that matches EXTNAME/EXTVER or 
# GROUP number. If both are specified, the former takes procedence.

int procedure mef_rdhdr (mef, group, extname, extver)

pointer	mef		#I Mef descriptor
int	group 		#I Group number to read
char    extname[ARB]    #I Extname to read		
int	extver		#I Extver to read

int	open(),in, cur_extn, note(), gnum
int	spool
bool	extnv, read_next_group
int	mef_load_header(), mef_pixtype()
bool    mef_cmp_extnv
errchk	open, read, mef_load_header

begin
	if (group == MEF_CGROUP(mef))
	    return (group)

	gnum = group
	if (MEF_FD(mef) == NULL) {
	    MEF_FD(mef) = open (MEF_FNAME(mef), READ_ONLY, BINARY_FILE)
	    MEF_ENUMBER(mef) = -1 
	    MEF_CGROUP(mef) = -1 
	} 
	MEF_SKDATA(mef) = NO  

	in = MEF_FD(mef)

	extnv = extname[1] != EOS || extver != INDEFL
	spool = open ("spool", NEW_FILE, SPOOL_FILE)

	if (gnum == -1 || extnv)
	    gnum = 0
	   
	cur_extn = MEF_CGROUP(mef)
	read_next_group = true

	repeat {
	   # If we need to read the next group
	   if (read_next_group) {

	       cur_extn = cur_extn+1

	       # See if this extension contains the correct 
	       # extname/extver values.

	       call fseti (spool, F_CANCEL, YES)
	       if (mef_load_header (mef, spool, cur_extn) == EOF) {
		   call close (spool)
		   return (EOF)
	       }			

	       # We read the header already, marked the spot.
	       MEF_POFF(mef) = note(in)

	       if (extnv) {
		   read_next_group = mef_cmp_extnv (mef, extname, extver)
	       } else {
		   if (gnum == cur_extn) 
		       read_next_group = false
	       }
	       call mef_skip_data_unit (mef)
	       next

	   } else {			# This is the group we want
	       if (MEF_HDRP(mef) != NULL)
		   call mfree (MEF_HDRP(mef), TY_CHAR)

	       call mef_cp_spool (spool, mef)
	       MEF_CGROUP(mef) = cur_extn

	       # To indicate that data has been skipped.
	       MEF_SKDATA(mef) = YES  
	       break
	   }
	}
	call close (spool)
	MEF_DATATYPE(mef) = mef_pixtype(mef)
	return (cur_extn)
end

int procedure mef_pixtype (mef)
pointer mef, hdrp
bool    bfloat, lscale, lzero
bool    fxf_fpl_equald()
int	i, impixtype, ctod(), ip
double  bscale, bzero
char    sval[LEN_CARD]

begin
        hdrp= MEF_HDRP(mef)
	bscale = 1.0d0
	ip=1
	ifnoerr (call mef_findkw (hdrp, "BSCALE", sval))
	     i = ctod(sval,ip,bscale)
	bzero = 0.0d0
	ip=1
	ifnoerr (call mef_findkw (hdrp, "BZERO", sval))
	     i = ctod(sval,ip,bzero)

	lscale = fxf_fpl_equald (1.0d0, bscale, 1)
	lzero =  fxf_fpl_equald (0.0d0, bzero, 1)

	# Determine if scaling is necessary.
	bfloat = (!lscale || !lzero)

	switch (MEF_BITPIX(mef)) {
	case  8:
	    if (bfloat)
		impixtype = TY_REAL
	    else
		impixtype = TY_SHORT              # convert from byte to short
	case 16:
	    if (bfloat) {
		impixtype = TY_REAL
	    } else
		impixtype = TY_SHORT

	    if (lscale && fxf_fpl_equald (32768.0d0, bzero, 4)) {
		impixtype = TY_USHORT
	    }
	case 32:
	    if (bfloat)
		impixtype = TY_REAL
	    else
		impixtype = TY_INT
	case -32:
	    impixtype = TY_REAL
	case -64:
	    impixtype = TY_DOUBLE
	default:
	    impixtype = ERR
	}

	return(impixtype)

end

# MEF_CMP_EXTNV -- Compare the EXTNAME and EXTVER header values with the
#		  ones passed as arguments. Return false if matched.

bool procedure mef_cmp_extnv (mef, extname, extver)
pointer	mef
char	extname[ARB] 	#I extname value
int	extver		#I extver value

int	mef_strcmp_lwr()
bool	bxtn, bxtv, bval, bxtn_eq, bxtv_eq

begin
	bxtn = extname[1] != EOS
	bxtv = extver != INDEFL

	if (bxtn)
	    bxtn_eq = (mef_strcmp_lwr(MEF_EXTNAME(mef), extname) == 0)
	if (bxtv)
	    bxtv_eq = (MEF_EXTVER(mef) == extver)
	
	if (bxtn && bxtv)
	    # Both EXTNAME and EXTVER are defined.
	    bval = bxtn_eq && bxtv_eq
	else if (bxtn && !bxtv)
	    # Only EXTNAME is defined.
	    bval = bxtn_eq
	else if (!bxtn && bxtv)
	    # Only EXTVER is defined.
	    bval = bxtv_eq 
	else
	    bval = false

	return (!bval)
end

# MEF_SKIP_DATA_UNIT -- Skip data unit. The file is already position at the
# end of the last header block.

procedure mef_skip_data_unit (mef)

pointer	mef	#I Input mef descriptor

int	in, ndim, off, note(), mef_totpix()
errchk  seek

begin
	# See if data portion has already been skipped.
	if (MEF_SKDATA(mef) == YES)
	    return

	in = MEF_FD(mef)
	ndim = MEF_NDIM (mef)
        if (ndim > 0 || MEF_PCOUNT(mef) > 0) {
	   # Skip to the beginning of next extension
	   off = note(in)
	   if (off == EOF)
	      return
	   off = off + mef_totpix(mef)
	   call seek (in, off)
	}
end


# MEF_TOTPIX -- Returns the number of pixels in the data area in units
#		of chars.

int procedure mef_totpix (mef)

pointer	mef		#I Mef descriptor

int	ndim, totpix, i, bitpix

begin
	ndim = MEF_NDIM (mef)
	if (ndim == 0 && MEF_PCOUNT(mef) <= 0)
	    return (0)

	if (ndim == 0)
	    totpix = 0
	else {
	    totpix = MEF_NAXIS(mef,1) 
	    do i = 2, ndim
		  totpix = totpix *  MEF_NAXIS(mef,i)
	}
	bitpix = abs(MEF_BITPIX(mef))

	# If PCOUNT is not zero, add it to totpix
	totpix = MEF_PCOUNT(mef) + totpix

	if (bitpix <= NBITS_BYTE)
	    totpix = (totpix + 1) / SZB_CHAR
	else
	    totpix = totpix * (bitpix / (SZB_CHAR * NBITS_BYTE))

	# Set the number of characters in multiple of 1440.
	totpix = ((totpix + 1439)/1440) * 1440
	return (totpix)
end


# MEF_STRCMP_LWR -- Compare 2 strings in lower case

int procedure mef_strcmp_lwr (s1, s2)

char s1[ARB], s2[ARB]

pointer sp, l1, l2
int	strcmp(), istat

begin
	call smark(sp)
	call salloc (l1, SZ_FNAME, TY_CHAR)
	call salloc (l2, SZ_FNAME, TY_CHAR)

	call strcpy (s1, Memc[l1], SZ_FNAME)
	call strcpy (s2, Memc[l2], SZ_FNAME)
	call strlwr(Memc[l1])
	call strlwr(Memc[l2])
	istat = strcmp (Memc[l1], Memc[l2])

	call sfree(sp)
	return (istat)
end


# MEF_KCTYPE -- Find the type of card that is based on the keyword name.

int procedure mef_kctype (card, index)

char	card[ARB]	#I FITS card
int	index		#O index value

int	strncmp()

begin
	if (strncmp (card, "SIMPLE  ", 8) == 0)
	   return (SIMPLE)
	if (strncmp (card, "NAXIS", 5) == 0) {
	   if (card[6] == ' ') {
	      call mef_gvali (card, index)
	      return (NAXIS)
    	   } else  if (IS_DIGIT(card[6])) {
	       index = TO_INTEG(card[6])
	       return (NAXISN)                      # NAXISn
     	   }
	}
	if (strncmp (card, "BITPIX  ", 8) == 0)
	   return (BITPIX)
	if (strncmp (card, "EXTNAME ", 8) == 0)
	   return (EXTNAME)
	if (strncmp (card, "EXTVER  ", 8) == 0)
	   return (EXTVER)
	if (strncmp (card, "EXTEND  ", 8) == 0)
	   return (EXTEND)
	if (strncmp (card, "PCOUNT  ", 8) == 0)
	   return (PCOUNT)
	if (strncmp (card, "FILENAME", 8) == 0)
	   return (FILENAME)
	if (strncmp (card, "INHERIT ", 8) == 0)
	   return (INHERIT)
	if (strncmp (card, "GCOUNT  ", 8) == 0)
	   return (GCOUNT)
	if (strncmp (card, "OBJECT  ", 8) == 0)
	   return (OBJECT)
	if (strncmp (card, "XTENSION", 8) == 0)
	   return (XTENSION)
	if (strncmp (card, "END     ", 8) == 0)
	   return (END)

        return(ERR)
end


# MEF_RDHDR_GN -- Read group based on group number

int procedure mef_rdhdr_gn (mef,gn)

pointer	mef 		#I mef descriptor
int	gn		#I group number to read

char	extname[MEF_SZVALSTR]
int	extver
int	mef_rdhdr()

errchk  mef_rdhdr

begin
	extname[1] =EOS
	extver=INDEFL
	return (mef_rdhdr (mef, gn, extname, extver))
end


# MEF_RDHDR_EXNV -- Read group based on the Extname and Extver values

int procedure mef_rdhdr_exnv (mef,extname, extver)

pointer	mef		#I, mef descriptor
char	extname[ARB]	#I, extname value
int	extver		#I, extver value
int     mef_rdhdr()

errchk  mef_rdhdr

begin
	return (mef_rdhdr (mef, 0, extname, extver))
end


# MEF_CP_SPOOL -- 

procedure mef_cp_spool (spool, mef)

int	spool 		#I spool file descriptor
pointer mef		#

pointer	hdr, lbuf, sp
int	fitslen, fstatl, user
int	stropen(), getline()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	call seek (spool, BOFL)
	fitslen = fstatl (spool, F_FILESIZE)
	fitslen = max (fitslen, MEF_HSIZE(mef))
	call malloc (hdr, fitslen, TY_CHAR)
	user = stropen (Memc[hdr], fitslen, NEW_FILE)

	# Append the saved FITS cards to saved cache.
	while (getline (spool, Memc[lbuf]) != EOF)
	    call putline (user, Memc[lbuf])

	call close (user)
	call close (spool)

	MEF_HDRP(mef) = hdr

	call sfree(sp)
end
