include <mach.h>
include <imhdr.h>
include <imio.h>
include <imset.h>
include <fio.h>
include "rfits.h"

define	SZ_KEYWORD	8
# UPDATE_GPB -- Procedure to update the WCS values. Since the image
# created by rfits has access NEW_IMAGE, the gpb values are taken from
# a template file and not from the fits header; this routine copy those
# values into the image descriptor. This routines is called only if the
# image created is of type STF.

procedure update_gpb (im, fits)

pointer	im		# image descriptor
pointer fits		# rfits descriptor

pointer pn
char	keyname[SZ_KEYWORD]
int	k, i, ndim

begin

	IM_UPDATE(im) = YES
	ndim = IM_NDIM(im)
	if (MAKE_CD(fits) == YES)
	   call cd_matrix (im, fits)
	
	if (ndim != 0) {
	   call sprintf (keyname, SZ_KEYWORD, "DATAMIN")
	   call imputr (im, keyname, IM_MIN(im))	    
	   call sprintf (keyname, SZ_KEYWORD, "DATAMAX")
	   call imputr (im, keyname, IM_MAX(im))	    
	}

	do k = 1, ndim {
	   pn = WCS_PDES(fits,k)
	   call sprintf (keyname, SZ_KEYWORD, "CRVAL%d")
	        call pargi(k)	       
	   call imputd (im, keyname, CRVAL(pn))	    

	   call sprintf (keyname, SZ_KEYWORD, "CRPIX%d")
	        call pargi(k)	       
	   call imputr (im, keyname, CRPIX(pn))	    

	   call sprintf (keyname, SZ_KEYWORD, "CTYPE%d")
	        call pargi(k)	       
	   call impstr (im, keyname, CTYPE(pn))	    

	   do i = 1, ndim {
	      call sprintf (keyname, SZ_KEYWORD, "CD%d_%d")
	          call pargi(k)
		  call pargi(i)
	      call imputr (im, keyname, CDMATRIX(pn,i))	    
	   }
	}
end

include <math.h>
# CD_MATRIX -- Procedure to calculate the CD matrix from the CDELT and CROTA
# values.

procedure cd_matrix (im, fits)

pointer im		# image descriptor
pointer	fits		# rfits descriptor

pointer pn, pn1, pn2
int	ndim, sign1, sign2, i
double	sinrota, cosrota, radcrota

begin

	ndim = IM_NDIM(im)

#	Convert CROTA and CDELT into the CD_ Matrix
#       1_D case is trivial since CROTA = 0

	pn1 = WCS_PDES(fits, 1)
	if ( ndim == 1 ) {

	   CDMATRIX(pn1, ndim) =  CDELT(pn1)

	} else if ( ndim >= 2 ) {
	   pn2 = WCS_PDES(fits, 2)
#        Note that for coordinates for which CDELT/CROTA are fully descriptive
#        CROTA(1) = CROTA(2); if this is not the case then the CDi_j matrix
#        should be used directly and the following calculations are incorrect!
           radcrota = CROTA(pn1) / RADIAN
	   cosrota = cos (radcrota)
	   sinrota = sin (radcrota)
	   sign1 = 1
	   sign2 = 1
	   if (CDELT(pn1) < 0)
	      sign1= -1
	   if (CDELT(pn2) < 0)
	      sign2= -1
#--                                                 cd1_1
	   CDMATRIX(pn1,1) =   CDELT(pn1) * cosrota
#--                                                 cd1_2
	   CDMATRIX(pn1,2) =   abs(CDELT(pn2)) * sign1 * sinrota
#--                                                 cd2_1
	   CDMATRIX(pn2,1) = - abs(CDELT(pn1)) * sign2 * sinrota
#--                                                 cd2_2
	   CDMATRIX(pn2,2) =   CDELT(pn2) * cosrota

	}
	if (ndim > 2)
	   do i = 3, ndim {
	      pn = WCS_PDES(fits, i)
	      CDMATRIX(pn, i) = CDELT(pn)
	   }			
end

define	SZ_KEYWORD	8
define	NEPSILON	10.0d0
# RFT_CREATE_GPB -- Procedure to create WCS names. Since the image
# created by rfits has access NEW_IMAGE, the gpb names are taken from
# a template file and not from the fits header. This routine copy those
# names into the image descriptor. This routines is called only if the
# image created is of type STF.

procedure rft_create_gpb (im, fd)

pointer	im		# image descriptor
pointer	fd		# text file descriptor 

char	keyname[SZ_KEYWORD], card[LEN_CARD+1]
int	k, i, ndim
string	one "1"
string	zero "0"

begin

	
	IM_UPDATE(im) = YES
	ndim = IM_NDIM(im)
	
	if (ndim != 0) {
	   call sprintf (keyname, SZ_KEYWORD, "DATAMIN")
	   call wft_encodec (keyname, zero, card, "")
	   card[LEN_CARD+1] = '\n'
	   card[LEN_CARD+2] = EOS
	   call putline (fd, card)
   
	   call sprintf (keyname, SZ_KEYWORD, "DATAMAX")
	   call wft_encodec (keyname, zero, card, "")
	   card[LEN_CARD+1] = '\n'
	   card[LEN_CARD+2] = EOS
	   call putline (fd, card)
	}

	do k = 1, ndim {
	   call sprintf (keyname, SZ_KEYWORD, "CRVAL%d")
	        call pargi(k)	       
	   call wft_encodec (keyname, one, card, "")
	   card[LEN_CARD+1] = '\n'
	   card[LEN_CARD+2] = EOS
	   call putline (fd, card)

	   call sprintf (keyname, SZ_KEYWORD, "CRPIX%d")
	        call pargi(k)	       
	   call wft_encodec (keyname, one, card, "")
	   card[LEN_CARD+1] = '\n'
	   card[LEN_CARD+2] = EOS
	   call putline (fd, card)

	   call sprintf (keyname, SZ_KEYWORD, "CTYPE%d")
	        call pargi(k)	       
	   call wft_encodec (keyname, "PIXEL", card, "")
	   card[LEN_CARD+1] = '\n'
	   card[LEN_CARD+2] = EOS
	   call putline (fd, card)

	   do i = 1, ndim {
	      call sprintf (keyname, SZ_KEYWORD, "CD%d_%d")
	          call pargi(k)
		  call pargi(i)
	      if (i == k)
	         call wft_encodec (keyname, one, card, "")
	      else
	         call wft_encodec (keyname, zero, card, "")
	      card[LEN_CARD+1] = '\n'
	      card[LEN_CARD+2] = EOS
	      call putline (fd, card)
	   }
	}
end

# RFT_HMS -- Procedure to decode a FITS HMS card from the mountain

int procedure rft_hms (card, str, maxch)

char	card[LEN_CARD]		# FITS card
char	str[LEN_CARD]		# string
int	maxch			# maximum number of characters

int	i, fst, lst, len, nmin, nsec

char	ch
int	stridx(), strldx(), strlen()

begin
	# return if not a FITS string parameter
	if (card[COL_VALUE] != '\'')
	    return (0)

	# get the FITS string
	call rft_get_fits_string (card, str, maxch)

	# test for blank string and for 2 colon delimiters
	if (str[1] == EOS)
	    return (0)
	ch = ':'
	fst = stridx (ch, str)
	if (fst == 0)
	    return (0)
	lst = strldx (ch, str)
	if (lst == 0)
	    return (0)
	if (fst == lst)
	    return (0)

	len = strlen (str)
	if (str[1] == '-') {
	    nmin = lst - fst - 1
	    nsec = len - lst
	    if (nmin == 2)
		str[fst+1] = '0'
	    else {
		do i = fst + 2, lst 
		    str[i-1] = str[i]
		lst = lst - 1
		len = len - 1
		str[len+1] = EOS
	    }
	    if (nsec == 2)
		str[lst+1] = '0'
	    else {
		do i = lst + 2, len
		    str[i-1] = str[i]
		len = len - 1
		str[len+1] = EOS
	    }
	} else {
	    do i = 1, len {
	        if (str[i] == ' ' && i != 1)
		    str[i] = '0'
	    }
	}

	return (len)
end

# RFT_GET_FITS_STRING -- Extract a string from a FITS card and trim trailing
# blanks. The EOS is marked by either ', /, or the end of the card.
# There may be an optional opening ' (FITS standard).

procedure rft_get_fits_string (card, str, maxchar)

char	card[LEN_CARD]		# FITS card
char	str[LEN_CARD]		# FITS string
int	maxchar			# maximum number of characters

int	j, istart, nchar

begin
	# Check for opening quote
	for (istart = COL_VALUE; istart <= LEN_CARD && card[istart] != '\'';
	    istart = istart + 1)
	    ;
	istart = istart + 1

	# closing quote
	for (j = istart; (j<LEN_CARD)&&(card[j]!='\''); j = j + 1)
	    ;
	for (j = j - 1; (j >= istart) && (card[j] == ' '); j = j - 1)
	    ;
	nchar = min (maxchar, j - istart + 1)

	# copy string
	if (nchar <= 0)
	    str[1] = EOS
	else
	    call strcpy (card[istart], str, nchar)
end


# RFT_EQUALD -- Procedure to compare two double precision numbers for equality
# to within the machine precision for doubles.

bool procedure rft_equald (x, y)

double	x, y		# the two numbers to be compared for equality

int	ex, ey
double	x1, x2, normed_x, normed_y

begin
	if (x == y)
	    return (true)

	call rft_normd (x, normed_x, ex)
	call rft_normd (y, normed_y, ey)

	if (ex != ey)
	    return (false)
	else {
	    x1 = 1.0d0 + abs (normed_x - normed_y)
	    x2 = 1.0d0 + NEPSILON * EPSILOND
	    return (x1 <= x2)
	}
end


# RFT_NORMED -- Normalize a double precision number x to the value normed_x,
# in the range [1-10]. Expon is returned such that x = normed_x *
# (10.0d0 ** expon).

procedure rft_normd (x, normed_x, expon)

double	x			# number to be normailized
double	normed_x		# normalized number
int	expon			# exponent

double	ax

begin
	ax = abs (x)
	expon = 0

	if (ax > 0) {
	    while (ax < (1.0d0 - NEPSILON * EPSILOND)) {
		ax = ax * 10.0d0
		expon = expon - 1
	    }

	    while (ax >= (10.0d0 - NEPSILON * EPSILOND)) {
		ax = ax / 10.0d0
		expon = expon + 1
	    }
	}

	if (x < 0)
	    normed_x = -ax
	else
	    normed_x = ax
end


# RFT_TRIM_CARD -- Procedure to trim trailing whitespace from the card

procedure rft_trim_card (incard, outcard, maxch)

char	incard[LEN_CARD]		# input FITS card image 
char	outcard[LEN_CARD]		# output FITS card
int	maxch			# maximum size of card

int	ip

begin
	ip = maxch
	while (incard[ip] == ' ')
	    ip = ip - 1

	call amovc (incard, outcard, ip)

	outcard[ip+1] = '\n'
	outcard[ip+2] = EOS
end

# RFT_LAST_USER -- Remove a partially written card from the data base

procedure rft_last_user (user, maxch) 

char	user[LEN_CARD]	# user area
int	maxch		# maximum number of characters

int	ip

begin
	ip = maxch
	while (user[ip] != '\n')
	    ip = ip - 1
	user[ip+1] = EOS
end
# RFT_CLEAN_CARD -- Procedure to clean HISTORY card from any null value

procedure rft_clean_card (incard, outcard, maxch)

char	incard[LEN_CARD]		# input FITS card image 
char	outcard[LEN_CARD]		# output FITS card
int	maxch			# maximum size of card

int	ip

begin
	do ip = 1, maxch  {
	   if (incard[ip] == NULL) {
	    call printf("%s \n")
	      call pargstr(incard)
	      incard[ip] = ' '
	   }
	}
	call amovc (incard, outcard, maxch)

	outcard[maxch+1] = '\n'
	outcard[maxch+2] = EOS
end
