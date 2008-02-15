include "psfmatch.h"

#  RG_PINIT -- Initialize the main psfmatch data structure. 

procedure rg_pinit (pm, cfunc)

pointer	pm		#O pointer to psfmatch structure
int	cfunc		#I mode of computing the convolution function

begin
	call malloc (pm, LEN_PSFSTRUCT, TY_STRUCT)

	# Initialize the pointers.
	PM_RC1(pm) = NULL
	PM_RC2(pm) = NULL
	PM_RL1(pm) = NULL
	PM_RL2(pm) = NULL
	PM_RZERO(pm) = NULL
	PM_RXSLOPE(pm) = NULL
	PM_RYSLOPE(pm) = NULL
	PM_NREGIONS(pm) = 0
	PM_CNREGION(pm) = 1
		
	# Define the background fitting parameters.
	PM_CENTER(pm) = DEF_CENTER
	PM_BACKGRD(pm) = DEF_BACKGRD
	PM_BVALUER(pm) = 0.0
	PM_BVALUE(pm) = 0.0
	call strcpy ("median", PM_BSTRING(pm), SZ_FNAME)
	PM_LOREJECT(pm) = DEF_LOREJECT
	PM_HIREJECT(pm) = DEF_HIREJECT
	PM_APODIZE(pm) = 0.0

	PM_UFLUXRATIO(pm) = DEF_UFLUXRATIO
	PM_FILTER(pm) = DEF_FILTER
	call strcpy ("replace", PM_FSTRING(pm), SZ_FNAME)
	PM_SXINNER(pm) = DEF_SXINNER
	PM_SXOUTER(pm) = DEF_SXOUTER
	PM_SYINNER(pm) = DEF_SYINNER
	PM_SYOUTER(pm) = DEF_SYOUTER
	PM_RADSYM(pm) = DEF_RADSYM
	PM_THRESHOLD(pm) = DEF_THRESHOLD

	PM_NORMFACTOR(pm) = DEF_NORMFACTOR

	PM_CONVOLUTION(pm) = cfunc
	switch (cfunc) {
	case PM_CONIMAGE:
	    PM_CONVOLUTION(pm) = PM_CONIMAGE
	    call strcpy ("image", PM_CSTRING(pm), SZ_FNAME)
	case PM_CONPSF:
	    PM_CONVOLUTION(pm) = PM_CONPSF
	    call strcpy ("psf", PM_CSTRING(pm), SZ_FNAME)
	case PM_CONKERNEL:
	    PM_CONVOLUTION(pm) = PM_CONKERNEL
	    call strcpy ("kernel", PM_CSTRING(pm), SZ_FNAME)
	default:
	    PM_CONVOLUTION(pm) = PM_CONIMAGE
	    call strcpy ("image", PM_CSTRING(pm), SZ_FNAME)
	}
	PM_DNX(pm) = DEF_DNX
	PM_DNY(pm) = DEF_DNY
	PM_PNX(pm) = DEF_PNX
	PM_PNY(pm) = DEF_PNY
	PM_KNX(pm) = 0
	PM_KNY(pm) = 0
	PM_POWER(pm) = DEF_POWER

	PM_REFFFT(pm) = NULL
	PM_IMFFT(pm) = NULL
	PM_FFT(pm) = NULL
	PM_CONV(pm) = NULL
	PM_ASFFT(pm) = NULL
	PM_NXFFT(pm) = 0
	PM_NYFFT(pm) = 0

	# Initialize the strings.
	PM_IMAGE(pm) = EOS
	PM_REFIMAGE(pm) = EOS
	PM_PSFDATA(pm) = EOS
	PM_PSFIMAGE(pm) = EOS
	PM_OBJLIST(pm) = EOS
	PM_KERNEL(pm) = EOS
	PM_OUTIMAGE(pm) = EOS

	# Initialize the buffers.
	call rg_prinit (pm)
end


#  RG_PRINIT -- Initialize the regions definition portion of the psf matching
# code fitting structure. 

procedure rg_prinit (pm)

pointer	pm		#I pointer to psfmatch structure

begin
	call rg_prfree (pm)

	PM_NREGIONS(pm) = 0
	PM_CNREGION(pm) = 1

	call malloc (PM_RC1(pm), MAX_NREGIONS, TY_INT)
	call malloc (PM_RC2(pm), MAX_NREGIONS, TY_INT)
	call malloc (PM_RL1(pm), MAX_NREGIONS, TY_INT)
	call malloc (PM_RL2(pm), MAX_NREGIONS, TY_INT)
	call malloc (PM_RZERO(pm), MAX_NREGIONS, TY_REAL)
	call malloc (PM_RXSLOPE(pm), MAX_NREGIONS, TY_REAL)
	call malloc (PM_RYSLOPE(pm), MAX_NREGIONS, TY_REAL)

	call amovki (INDEFI, Memi[PM_RC1(pm)], MAX_NREGIONS)
	call amovki (INDEFI, Memi[PM_RC2(pm)], MAX_NREGIONS)
	call amovki (INDEFI, Memi[PM_RL1(pm)], MAX_NREGIONS)
	call amovki (INDEFI, Memi[PM_RL2(pm)], MAX_NREGIONS)
	call amovkr (INDEFR, Memr[PM_RZERO(pm)], MAX_NREGIONS)
	call amovkr (INDEFR, Memr[PM_RXSLOPE(pm)], MAX_NREGIONS)
	call amovkr (INDEFR, Memr[PM_RYSLOPE(pm)], MAX_NREGIONS)
end


# RG_PINDEFR -- Re-initialize the background and answers regions portion of
# the psf-matching structure.

procedure rg_pindefr (pm)

pointer	pm		#I pointer to the psfmatch structure

int	nregions
int	rg_pstati ()

begin
	nregions = rg_pstati (pm, NREGIONS)

	if (nregions > 0) {
	    call amovkr (INDEFR, Memr[PM_RZERO(pm)], nregions)
	    call amovkr (INDEFR, Memr[PM_RXSLOPE(pm)], nregions)
	    call amovkr (INDEFR, Memr[PM_RYSLOPE(pm)], nregions)
	}
end


#  RG_PREALLOC -- Reallocate the regions buffers and initialize if necessary.

procedure rg_prealloc (pm, nregions)

pointer	pm		#I pointer to psfmatch structure
int	nregions	#I number of regions

int	nr
int	rg_pstati()

begin
	nr = rg_pstati (pm, NREGIONS)

	call realloc (PM_RC1(pm), nregions, TY_INT)
	call realloc (PM_RC2(pm), nregions, TY_INT)
	call realloc (PM_RL1(pm), nregions, TY_INT)
	call realloc (PM_RL2(pm), nregions, TY_INT)
	call realloc (PM_RZERO(pm), nregions, TY_REAL)
	call realloc (PM_RXSLOPE(pm), nregions, TY_REAL)
	call realloc (PM_RYSLOPE(pm), nregions, TY_REAL)

	call amovki (INDEFI, Memi[PM_RC1(pm)+nr], nregions - nr)
	call amovki (INDEFI, Memi[PM_RC2(pm)+nr], nregions - nr)
	call amovki (INDEFI, Memi[PM_RL1(pm)+nr], nregions - nr)
	call amovki (INDEFI, Memi[PM_RL2(pm)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[PM_RZERO(pm)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[PM_RXSLOPE(pm)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[PM_RYSLOPE(pm)+nr], nregions - nr)
	#call amovkr (INDEFR, Memr[PM_XSHIFTS(pm)+nr], nregions - nr)
	#call amovkr (INDEFR, Memr[PM_YSHIFTS(pm)+nr], nregions - nr)
end


# RG_PRFREE -- Free the regions portion of the psfmatch structure.

procedure rg_prfree (pm)

pointer	pm		#I/O pointer to psfmatch structure

begin
	call rg_pseti (pm, NREGIONS, 0)
	if (PM_RC1(pm) != NULL)
	    call mfree (PM_RC1(pm), TY_INT)
	PM_RC1(pm) = NULL
	if (PM_RC2(pm) != NULL)
	    call mfree (PM_RC2(pm), TY_INT)
	PM_RC2(pm) = NULL
	if (PM_RL1(pm) != NULL)
	    call mfree (PM_RL1(pm), TY_INT)
	PM_RL1(pm) = NULL
	if (PM_RL2(pm) != NULL)
	    call mfree (PM_RL2(pm), TY_INT)
	PM_RL2(pm) = NULL
	if (PM_RZERO(pm) != NULL)
	    call mfree (PM_RZERO(pm), TY_REAL)
	PM_RZERO(pm) = NULL
	if (PM_RXSLOPE(pm) != NULL)
	    call mfree (PM_RXSLOPE(pm), TY_REAL)
	PM_RXSLOPE(pm) = NULL
	if (PM_RYSLOPE(pm) != NULL)
	    call mfree (PM_RYSLOPE(pm), TY_REAL)
	PM_RYSLOPE(pm) = NULL
end


# RG_PFREE -- Free the psfmatch structure.

procedure rg_pfree (pm)

pointer	pm		#I pointer to psfmatch structure

begin
	# Free the region descriptors
	call rg_prfree (pm)

	if (PM_REFFFT(pm) != NULL)
	    call mfree (PM_REFFFT(pm), TY_REAL)
	if (PM_IMFFT(pm) != NULL)
	    call mfree (PM_IMFFT(pm), TY_REAL)
	if (PM_FFT(pm) != NULL)
	    call mfree (PM_FFT(pm), TY_REAL)
	if (PM_CONV(pm) != NULL)
	    call mfree (PM_CONV(pm), TY_REAL)
	if (PM_ASFFT(pm) != NULL)
	    call mfree (PM_ASFFT(pm), TY_REAL)

	call mfree (pm, TY_STRUCT)
end


# RG_PSTATI -- Fetch the value of a psfmatch task integer parameter.

int procedure rg_pstati (pm, param)

pointer	pm		# pointer to psfmatch structure
int	param		# parameter to be fetched

begin
	switch (param) {
	case NREGIONS:
	    return (PM_NREGIONS(pm))
	case CNREGION:
	    return (PM_CNREGION(pm))
	case CENTER:
	    return (PM_CENTER(pm))
	case BACKGRD:
	    return (PM_BACKGRD(pm))
	case CONVOLUTION:
	    return (PM_CONVOLUTION(pm))
	case DNX:
	    return (PM_DNX(pm))
	case DNY:
	    return (PM_DNY(pm))
	case PNX:
	    return (PM_PNX(pm))
	case PNY:
	    return (PM_PNY(pm))
	case KNX:
	    return (PM_KNX(pm))
	case KNY:
	    return (PM_KNY(pm))
	case POWER:
	    return (PM_POWER(pm))

	case FILTER:
	    return (PM_FILTER(pm))
	case RADSYM:
	    return (PM_RADSYM(pm))

	case NXFFT:
	    return (PM_NXFFT(pm))
	case NYFFT:
	    return (PM_NYFFT(pm))

	default:
	    call error (0, "RG_PSTATI: Unknown integer parameter.")
	}
end


# RG_PSTATP -- Fetch the value of a psfmatch task pointer parameter.

pointer procedure rg_pstatp (pm, param)

pointer	pm		# pointer to psfmatch structure
int	param		# parameter to be fetched

begin
	switch (param) {
	case RC1:
	    return (PM_RC1(pm))
	case RC2:
	    return (PM_RC2(pm))
	case RL1:
	    return (PM_RL1(pm))
	case RL2:
	    return (PM_RL2(pm))
	case RZERO:
	    return (PM_RZERO(pm))
	case RXSLOPE:
	    return (PM_RXSLOPE(pm))
	case RYSLOPE:
	    return (PM_RYSLOPE(pm))
	case REFFFT:
	    return (PM_REFFFT(pm))
	case IMFFT:
	    return (PM_IMFFT(pm))
	case FFT:
	    return (PM_FFT(pm))
	case CONV:
	    return (PM_CONV(pm))
	case ASFFT:
	    return (PM_ASFFT(pm))
	default:
	    call error (0, "RG_PSTATP: Unknown pointer parameter.")
	}
end


# RG_PSTATR -- Fetch the value of a psfmath task real parameter.

real procedure rg_pstatr (pm, param)

pointer	pm		# pointer to psfmatch structure
int	param		# parameter to be fetched

begin
	switch (param) {
	case BVALUER:
	    return (PM_BVALUER(pm))
	case BVALUE:
	    return (PM_BVALUE(pm))
	case APODIZE:
	    return (PM_APODIZE(pm))
	case LOREJECT:
	    return (PM_LOREJECT(pm))
	case HIREJECT:
	    return (PM_HIREJECT(pm))
	case UFLUXRATIO:
	    return (PM_UFLUXRATIO(pm))
	case FLUXRATIO:
	    return (PM_FLUXRATIO(pm))
	case SXINNER:
	    return (PM_SXINNER(pm))
	case SXOUTER:
	    return (PM_SXOUTER(pm))
	case SYINNER:
	    return (PM_SYINNER(pm))
	case SYOUTER:
	    return (PM_SYOUTER(pm))
	case THRESHOLD:
	    return (PM_THRESHOLD(pm))
	case NORMFACTOR:
	    return (PM_NORMFACTOR(pm))
	default:
	    call error (0, "RG_PSTATR: Unknown real parameter.")
	}
end


# RG_PSTATS -- Fetch the value of a psfmatch string string parameter.

procedure rg_pstats (pm, param, str, maxch)

pointer	pm		# pointer to psfmatch structure
int	param		# parameter to be fetched
char	str[ARB]	# output string
int	maxch		# maximum number of characters

begin
	switch (param) {
	case BSTRING:
	    call strcpy (PM_BSTRING(pm), str, maxch)
	case CSTRING:
	    call strcpy (PM_CSTRING(pm), str, maxch)
	case FSTRING:
	    call strcpy (PM_FSTRING(pm), str, maxch)
	case IMAGE:
	    call strcpy (PM_IMAGE(pm), str, maxch)
	case REFIMAGE:
	    call strcpy (PM_REFIMAGE(pm), str, maxch)
	case PSFDATA:
	    call strcpy (PM_PSFDATA(pm), str, maxch)
	case PSFIMAGE:
	    call strcpy (PM_PSFIMAGE(pm), str, maxch)
	case OBJLIST:
	    call strcpy (PM_OBJLIST(pm), str, maxch)
	case KERNEL:
	    call strcpy (PM_KERNEL(pm), str, maxch)
	case OUTIMAGE:
	    call strcpy (PM_OUTIMAGE(pm), str, maxch)
	default:
	    call error (0, "RG_PSTATS: Unknown string parameter.")
	}
end


# RG_PSETI -- Set the value of a psfmatch task integer parameter.

procedure rg_pseti (pm, param, value)

pointer	pm		# pointer to psfmatch structure
int	param		# parameter to be fetched
int	value		# value of the integer parameter

begin
	switch (param) {
	case NREGIONS:
	    PM_NREGIONS(pm) = value
	case CNREGION:
	    PM_CNREGION(pm) = value
	case CENTER:
	    PM_CENTER(pm) = value
	case BACKGRD:
	    PM_BACKGRD(pm) = value
	    switch (value) {
	    case PM_BNONE:
		call strcpy ("none", PM_BSTRING(pm), SZ_FNAME)
	    case PM_BMEAN:
		call strcpy ("mean", PM_BSTRING(pm), SZ_FNAME)
	    case PM_BMEDIAN:
		call strcpy ("median", PM_BSTRING(pm), SZ_FNAME)
	    case PM_BSLOPE:
		call strcpy ("plane", PM_BSTRING(pm), SZ_FNAME)
	    case PM_BNUMBER:
		;
	    default:
		call strcpy ("none", PM_BSTRING(pm), SZ_FNAME)
	    }
	case CONVOLUTION:
	    PM_CONVOLUTION(pm) = value
	    switch (value) {
	    case PM_CONIMAGE:
		call strcpy ("image", PM_CSTRING(pm), SZ_FNAME)
	    case PM_CONPSF:
		call strcpy ("psf", PM_CSTRING(pm), SZ_FNAME)
	    case PM_CONKERNEL:
		call strcpy ("kernel", PM_CSTRING(pm), SZ_FNAME)
	    default:
		call strcpy ("image", PM_CSTRING(pm), SZ_FNAME)
	    }
	case DNX:
	    PM_DNX(pm) = value
	case DNY:
	    PM_DNY(pm) = value
	case PNX:
	    PM_PNX(pm) = value
	case PNY:
	    PM_PNY(pm) = value
	case KNX:
	    PM_KNX(pm) = value
	case KNY:
	    PM_KNY(pm) = value
	case POWER:
	    PM_POWER(pm) = value
	case RADSYM:
	    PM_RADSYM(pm) = value
	case NXFFT:
	    PM_NXFFT(pm) = value
	case NYFFT:
	    PM_NYFFT(pm) = value
	case FILTER:
	    PM_FILTER(pm) = value
	    switch (value) {
	    case PM_FNONE:
		call strcpy ("none", PM_FSTRING(pm), SZ_FNAME)
	    case PM_FCOSBELL:
		call strcpy ("cosbell", PM_FSTRING(pm), SZ_FNAME)
	    case PM_FREPLACE:
		call strcpy ("replace", PM_FSTRING(pm), SZ_FNAME)
	    case PM_FMODEL:
		call strcpy ("model", PM_FSTRING(pm), SZ_FNAME)
	    default:
		call strcpy ("none", PM_FSTRING(pm), SZ_FNAME)
	    }
	default:
	    call error (0, "RG_PSETI: Unknown integer parameter.")
	}
end


# RG_PSETP -- Set the value of a psfmatch task  pointer parameter.

procedure rg_psetp (pm, param, value)

pointer	pm		# pointer to psfmatch structure
int	param		# parameter to be fetched
pointer value		# value of the pointer parameter

begin
	switch (param) {
	case RC1:
	    PM_RC1(pm) = value
	case RC2:
	    PM_RC2(pm) = value
	case RL1:
	    PM_RL1(pm) = value
	case RL2:
	    PM_RL2(pm) = value
	case RZERO:
	    PM_RZERO(pm) = value
	case RXSLOPE:
	    PM_RXSLOPE(pm) = value
	case RYSLOPE:
	    PM_RYSLOPE(pm) = value
	case REFFFT:
	    PM_REFFFT(pm) = value
	case IMFFT:
	    PM_IMFFT(pm) = value
	case FFT:
	    PM_FFT(pm) = value
	case CONV:
	    PM_CONV(pm) = value
	case ASFFT:
	    PM_ASFFT(pm) = value

	default:
	    call error (0, "RG_PSETP: Unknown pointer parameter.")
	}
end


# RG_PSETR -- Set the value of a psfmatch task real parameter.

procedure rg_psetr (pm, param, value)

pointer	pm		# pointer to psfmatch structure
int	param		# parameter to be fetched
real	value		# real parameter

begin
	switch (param) {
	case BVALUER:
	    PM_BVALUER(pm) = value
	case BVALUE:
	    PM_BVALUE(pm) = value
	case LOREJECT:
	    PM_LOREJECT(pm) = value
	case HIREJECT:
	    PM_HIREJECT(pm) = value
	case APODIZE:
	    PM_APODIZE(pm) = value
	case UFLUXRATIO:
	    PM_UFLUXRATIO(pm) = value
	case FLUXRATIO:
	    PM_FLUXRATIO(pm) = value
	case SXINNER:
	    PM_SXINNER(pm) = value
	case SXOUTER:
	    PM_SXOUTER(pm) = value
	case SYINNER:
	    PM_SYINNER(pm) = value
	case SYOUTER:
	    PM_SYOUTER(pm) = value
	case THRESHOLD:
	    PM_THRESHOLD(pm) = value
	case NORMFACTOR:
	    PM_NORMFACTOR(pm) = value
	default:
	    call error (0, "RG_PSETR: Unknown real parameter.")
	}
end


# RG_PSETS -- Procedure to set the value of a string parameter.

procedure rg_psets (pm, param, str)

pointer	pm		# pointer to psfmatch structure
int	param		# parameter to be fetched
char	str[ARB]	# output string

int	index, ip
pointer	sp, temp
real	rval
int	strdic(), fnldir(), ctor()

begin
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)

	switch (param) {
	case BSTRING:
	    ip = 1
 	    index = strdic (str, str, SZ_LINE, PM_BTYPES)
            if (index > 0) {
                call strcpy (str, PM_BSTRING(pm), SZ_FNAME)
                call rg_pseti (pm, BACKGRD, index)
            } else if (ctor (str, ip, rval) > 0) {
		call rg_psetr (pm, BVALUE, rval)
		if (ctor (str, ip, rval) > 0) {
		    call rg_psetr (pm, BVALUER, rval)
                    call strcpy (str, PM_BSTRING(pm), SZ_FNAME)
                    call rg_pseti (pm, BACKGRD, PM_NUMBER)
		} else {
		    call rg_psetr (pm, BVALUE, 0.0)
		    call rg_psetr (pm, BVALUER, 0.0)
		}
	    }
	case CSTRING:
 	    index = strdic (str, str, SZ_LINE, PM_CTYPES)
            if (index > 0) {
                call strcpy (str, PM_CSTRING(pm), SZ_FNAME)
                call rg_pseti (pm, CONVOLUTION, index)
            }
	case FSTRING:
 	    index = strdic (str, str, SZ_LINE, PM_FTYPES)
            if (index > 0) {
                call strcpy (str, PM_FSTRING(pm), SZ_FNAME)
                call rg_pseti (pm, FILTER, index)
            }
	case IMAGE:
	    call imgcluster (str, Memc[temp], SZ_FNAME)
	    index = fnldir (Memc[temp], PM_IMAGE(pm), SZ_FNAME)
	    call strcpy (Memc[temp+index], PM_IMAGE(pm), SZ_FNAME)
	case REFIMAGE:
	    call imgcluster (str, Memc[temp], SZ_FNAME)
	    index = fnldir (Memc[temp], PM_REFIMAGE(pm), SZ_FNAME)
	    call strcpy (Memc[temp+index], PM_REFIMAGE(pm), SZ_FNAME)
	case PSFDATA:
	    call strcpy (str, PM_PSFDATA(pm), SZ_FNAME)
	case PSFIMAGE:
	    call imgcluster (str, Memc[temp], SZ_FNAME)
	    index = fnldir (Memc[temp], PM_PSFIMAGE(pm), SZ_FNAME)
	    call strcpy (Memc[temp+index], PM_PSFIMAGE(pm), SZ_FNAME)
	case OBJLIST:
	    call strcpy (str, PM_OBJLIST(pm), SZ_FNAME)
	case KERNEL:
	    call imgcluster (str, Memc[temp], SZ_FNAME)
	    index = fnldir (Memc[temp], PM_KERNEL(pm), SZ_FNAME)
	    call strcpy (Memc[temp+index], PM_KERNEL(pm), SZ_FNAME)
	case OUTIMAGE:
	    call strcpy (str, PM_OUTIMAGE(pm), SZ_FNAME)
	default:
	    call error (0, "RG_PSETS: Unknown string parameter.")
	}

	call sfree (sp)
end
