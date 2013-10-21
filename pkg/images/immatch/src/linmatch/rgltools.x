include "linmatch.h"

#  RG_LINIT -- Initialize the linscale structure. 

procedure rg_linit (ls, max_nregions)

pointer	ls		#I/O pointer to the intensity scaling structure
int	max_nregions	#I the maximum number of regions

begin
	# Allocate the temporary space.
	call malloc (ls, LEN_LSSTRUCT, TY_STRUCT)

	# Set up the regions parameters.
	LS_NREGIONS(ls) = 0
	LS_CNREGION(ls) = 1
	LS_MAXNREGIONS(ls) = max_nregions
		
	# Initialize the pointers.
	LS_RC1(ls) = NULL
	LS_RC2(ls) = NULL
	LS_RL1(ls) = NULL
	LS_RL2(ls) = NULL
	LS_RXSTEP(ls) = NULL
	LS_RYSTEP(ls) = NULL
	LS_XSHIFT(ls) = 0.0
	LS_YSHIFT(ls) = 0.0
	LS_SXSHIFT(ls) = 0.0
	LS_SYSHIFT(ls) = 0.0

	LS_RBUF(ls) = NULL
	LS_RGAIN(ls) = 1.0
	LS_RREADNOISE(ls) = 0.0
	LS_RMEAN(ls) = NULL
	LS_RMEDIAN(ls) = NULL
	LS_RMODE(ls) = NULL
	LS_RSIGMA(ls) = NULL
	LS_RSKY(ls) = NULL
	LS_RSKYERR(ls) = NULL
	LS_RMAG(ls) = NULL
	LS_RMAGERR(ls) = NULL
	LS_RNPTS(ls) = NULL

	LS_IBUF(ls) = NULL
	LS_IGAIN(ls) = 1.0
	LS_IREADNOISE(ls) = 0.0
	LS_IMEAN(ls) = NULL
	LS_IMEDIAN(ls) = NULL
	LS_IMODE(ls) = NULL
	LS_ISIGMA(ls) = NULL
	LS_ISKY(ls) = NULL
	LS_ISKYERR(ls) = NULL
	LS_IMAG(ls) = NULL
	LS_IMAGERR(ls) = NULL
	LS_INPTS(ls) = NULL

	LS_RBSCALE(ls) = NULL
	LS_RBSCALEERR(ls) = NULL
	LS_RBZERO(ls) = NULL
	LS_RBZEROERR(ls) = NULL
	LS_RDELETE(ls) = NULL
	LS_RCHI(ls) = NULL

	# Initialize the scaling algorithm parameters.
	LS_BZALGORITHM(ls) = DEF_BZALGORITHM
	LS_BSALGORITHM(ls) = DEF_BSALGORITHM
	LS_CBZERO(ls) = DEF_CBZERO
	LS_CBSCALE(ls) = DEF_CBSCALE
	LS_DNX(ls) = DEF_DNX
	LS_DNY(ls) = DEF_DNY
	LS_MAXITER(ls) = DEF_MAXITER
	LS_DATAMIN(ls) = DEF_DATAMIN
	LS_DATAMAX(ls) = DEF_DATAMAX
	LS_NREJECT(ls) = DEF_NREJECT
	LS_LOREJECT(ls) = DEF_LOREJECT
	LS_HIREJECT(ls) = DEF_HIREJECT
	LS_GAIN(ls) = DEF_GAIN
	LS_READNOISE(ls) = DEF_READNOISE

	# Initialize the answers
	LS_TBZERO(ls) = 0.0
	LS_TBZEROERR(ls) = INDEFR
	LS_TBSCALE(ls) = 1.0
	LS_TBSCALEERR(ls) = INDEFR

	# Initialize the strings.
	call strcpy ("mean", LS_BSSTRING(ls), SZ_FNAME)
	call strcpy ("mean", LS_BZSTRING(ls), SZ_FNAME)
	LS_CCDGAIN(ls) = EOS
	LS_CCDREAD(ls) = EOS
	LS_IMAGE(ls) = EOS
	LS_REFIMAGE(ls) = EOS
	LS_REGIONS(ls) = EOS
	LS_DATABASE(ls) = EOS
	LS_OUTIMAGE(ls) = EOS
	LS_RECORD(ls) = EOS
	LS_SHIFTSFILE(ls) = EOS
	LS_PHOTFILE(ls) = EOS

	# Initialize the buffers.
	call rg_lrinit (ls)
end


#  RG_LRINIT -- Initialize the region dependent part of the linscale structure. 

procedure rg_lrinit (ls)

pointer	ls		#I pointer to the intensity scaling structure

begin
	# Free up previously defined region pointers.
	call rg_lrfree (ls)

	# Allocate region definition pointers.
	call malloc (LS_RC1(ls), LS_MAXNREGIONS(ls), TY_INT)
	call malloc (LS_RC2(ls), LS_MAXNREGIONS(ls), TY_INT)
	call malloc (LS_RL1(ls), LS_MAXNREGIONS(ls), TY_INT)
	call malloc (LS_RL2(ls), LS_MAXNREGIONS(ls), TY_INT)
	call malloc (LS_RXSTEP(ls), LS_MAXNREGIONS(ls), TY_INT)
	call malloc (LS_RYSTEP(ls), LS_MAXNREGIONS(ls), TY_INT)

	# Allocate region statistics pointers.
	call malloc (LS_RMEAN(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RMEDIAN(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RMODE(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RSIGMA(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RSKY(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RSKYERR(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RMAG(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RMAGERR(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RNPTS(ls), LS_MAXNREGIONS(ls), TY_INT)

	call malloc (LS_IMEAN(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_IMEDIAN(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_IMODE(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_ISIGMA(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_ISKY(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_ISKYERR(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_IMAG(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_IMAGERR(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_INPTS(ls), LS_MAXNREGIONS(ls), TY_INT)

	call malloc (LS_RBSCALE(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RBSCALEERR(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RBZERO(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RBZEROERR(ls), LS_MAXNREGIONS(ls), TY_REAL)
	call malloc (LS_RDELETE(ls), LS_MAXNREGIONS(ls), TY_INT)
	call malloc (LS_RCHI(ls), LS_MAXNREGIONS(ls), TY_REAL)

	# Initialize region definitions.
	call amovki (INDEFI, Memi[LS_RC1(ls)], LS_MAXNREGIONS(ls))
	call amovki (INDEFI, Memi[LS_RC2(ls)], LS_MAXNREGIONS(ls))
	call amovki (INDEFI, Memi[LS_RL1(ls)], LS_MAXNREGIONS(ls))
	call amovki (INDEFI, Memi[LS_RL2(ls)], LS_MAXNREGIONS(ls))
	call amovki (INDEFI, Memi[LS_RXSTEP(ls)], LS_MAXNREGIONS(ls))
	call amovki (INDEFI, Memi[LS_RYSTEP(ls)], LS_MAXNREGIONS(ls))

	# Initilaize the statistics.
	call amovkr (INDEFR, Memr[LS_RMEAN(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RMEDIAN(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RMODE(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RSIGMA(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RSKY(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RSKYERR(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RMAG(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RMAGERR(ls)], LS_MAXNREGIONS(ls))
	call amovki (INDEFI, Memi[LS_RNPTS(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_IMEAN(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_IMEDIAN(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_IMODE(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_ISIGMA(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_ISKY(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_ISKYERR(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_IMAG(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_IMAGERR(ls)], LS_MAXNREGIONS(ls))
	call amovki (INDEFI, Memi[LS_INPTS(ls)], LS_MAXNREGIONS(ls))

	# Initialize the answers.
	call amovkr (INDEFR, Memr[LS_RBSCALE(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RBSCALEERR(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RBZERO(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RBZEROERR(ls)], LS_MAXNREGIONS(ls))
	call amovki (LS_NO, Memi[LS_RDELETE(ls)], LS_MAXNREGIONS(ls))
	call amovkr (INDEFR, Memr[LS_RCHI(ls)], LS_MAXNREGIONS(ls))
end


# RG_LINDEFR -- Re-initialize the regions dependent buffers.

procedure rg_lindefr (ls)

pointer	ls		#I pointer to the intensity scaling structure

int	nregions
int	rg_lstati()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions > 0) {

	    # Reinitialize the region definition pointers.
	    call amovki (INDEFI, Memi[LS_RC1(ls)], nregions)
	    call amovki (INDEFI, Memi[LS_RC2(ls)], nregions)
	    call amovki (INDEFI, Memi[LS_RL1(ls)], nregions)
	    call amovki (INDEFI, Memi[LS_RL2(ls)], nregions)
	    call amovki (INDEFI, Memi[LS_RXSTEP(ls)], nregions)
	    call amovki (INDEFI, Memi[LS_RYSTEP(ls)], nregions)

	    # Reinitialize the statistics pointers.
	    call amovkr (INDEFR, Memr[LS_RMEAN(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RMEDIAN(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RMODE(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RSIGMA(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RSKY(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RSKYERR(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RMAG(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RMAGERR(ls)], nregions)
	    call amovki (INDEFI, Memi[LS_RNPTS(ls)], nregions)

	    call amovkr (INDEFR, Memr[LS_IMEAN(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_IMEDIAN(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_IMODE(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_ISIGMA(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_ISKY(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_ISKYERR(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_IMAG(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_IMAGERR(ls)], nregions)
	    call amovki (INDEFI, Memi[LS_INPTS(ls)], nregions)

	    # Reinitialize the answers pointers.
	    call amovkr (INDEFR, Memr[LS_RBSCALE(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RBSCALEERR(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RBZERO(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RBZEROERR(ls)], nregions)
	    call amovki (LS_NO, Memi[LS_RDELETE(ls)], nregions)
	    call amovkr (INDEFR, Memr[LS_RCHI(ls)], nregions)

	}
end


#  RG_LREALLOC -- Reallocate the regions dependent buffers.

procedure rg_lrealloc (ls, nregions)

pointer	ls		#I pointer to the intensity scaling structure
int	nregions	#I the number of regions

int	nr
int	rg_lstati()

begin
	nr = rg_lstati (ls, NREGIONS)

	# Resize the region definition buffers.
	call realloc (LS_RC1(ls), nregions, TY_INT)
	call realloc (LS_RC2(ls), nregions, TY_INT)
	call realloc (LS_RL1(ls), nregions, TY_INT)
	call realloc (LS_RL2(ls), nregions, TY_INT)
	call realloc (LS_RXSTEP(ls), nregions, TY_INT)
	call realloc (LS_RYSTEP(ls), nregions, TY_INT)

	# Resize the statistics buffers.
	call realloc (LS_RMEAN(ls), nregions, TY_REAL)
	call realloc (LS_RMEDIAN(ls), nregions, TY_REAL)
	call realloc (LS_RMODE(ls), nregions, TY_REAL)
	call realloc (LS_RSIGMA(ls), nregions, TY_REAL)
	call realloc (LS_RSKY(ls), nregions, TY_REAL)
	call realloc (LS_RSKYERR(ls), nregions, TY_REAL)
	call realloc (LS_RMAG(ls), nregions, TY_REAL)
	call realloc (LS_RMAGERR(ls), nregions, TY_REAL)
	call realloc (LS_RNPTS(ls), nregions, TY_INT)

	call realloc (LS_IMEAN(ls), nregions, TY_REAL)
	call realloc (LS_IMEDIAN(ls), nregions, TY_REAL)
	call realloc (LS_IMODE(ls), nregions, TY_REAL)
	call realloc (LS_ISIGMA(ls), nregions, TY_REAL)
	call realloc (LS_ISKY(ls), nregions, TY_REAL)
	call realloc (LS_ISKYERR(ls), nregions, TY_REAL)
	call realloc (LS_IMAG(ls), nregions, TY_REAL)
	call realloc (LS_IMAGERR(ls), nregions, TY_REAL)
	call realloc (LS_INPTS(ls), nregions, TY_INT)

	# Resize the answers buffers.
	call realloc (LS_RBSCALE(ls), nregions, TY_REAL)
	call realloc (LS_RBSCALEERR(ls), nregions, TY_REAL)
	call realloc (LS_RBZERO(ls), nregions, TY_REAL)
	call realloc (LS_RBZEROERR(ls), nregions, TY_REAL)
	call realloc (LS_RDELETE(ls), nregions, TY_INT)
	call realloc (LS_RCHI(ls), nregions, TY_REAL)

	# Reinitialize the region defintions.
	call amovki (INDEFI, Memi[LS_RC1(ls)+nr], nregions - nr)
	call amovki (INDEFI, Memi[LS_RC2(ls)+nr], nregions - nr)
	call amovki (INDEFI, Memi[LS_RL1(ls)+nr], nregions - nr)
	call amovki (INDEFI, Memi[LS_RL2(ls)+nr], nregions - nr)
	call amovki (INDEFI, Memi[LS_RXSTEP(ls)+nr], nregions - nr)
	call amovki (INDEFI, Memi[LS_RYSTEP(ls)+nr], nregions - nr)

	# Reinitialize the statistics buffers.
	call amovkr (INDEFR, Memr[LS_RMEAN(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RMEDIAN(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RMODE(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RSIGMA(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RSKY(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RSKYERR(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RMAG(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RMAGERR(ls)+nr], nregions - nr)
	call amovki (INDEFI, Memi[LS_RNPTS(ls)+nr], nregions - nr)

	call amovkr (INDEFR, Memr[LS_IMEAN(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_IMEDIAN(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_IMODE(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_ISIGMA(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_ISKY(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_ISKYERR(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_IMAG(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_IMAGERR(ls)+nr], nregions - nr)
	call amovki (INDEFI, Memi[LS_INPTS(ls)+nr], nregions - nr)

	# Reinitialize the answers buffers.
	call amovkr (INDEFR, Memr[LS_RBSCALE(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RBSCALEERR(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RBZERO(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RBZEROERR(ls)+nr], nregions - nr)
	call amovki (LS_NO, Memi[LS_RDELETE(ls)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[LS_RCHI(ls)+nr], nregions - nr)
end


# RG_LRFREE -- Free the regions portion of the linscale structure.

procedure rg_lrfree (ls)

pointer	ls		#I pointer to the intensity scaling structure

begin
	LS_NREGIONS(ls) = 0

	# Free the regions definitions buffers.
	if (LS_RC1(ls) != NULL)
	    call mfree (LS_RC1(ls), TY_INT)
	LS_RC1(ls) = NULL
	if (LS_RC2(ls) != NULL)
	    call mfree (LS_RC2(ls), TY_INT)
	LS_RC2(ls) = NULL
	if (LS_RL1(ls) != NULL)
	    call mfree (LS_RL1(ls), TY_INT)
	LS_RL1(ls) = NULL
	if (LS_RL2(ls) != NULL)
	    call mfree (LS_RL2(ls), TY_INT)
	LS_RL2(ls) = NULL
	if (LS_RXSTEP(ls) != NULL)
	    call mfree (LS_RXSTEP(ls), TY_INT)
	LS_RXSTEP(ls) = NULL
	if (LS_RYSTEP(ls) != NULL)
	    call mfree (LS_RYSTEP(ls), TY_INT)
	LS_RYSTEP(ls) = NULL

	# Free the statistics buffers.
	if (LS_RBUF(ls) != NULL)
	    call mfree (LS_RBUF(ls), TY_REAL)
	if (LS_RMEAN(ls) != NULL)
	    call mfree (LS_RMEAN(ls), TY_REAL)
	LS_RMEAN(ls) = NULL
	if (LS_RMEDIAN(ls) != NULL)
	    call mfree (LS_RMEDIAN(ls), TY_REAL)
	LS_RMEDIAN(ls) = NULL
	if (LS_RMODE(ls) != NULL)
	    call mfree (LS_RMODE(ls), TY_REAL)
	LS_RMODE(ls) = NULL
	if (LS_RSIGMA(ls) != NULL)
	    call mfree (LS_RSIGMA(ls), TY_REAL)
	LS_RSIGMA(ls) = NULL
	if (LS_RSKY(ls) != NULL)
	    call mfree (LS_RSKY(ls), TY_REAL)
	LS_RSKY(ls) = NULL
	if (LS_RSKYERR(ls) != NULL)
	    call mfree (LS_RSKYERR(ls), TY_REAL)
	LS_RSKYERR(ls) = NULL
	if (LS_RMAG(ls) != NULL)
	    call mfree (LS_RMAG(ls), TY_REAL)
	LS_RMAG(ls) = NULL
	if (LS_RMAGERR(ls) != NULL)
	    call mfree (LS_RMAGERR(ls), TY_REAL)
	LS_RMAGERR(ls) = NULL
	if (LS_RNPTS(ls) != NULL)
	    call mfree (LS_RNPTS(ls), TY_INT)
	LS_RNPTS(ls) = NULL

	if (LS_IBUF(ls) != NULL)
	    call mfree (LS_IBUF(ls), TY_REAL)
	if (LS_IMEAN(ls) != NULL)
	    call mfree (LS_IMEAN(ls), TY_REAL)
	LS_IMEAN(ls) = NULL
	if (LS_IMEDIAN(ls) != NULL)
	    call mfree (LS_IMEDIAN(ls), TY_REAL)
	LS_IMEDIAN(ls) = NULL
	if (LS_IMODE(ls) != NULL)
	    call mfree (LS_IMODE(ls), TY_REAL)
	LS_IMODE(ls) = NULL
	if (LS_ISIGMA(ls) != NULL)
	    call mfree (LS_ISIGMA(ls), TY_REAL)
	LS_ISIGMA(ls) = NULL
	if (LS_ISKY(ls) != NULL)
	    call mfree (LS_ISKY(ls), TY_REAL)
	LS_ISKY(ls) = NULL
	if (LS_ISKYERR(ls) != NULL)
	    call mfree (LS_ISKYERR(ls), TY_REAL)
	LS_ISKYERR(ls) = NULL
	if (LS_IMAG(ls) != NULL)
	    call mfree (LS_IMAG(ls), TY_REAL)
	LS_IMAG(ls) = NULL
	if (LS_IMAGERR(ls) != NULL)
	    call mfree (LS_IMAGERR(ls), TY_REAL)
	LS_IMAGERR(ls) = NULL
	if (LS_INPTS(ls) != NULL)
	    call mfree (LS_INPTS(ls), TY_INT)
	LS_INPTS(ls) = NULL

	# Free the answers buffers.
	if (LS_RBSCALE(ls) != NULL)
	    call mfree (LS_RBSCALE(ls), TY_REAL)
	LS_RBSCALE(ls) = NULL
	if (LS_RBSCALEERR(ls) != NULL)
	    call mfree (LS_RBSCALEERR(ls), TY_REAL)
	LS_RBSCALEERR(ls) = NULL
	if (LS_RBZERO(ls) != NULL)
	    call mfree (LS_RBZERO(ls), TY_REAL)
	LS_RBZERO(ls) = NULL
	if (LS_RBZEROERR(ls) != NULL)
	    call mfree (LS_RBZEROERR(ls), TY_REAL)
	LS_RBZEROERR(ls) = NULL
	if (LS_RDELETE(ls) != NULL)
	    call mfree (LS_RDELETE(ls), TY_INT)
	LS_RDELETE(ls) = NULL
	if (LS_RCHI(ls) != NULL)
	    call mfree (LS_RCHI(ls), TY_REAL)
	LS_RCHI(ls) = NULL
end


# RG_LFREE -- Free the linscale structure.

procedure rg_lfree (ls)

pointer	ls		#I/O pointer to the intensity scaling structure

begin
	# Free the regions dependent pointers.
	call rg_lrfree (ls)

	call mfree (ls, TY_STRUCT)
end


# RG_LSTATI -- Fetch the value of an integer parameter.

int procedure rg_lstati (ls, param)

pointer	ls		#I pointer to the intensity scaling structure
int	param		#I parameter to be fetched

begin
	switch (param) {
	case CNREGION:
	    return (LS_CNREGION(ls))
	case NREGIONS:
	    return (LS_NREGIONS(ls))
	case MAXNREGIONS:
	    return (LS_MAXNREGIONS(ls))
	case BZALGORITHM:
	    return (LS_BZALGORITHM(ls))
	case BSALGORITHM:
	    return (LS_BSALGORITHM(ls))
	case DNX:
	    return (LS_DNX(ls))
	case DNY:
	    return (LS_DNY(ls))
	case MAXITER:
	    return (LS_MAXITER(ls))
	case NREJECT:
	    return (LS_NREJECT(ls))
	default:
	    call error (0, "RG_LSTATI: Unknown integer parameter.")
	}
end


# RG_LSTATP -- Fetch the value of a pointer parameter.

pointer procedure rg_lstatp (ls, param)

pointer	ls		#I pointer to the intensity scaling structure
int	param		#I parameter to be fetched

begin
	switch (param) {

	case RC1:
	    return (LS_RC1(ls))
	case RC2:
	    return (LS_RC2(ls))
	case RL1:
	    return (LS_RL1(ls))
	case RL2:
	    return (LS_RL2(ls))
	case RXSTEP:
	    return (LS_RXSTEP(ls))
	case RYSTEP:
	    return (LS_RYSTEP(ls))

	case RBUF:
	    return (LS_RBUF(ls))
	case RMEAN:
	    return (LS_RMEAN(ls))
	case RMEDIAN:
	    return (LS_RMEDIAN(ls))
	case RMODE:
	    return (LS_RMODE(ls))
	case RSIGMA:
	    return (LS_RSIGMA(ls))
	case RSKY:
	    return (LS_RSKY(ls))
	case RSKYERR:
	    return (LS_RSKYERR(ls))
	case RMAG:
	    return (LS_RMAG(ls))
	case RMAGERR:
	    return (LS_RMAGERR(ls))
	case RNPTS:
	    return (LS_RNPTS(ls))

	case IBUF:
	    return (LS_IBUF(ls))
	case IMEAN:
	    return (LS_IMEAN(ls))
	case IMEDIAN:
	    return (LS_IMEDIAN(ls))
	case IMODE:
	    return (LS_IMODE(ls))
	case ISIGMA:
	    return (LS_ISIGMA(ls))
	case ISKY:
	    return (LS_ISKY(ls))
	case ISKYERR:
	    return (LS_ISKYERR(ls))
	case IMAG:
	    return (LS_IMAG(ls))
	case IMAGERR:
	    return (LS_IMAGERR(ls))
	case INPTS:
	    return (LS_INPTS(ls))

	case RBSCALE:
	    return (LS_RBSCALE(ls))
	case RBSCALEERR:
	    return (LS_RBSCALEERR(ls))
	case RBZERO:
	    return (LS_RBZERO(ls))
	case RBZEROERR:
	    return (LS_RBZEROERR(ls))
	case RDELETE:
	    return (LS_RDELETE(ls))
	case RCHI:
	    return (LS_RCHI(ls))

	default:
	    call error (0, "RG_LSTATP: Unknown pointer parameter.")
	}
end


# RG_LSTATR -- Fetch the value of a real parameter.

real procedure rg_lstatr (ls, param)

pointer	ls		#I pointer to the intensity scaling structure
int	param		#I parameter to be fetched

begin
	switch (param) {

	case XSHIFT:
	    return (LS_XSHIFT(ls))
	case YSHIFT:
	    return (LS_YSHIFT(ls))
	case SXSHIFT:
	    return (LS_SXSHIFT(ls))
	case SYSHIFT:
	    return (LS_SYSHIFT(ls))

	case CBZERO:
	    return (LS_CBZERO(ls))
	case CBSCALE:
	    return (LS_CBSCALE(ls))
	case DATAMIN:
	    return (LS_DATAMIN(ls))
	case DATAMAX:
	    return (LS_DATAMAX(ls))
	case LOREJECT:
	    return (LS_LOREJECT(ls))
	case HIREJECT:
	    return (LS_HIREJECT(ls))
	case GAIN:
	    return (LS_GAIN(ls))
	case RGAIN:
	    return (LS_RGAIN(ls))
	case IGAIN:
	    return (LS_IGAIN(ls))
	case READNOISE:
	    return (LS_READNOISE(ls))
	case RREADNOISE:
	    return (LS_RREADNOISE(ls))
	case IREADNOISE:
	    return (LS_IREADNOISE(ls))

	case TBZERO:
	    return (LS_TBZERO(ls))
	case TBZEROERR:
	    return (LS_TBZEROERR(ls))
	case TBSCALE:
	    return (LS_TBSCALE(ls))
	case TBSCALEERR:
	    return (LS_TBSCALEERR(ls))

	default:
	    call error (0, "RG_LSTATR: Unknown real parameter.")
	}
end


# RG_LSTATS -- Fetch the value of a string parameter.

procedure rg_lstats (ls, param, str, maxch)

pointer	ls		#I pointer to the intensity scaling structure
int	param		#I parameter to be fetched
char	str[ARB]	#I the output string
int	maxch		#I maximum number of characters

begin
	switch (param) {
	case BZSTRING:
	    call strcpy (LS_BZSTRING(ls), str, maxch)
	case BSSTRING:
	    call strcpy (LS_BSSTRING(ls), str, maxch)
	case CCDGAIN:
	    call strcpy (LS_CCDGAIN(ls), str, maxch)
	case CCDREAD:
	    call strcpy (LS_CCDREAD(ls), str, maxch)
	case IMAGE:
	    call strcpy (LS_IMAGE(ls), str, maxch)
	case REFIMAGE:
	    call strcpy (LS_REFIMAGE(ls), str, maxch)
	case REGIONS:
	    call strcpy (LS_REGIONS(ls), str, maxch)
	case DATABASE:
	    call strcpy (LS_DATABASE(ls), str, maxch)
	case OUTIMAGE:
	    call strcpy (LS_OUTIMAGE(ls), str, maxch)
	case SHIFTSFILE:
	    call strcpy (LS_SHIFTSFILE(ls), str, maxch)
	case PHOTFILE:
	    call strcpy (LS_PHOTFILE(ls), str, maxch)
	case RECORD:
	    call strcpy (LS_RECORD(ls), str, maxch)
	default:
	    call error (0, "RG_LSTATS: Unknown string parameter.")
	}
end


# RG_LSETI -- Set the value of an integer parameter.

procedure rg_lseti (ls, param, value)

pointer	ls		# pointer to the intensity scaling structure
int	param		# parameter to be fetched
int	value		# value of the integer parameter

begin
	switch (param) {

	case NREGIONS:
	    LS_NREGIONS(ls) = value
	case CNREGION:
	    LS_CNREGION(ls) = value
	case MAXNREGIONS:
	    LS_MAXNREGIONS(ls) = value

	case BZALGORITHM:
	    LS_BZALGORITHM(ls) = value
	    switch (value) {
	    case LS_MEAN:
		call strcpy ("mean", LS_BZSTRING(ls), SZ_FNAME)
	    case LS_MEDIAN:
		call strcpy ("median", LS_BZSTRING(ls), SZ_FNAME)
	    case LS_MODE:
		call strcpy ("mode", LS_BZSTRING(ls), SZ_FNAME)
	    case LS_FIT:
		call strcpy ("fit", LS_BZSTRING(ls), SZ_FNAME)
	    case LS_PHOTOMETRY:
		call strcpy ("photometry", LS_BZSTRING(ls), SZ_FNAME)
	    case LS_NUMBER:
		;
	    case LS_FILE:
		call strcpy ("file", LS_BZSTRING(ls), SZ_FNAME)
	        LS_BSALGORITHM(ls) = value
		call strcpy ("file", LS_BSSTRING(ls), SZ_FNAME)
	    default:
		LS_BZALGORITHM(ls) = LS_NUMBER
		call strcpy ("0.0", LS_BZSTRING(ls), SZ_FNAME)
		LS_CBZERO(ls) = 0.0
	    }

	case BSALGORITHM:
	    LS_BSALGORITHM(ls) = value
	    switch (value) {
	    case LS_MEAN:
		call strcpy ("mean", LS_BSSTRING(ls), SZ_FNAME)
	    case LS_MEDIAN:
		call strcpy ("median", LS_BSSTRING(ls), SZ_FNAME)
	    case LS_MODE:
		call strcpy ("mode", LS_BSSTRING(ls), SZ_FNAME)
	    case LS_FIT:
		call strcpy ("fit", LS_BSSTRING(ls), SZ_FNAME)
	    case LS_PHOTOMETRY:
		call strcpy ("photometry", LS_BSSTRING(ls), SZ_FNAME)
	    case LS_NUMBER:
		;
	    case LS_FILE:
		call strcpy ("file", LS_BSSTRING(ls), SZ_FNAME)
	        LS_BZALGORITHM(ls) = value
		call strcpy ("file", LS_BZSTRING(ls), SZ_FNAME)
	    default:
		LS_BSALGORITHM(ls) = LS_NUMBER
		call strcpy ("1.0", LS_BSSTRING(ls), SZ_FNAME)
		LS_CBSCALE(ls) = 1.0
	    }

	case DNX:
	    LS_DNX(ls) = value
	case DNY:
	    LS_DNY(ls) = value
	case MAXITER:
	    LS_MAXITER(ls) = value
	case NREJECT:
	    LS_NREJECT(ls) = value

	default:
	    call error (0, "RG_LSETI: Unknown integer parameter.")
	}
end


# RG_LSETP -- Set the value of a pointer parameter.

procedure rg_lsetp (ls, param, value)

pointer	ls		#I pointer to the linscale structure
int	param		#I parameter to be fetched
pointer value		#I value of the pointer parameter

begin
	switch (param) {

	case RC1:
	    LS_RC1(ls) = value
	case RC2:
	    LS_RC2(ls) = value
	case RL1:
	    LS_RL1(ls) = value
	case RL2:
	    LS_RL2(ls) = value
	case RXSTEP:
	    LS_RXSTEP(ls) = value
	case RYSTEP:
	    LS_RYSTEP(ls) = value

	case RBUF:
	    LS_RBUF(ls) = value
	case RMEAN:
	    LS_RMEAN(ls) = value
	case RMEDIAN:
	    LS_RMEDIAN(ls) = value
	case RMODE:
	    LS_RMODE(ls) = value
	case RSIGMA:
	    LS_RSIGMA(ls) = value
	case RSKY:
	    LS_RSKY(ls) = value
	case RSKYERR:
	    LS_RSKYERR(ls) = value
	case RMAG:
	    LS_RMAG(ls) = value
	case RMAGERR:
	    LS_RMAGERR(ls) = value
	case RNPTS:
	    LS_RNPTS(ls) = value

	case IBUF:
	    LS_IBUF(ls) = value
	case IMEAN:
	    LS_IMEAN(ls) = value
	case IMEDIAN:
	    LS_IMEDIAN(ls) = value
	case IMODE:
	    LS_IMODE(ls) = value
	case ISIGMA:
	    LS_ISIGMA(ls) = value
	case ISKY:
	    LS_ISKY(ls) = value
	case ISKYERR:
	    LS_ISKYERR(ls) = value
	case IMAG:
	    LS_IMAG(ls) = value
	case IMAGERR:
	    LS_IMAGERR(ls) = value
	case INPTS:
	    LS_INPTS(ls) = value

	case RBSCALE:
	    LS_RBSCALE(ls) = value
	case RBSCALEERR:
	    LS_RBSCALEERR(ls) = value
	case RBZERO:
	    LS_RBZERO(ls) = value
	case RBZEROERR:
	    LS_RBZEROERR(ls) = value
	case RDELETE:
	    LS_RDELETE(ls) = value
	case RCHI:
	    LS_RCHI(ls) = value

	default:
	    call error (0, "RG_LSETP: Unknown pointer parameter.")
	}
end


# RG_LSETR -- Set the value of a real parameter.

procedure rg_lsetr (ls, param, value)

pointer	ls		#I pointer to iscale structure
int	param		#I parameter to be fetched
real	value		#I real parameter

begin
	switch (param) {
	case XSHIFT:
	    LS_XSHIFT(ls) = value
	case YSHIFT:
	    LS_YSHIFT(ls) = value
	case SXSHIFT:
	    LS_SXSHIFT(ls) = value
	case SYSHIFT:
	    LS_SYSHIFT(ls) = value
	case CBZERO:
	    LS_CBZERO(ls) = value
	case CBSCALE:
	    LS_CBSCALE(ls) = value
	case DATAMIN:
	    LS_DATAMIN(ls) = value
	case DATAMAX:
	    LS_DATAMAX(ls) = value
	case LOREJECT:
	    LS_LOREJECT(ls) = value
	case HIREJECT:
	    LS_HIREJECT(ls) = value
	case GAIN:
	    LS_GAIN(ls) = value
	case RGAIN:
	    LS_RGAIN(ls) = value
	case IGAIN:
	    LS_IGAIN(ls) = value
	case READNOISE:
	    LS_READNOISE(ls) = value
	case RREADNOISE:
	    LS_RREADNOISE(ls) = value
	case IREADNOISE:
	    LS_IREADNOISE(ls) = value
	case TBSCALE:
	    LS_TBSCALE(ls) = value
	case TBSCALEERR:
	    LS_TBSCALEERR(ls) = value
	case TBZERO:
	    LS_TBZERO(ls) = value
	case TBZEROERR:
	    LS_TBZEROERR(ls) = value
	default:
	    call error (0, "RG_LSETR: Unknown real parameter.")
	}
end


# RG_LSETS -- Set the value of a string parameter.

procedure rg_lsets (ls, param, str)

pointer	ls		# pointer to the intensity scaling structure
int	param		# parameter to be fetched
char	str[ARB]	# output string

int	index, ip
pointer	sp, temp
real	rval
int	fnldir(), strdic(), ctor(), rg_lstati()

begin
	call smark (sp)
	call salloc (temp, SZ_LINE, TY_CHAR)

	switch (param) {

	case BZSTRING:
	    ip = 1
	    index = strdic (str, str, SZ_LINE, LS_SCALING)
	    if (index > 0) {
		if (rg_lstati (ls, BSALGORITHM) == LS_NUMBER) {
		    call strcpy (str, LS_BZSTRING(ls), SZ_FNAME)
		    call rg_lseti (ls, BZALGORITHM, index)
		} else {
		    call strcpy (LS_BSSTRING(ls), LS_BZSTRING(ls), SZ_FNAME)
		    call rg_lseti (ls, BZALGORITHM, rg_lstati (ls, BSALGORITHM))
		}
	    } else if (ctor (str, ip, rval) > 0) {
		call strcpy (str, LS_BZSTRING(ls), SZ_FNAME)
		call rg_lsetr (ls, CBZERO, rval) 
		call rg_lseti (ls, BZALGORITHM, LS_NUMBER)
	    } else {
		call strcpy ("0.0", LS_BZSTRING(ls), SZ_FNAME)
		call rg_lsetr (ls, CBZERO, 0.0) 
		call rg_lseti (ls, BZALGORITHM, LS_NUMBER)
	    }
	case BSSTRING:
	    ip = 1
	    index = strdic (str, str, SZ_LINE, LS_SCALING)
	    if (index > 0) {
		call strcpy (str, LS_BSSTRING(ls), SZ_FNAME)
		call rg_lseti (ls, BSALGORITHM, index)
	    } else if (ctor (str, ip, rval) > 0) {
		call strcpy (str, LS_BSSTRING(ls), SZ_FNAME)
		call rg_lsetr (ls, CBSCALE, rval) 
		call rg_lseti (ls, BSALGORITHM, LS_NUMBER)
	    } else {
		call strcpy ("1.0", LS_BSSTRING(ls), SZ_FNAME)
		call rg_lsetr (ls, CBSCALE, 1.0) 
		call rg_lseti (ls, BSALGORITHM, LS_NUMBER)
	    }
	case CCDGAIN:
	    ip = 1
	    if (ctor (str, ip, rval) > 0) {
		call strcpy (str, LS_CCDGAIN(ls), SZ_FNAME)
		call rg_lsetr (ls, RGAIN, rval)
	        if (ctor (str, ip, rval) > 0)
		    call rg_lsetr (ls, IGAIN, rval)
		else
		    call rg_lsetr (ls, IGAIN, 1.0)
		call rg_lsetr (ls, GAIN, INDEFR)
	    } else {
		call sscan (str)
		    call gargwrd (Memc[temp], SZ_LINE)
		call strcpy (Memc[temp], LS_CCDGAIN(ls), SZ_FNAME)
		call rg_lsetr (ls, RGAIN, 1.0)
		call rg_lsetr (ls, IGAIN, 1.0)
		call rg_lsetr (ls, GAIN, INDEFR)
	    }
	case CCDREAD:
	    ip = 1
	    if (ctor (str, ip, rval) > 0) {
		call strcpy (str, LS_CCDREAD(ls), SZ_FNAME)
		call rg_lsetr (ls, RREADNOISE, rval)
	        if (ctor (str, ip, rval) > 0)
		    call rg_lsetr (ls, IREADNOISE, rval)
		else
		    call rg_lsetr (ls, IREADNOISE, 0.0)
		call rg_lsetr (ls, READNOISE, INDEFR)
	    } else {
		call sscan (str)
		    call gargwrd (Memc[temp], SZ_LINE)
		call strcpy (Memc[temp], LS_CCDREAD(ls), SZ_FNAME)
		call rg_lsetr (ls, RREADNOISE, 0.0)
		call rg_lsetr (ls, IREADNOISE, 0.0)
		call rg_lsetr (ls, READNOISE, INDEFR)
	    }

	case IMAGE:
	    call imgcluster (str, Memc[temp], SZ_FNAME)
	    index = fnldir (Memc[temp], LS_IMAGE(ls), SZ_FNAME)
	    call strcpy (Memc[temp+index], LS_IMAGE(ls), SZ_FNAME)
	case REFIMAGE:
	    call imgcluster (str, Memc[temp], SZ_FNAME)
	    index = fnldir (Memc[temp], LS_REFIMAGE(ls), SZ_FNAME)
	    call strcpy (Memc[temp+index], LS_REFIMAGE(ls), SZ_FNAME)
	case REGIONS:
	    call strcpy (str, LS_REGIONS(ls), SZ_FNAME)
	case DATABASE:
	    index = fnldir (str, LS_DATABASE(ls), SZ_FNAME)
	    call strcpy (str[index+1], LS_DATABASE(ls), SZ_FNAME)
	case OUTIMAGE:
	    call strcpy (str, LS_OUTIMAGE(ls), SZ_FNAME)
	case SHIFTSFILE:
	    call strcpy (str, LS_SHIFTSFILE(ls), SZ_FNAME)
	case PHOTFILE:
	    call strcpy (str, LS_PHOTFILE(ls), SZ_FNAME)
	case RECORD:
	    call strcpy (str, LS_RECORD(ls), SZ_FNAME)

	default:
	    call error (0, "RG_LSETS: Unknown string parameter.")
	}

	call sfree (sp)
end
