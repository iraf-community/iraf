# NSTAR Strcuture

define	LEN_NSTARSTRUCT (30)

define	DP_GROUPFD	Memi[$1]	# fd of input group
define	DP_PSFFD	Memi[$1+1]	# fd of the PSF image
define	DP_NSTARFD	Memi[$1+2]	# fd of the NSTAR table

define	DP_GROUPNUM	Memi[$1+3]	# current group number 
define	DP_NUMBSTAR	Memi[$1+4]	# number of stars in the group
define	DP_NSTARCOLS	Memi[$1+5]	# pointer to NSTAR table columns

define	DP_NPIX		Memi[$1+14]	# pointer to number of pixels
define	DP_NUMER	Memi[$1+15]	# pointer to NUMER in curr group
define	DP_DENOM	Memi[$1+16]	# pointer to DENOM stat in curr group
define	DP_SKIP		Memi[$1+17]	# pointer to SKIP in curr group
define	DP_XCLAMP	Memi[$1+18]	# pointer to CLAMP stat in curr group
define	DP_XOLD		Memi[$1+19]	# pointer to XOLD stat in curr group
define  DP_X		Memi[$1+20]	# pointer to X array in curr group
define  DP_V		Memi[$1+21]	# pointer to V array in curr group
define  DP_SUMWT	Memi[$1+22]	# pointer to SUMWT array in curr group
define  DP_C	        Memi[$1+23]	# pointer to C array in curr group
define	DP_RPIXSQ	Memi[$1+24]	# pointer to RPIXSQ array in curr group
