include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<math/nlfit.h>
include	"starfocus.h"


# STF_MEASURE -- PSF measuring routine.
# This is a stand-alone routine that can be called to return the FWHM.
# It is a greatly abbreviated version of starfocus.

procedure stf_measure (im, xc, yc, beta, level, radius, nit,
	sbuffer, swidth, saturation, gp, logfd,
	bkg, renclosed, dfwhm, gfwhm, mfwhm)

pointer	im		#I Image pointer
real	xc		#I Initial X center
real	yc		#I Initial Y center
real	beta		#I Moffat beta
real	level		#I Measurement level
real	radius		#U Profile radius
int	nit		#I Number of iterations on radius
real	sbuffer		#I Sky buffer (pixels)
real	swidth		#I Sky width (pixels)
real	saturation	#I Saturation
pointer	gp		#I Graphics output if not NULL
int	logfd		#I Log output if not NULL
real	bkg		#O Background used
real	renclosed	#O Enclosed flux radius
real	dfwhm		#O Direct FWHM
real	gfwhm		#O Gaussian FWHM
real	mfwhm		#O Moffat FWHM

int	i
bool	ignore_sat
pointer	sp, str, sf, sfd, sfds

int	strdic()
real	stf_r2i()
errchk	stf_find, stf_bkgd, stf_profile, stf_widths, stf_fwhms, stf_organize

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (sf, SF, TY_STRUCT)
	call salloc (sfd, SFD, TY_STRUCT)
	call salloc (sfds, 1, TY_POINTER)
	call aclri (Memi[sf], SF)
	call aclri (Memi[sfd], SFD)
	Memi[sfds] = sfd

	# Initialize parameters.
	SF_TASK(sf) = PSFMEASURE
	SF_WCODE(sf) = strdic ("FWHM", SF_WTYPE(sf), SF_SZWTYPE, SF_WTYPES)
	SF_SCALE(sf) = 1.
	SF_LEVEL(sf) = level
	SF_BETA(sf) = beta
	SF_RADIUS(sf) = radius
	SF_SBUF(sf) = sbuffer
	SF_SWIDTH(sf) = swidth
	SF_SAT(sf) = saturation
	SF_NIT(sf) = nit
	SF_OVRPLT(sf) = NO
	SF_NCOLS(sf) = IM_LEN(im,1)
	SF_NLINES(sf) = IM_LEN(im,2)
	SF_XF(sf) = (IM_LEN(im,1) + 1) / 2.
	SF_YF(sf) = (IM_LEN(im,2) + 1) / 2.
	ignore_sat = false

	call imstats (im, IM_IMAGENAME, SFD_IMAGE(sfd), SF_SZFNAME)
	SFD_ID(sfd) = 1
	SFD_X(sfd) = xc
	SFD_Y(sfd) = yc
	SFD_F(sfd) = INDEF
	SFD_STATUS(sfd) = 0
	SFD_SFS(sfd) = NULL
	SFD_SFF(sfd) = NULL
	SFD_SFI(sfd) = NULL

	if (SF_LEVEL(sf) > 1.)
	    SF_LEVEL(sf) = SF_LEVEL(sf) / 100.
	SF_LEVEL(sf) = max (0.05, min (0.95, SF_LEVEL(sf)))

	# Evaluate PSF data.
	iferr {
	    do i = 1, SF_NIT(sf) {
		if (i == 1)
		    SFD_RADIUS(sfd) = SF_RADIUS(sf)
		else
		    SFD_RADIUS(sfd) = 3. * SFD_DFWHM(sfd)
		SFD_NPMAX(sfd) = stf_r2i (SFD_RADIUS(sfd)) + 1
		SFD_NP(sfd) = SFD_NPMAX(sfd)
		call stf_find (sf, sfd, im)
		call stf_bkgd (sf, sfd)
		if (SFD_NSAT(sfd) > 0 && i == 1) {
		    if (ignore_sat)
			call error (0,
			"Saturated pixels found - ignoring object")
		    else
			call eprintf (
			    "WARNING: Saturated pixels found.\n")
		}
		call stf_profile (sf, sfd)
		call stf_widths (sf, sfd)
		call stf_fwhms (sf, sfd)
	    }

	    # Set output results.
	    radius = SFD_RADIUS(sfd)
	    bkg = SFD_BKGD(sfd)
	    renclosed = SFD_R(sfd)
	    dfwhm = SFD_DFWHM(sfd)
	    mfwhm = SFD_MFWHM(sfd)
	    gfwhm = SFD_GFWHM(sfd)

	    # Optional graph and log output.  Note that the gp pointer is only
	    # used to indicate whether to make a graph.  The stf_graph
	    # procedure opens its own graphics stream.

	    call stf_organize (sf, sfds, 1)
	    if (gp != NULL)
		call stf_graph (sf)
	    if (logfd != NULL)
		call stf_log (sf, logfd)

	    call asifree (SFD_ASI1(sfd))
	    call asifree (SFD_ASI2(sfd))
	} then
	    call erract (EA_WARN)

	# Finish up
	call stf_free (sf)
	call sfree (sp)
end
