include "psfmatch.h"

# RG_PGPARS -- Read in the psf matching algorithm parameters.

procedure rg_pgpars (pm)

pointer	pm		#I pointer to psfmatch structure

int	ival
pointer	sp, str
bool	clgetb()
int	clgwrd(), clgeti(), btoi()
real	clgetr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Initialize the psf matching structure.
	call rg_pinit (pm, clgwrd ("convolution", Memc[str], SZ_LINE,
	    PM_CTYPES))

	# Define the data and kernel sizes.
	ival = clgeti ("dnx")
	if (mod (ival, 2) == 0)
	    ival = ival + 1
	call rg_pseti (pm, DNX, ival)
	ival = clgeti ("dny")
	if (mod (ival, 2) == 0)
	    ival = ival + 1
	call rg_pseti (pm, DNY, ival)
	ival = clgeti ("pnx")
	if (mod (ival, 2) == 0)
	    ival = ival + 1
	call rg_pseti (pm, PNX, ival)
	ival = clgeti ("pny")
	if (mod (ival, 2) == 0)
	    ival = ival + 1
	call rg_pseti (pm, PNY, ival)

	# Centering parameters.
	call rg_pseti (pm, CENTER, btoi (clgetb ("center")))

	# Background value computation.
	call clgstr ("background", Memc[str], SZ_LINE)
	call rg_psets (pm, BSTRING, Memc[str])
	call rg_psetr (pm, LOREJECT, clgetr ("loreject"))
	call rg_psetr (pm, HIREJECT, clgetr ("hireject"))
	call rg_psetr (pm, APODIZE, clgetr ("apodize"))

	# Filtering parameters.
	call rg_psetr (pm, UFLUXRATIO, clgetr ("fluxratio"))
	call clgstr ("filter", Memc[str], SZ_LINE)
	call rg_psets (pm, FSTRING, Memc[str])
	call rg_psetr (pm, SXINNER, clgetr ("sx1"))
	call rg_psetr (pm, SXOUTER, clgetr ("sx2"))
	call rg_psetr (pm, SYINNER, clgetr ("sy1"))
	call rg_psetr (pm, SYOUTER, clgetr ("sy2"))
	call rg_pseti (pm, RADSYM, btoi (clgetb ("radsym")))
	call rg_psetr (pm, THRESHOLD, (clgetr ("threshold")))

	# Normalization parameter.
	call rg_psetr (pm, NORMFACTOR, clgetr ("normfactor"))

	#call rg_psetr (pm, PRATIO, clgetr ("pratio"))

	call sfree (sp)
end


# RG_PPPARS -- Put the parameters required for the psf matching from
# the cl to the parameter file.

procedure rg_pppars (pm)

pointer	pm		#I pointer to the psf matching structure

pointer	sp, str
bool	itob()
int	rg_pstati()
real	rg_pstatr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Store the psf data string.
	call rg_pstats (pm, PSFDATA, Memc[str], SZ_LINE)
	call clpstr ("psf", Memc[str])

	# Store the size parameters.
	call clputi ("dnx", rg_pstati (pm, DNX))
	call clputi ("dny", rg_pstati (pm, DNY))
	call clputi ("pnx", rg_pstati (pm, PNX))
	call clputi ("pny", rg_pstati (pm, PNY))

	# Store the centering parameters.
	call clputb ("center", itob (rg_pstati (pm, CENTER)))

	# Store the background fitting parameters.
	call rg_pstats (pm, BSTRING, Memc[str], SZ_LINE)
	call clpstr ("background", Memc[str])
	call clputr ("loreject", rg_pstatr (pm, LOREJECT))
	call clputr ("hireject", rg_pstatr (pm, HIREJECT))
	call clputr ("apodize", rg_pstatr (pm, APODIZE))

	# Store the filtering parameters.
	call clputr ("fluxratio", rg_pstatr(pm, UFLUXRATIO))
	call rg_pstats (pm, FSTRING, Memc[str], SZ_LINE)
	call clpstr ("filter", Memc[str])
	call clputr ("sx1", rg_pstatr (pm, SXINNER))
	call clputr ("sx2", rg_pstatr (pm, SXOUTER))
	call clputr ("sy1", rg_pstatr (pm, SYINNER))
	call clputr ("sy2", rg_pstatr (pm, SYOUTER))
	call clputb ("radsym", itob (rg_pstati (pm, RADSYM)))
	call clputr ("threshold", rg_pstatr (pm, THRESHOLD))

	# Store the normalization parameters.
	call clputr ("normfactor", rg_pstatr (pm, NORMFACTOR))

	call sfree (sp)
end
