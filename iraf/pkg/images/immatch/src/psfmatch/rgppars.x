include "psfmatch.h"

# RG_PGPARS -- Read in the psf matching algorithm parameters.

procedure rg_pgpars (pm)

pointer	pm		#I pointer to psfmatch structure

size_t	sz_val
long	lval, c_2
pointer	sp, str
bool	clgetb()
int	clgwrd(), btoi()
long	clgetl(), lmod()
real	clgetr()

begin
	c_2 = 2

	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)

	# Initialize the psf matching structure.
	call rg_pinit (pm, clgwrd ("convolution", Memc[str], SZ_LINE,
	    PM_CTYPES))

	# Define the data and kernel sizes.
	lval = clgetl ("dnx")
	if (lmod (lval, c_2) == 0)
	    lval = lval + 1
	call rg_psetl (pm, DNX, lval)
	lval = clgetl ("dny")
	if (lmod (lval, c_2) == 0)
	    lval = lval + 1
	call rg_psetl (pm, DNY, lval)
	lval = clgetl ("pnx")
	if (lmod (lval, c_2) == 0)
	    lval = lval + 1
	call rg_psetl (pm, PNX, lval)
	lval = clgetl ("pny")
	if (lmod (lval, c_2) == 0)
	    lval = lval + 1
	call rg_psetl (pm, PNY, lval)

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

size_t	sz_val
pointer	sp, str
bool	itob()
int	rg_pstati()
long	rg_pstatl()
real	rg_pstatr()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)

	# Store the psf data string.
	call rg_pstats (pm, PSFDATA, Memc[str], SZ_LINE)
	call clpstr ("psf", Memc[str])

	# Store the size parameters.
	call clputl ("dnx", rg_pstatl (pm, DNX))
	call clputl ("dny", rg_pstatl (pm, DNY))
	call clputl ("pnx", rg_pstatl (pm, PNX))
	call clputl ("pny", rg_pstatl (pm, PNY))

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
