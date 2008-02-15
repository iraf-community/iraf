include <lexnum.h>
include "linmatch.h"


# RG_GLPARS -- Fetch the algorithm parameters required by the intensity scaling
# task.

procedure rg_glpars (ls)

pointer ls              #I pointer to iscale structure

int	ip, nchars
pointer sp, str1, str2
int     clgeti(), nscan(), lexnum()
real    clgetr()

begin
        # Allocate working space.
        call smark (sp)
        call salloc (str1, SZ_LINE, TY_CHAR)
        call salloc (str2, SZ_LINE, TY_CHAR)

        # Initialize the linscale structure.
        call rg_linit (ls, clgeti ("maxnregions"))

	# Get the x and y shifts.
	call rg_lsetr (ls, XSHIFT, clgetr("xshift"))
	call rg_lsetr (ls, YSHIFT, clgetr("yshift"))

        # Get the scaling algorithm parameters.
        call clgstr ("scaling", Memc[str1], SZ_LINE)
	call sscan (Memc[str1])
	    call gargwrd (Memc[str1], SZ_LINE)
	    call gargwrd (Memc[str2], SZ_LINE)
        call rg_lsets (ls, BSSTRING, Memc[str1])
	ip = 1
	if (nscan() == 2)
            call rg_lsets (ls, BZSTRING, Memc[str2])
	else if (lexnum(Memc[str1], ip, nchars) == LEX_NONNUM)
            call rg_lsets (ls, BZSTRING, Memc[str1])
	else
            call rg_lsets (ls, BZSTRING, "0.0")

        call rg_lseti (ls, DNX, clgeti ("dnx"))
        call rg_lseti (ls, DNY, clgeti ("dny"))
        call rg_lseti (ls, MAXITER, clgeti ("maxiter"))
        call rg_lsetr (ls, DATAMIN, clgetr ("datamin"))
        call rg_lsetr (ls, DATAMAX, clgetr ("datamax"))
        call rg_lseti (ls, NREJECT, clgeti ("nreject"))
        call rg_lsetr (ls, LOREJECT, clgetr ("loreject"))
        call rg_lsetr (ls, HIREJECT, clgetr ("hireject"))

        call clgstr ("gain", Memc[str1], SZ_LINE)
	call rg_lsets (ls, CCDGAIN, Memc[str1])
        call clgstr ("readnoise", Memc[str1], SZ_LINE)
	call rg_lsets (ls, CCDREAD, Memc[str1])

        call sfree (sp)
end


# RG_PLPARS -- Save the intensity scaling parameters in the .par file.

procedure rg_plpars (ls)

pointer	ls		# pointer to the linscale structure

pointer	sp, str1, str2, str
int	rg_lstati()
real	rg_lstatr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set the x and y shifts parameters.
	call clputr ("xshift", rg_lstatr (ls, XSHIFT))
	call clputr ("yshift", rg_lstatr (ls, YSHIFT))

	# Scaling algorithm parameters.
	call rg_lstats (ls, BSSTRING, Memc[str1], SZ_LINE)
	call rg_lstats (ls, BZSTRING, Memc[str2], SZ_LINE)
	call sprintf (Memc[str], SZ_FNAME, "%s %s")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	call clpstr ("scaling", Memc[str])
	call clputi ("dnx", rg_lstati (ls, DNX))
	call clputi ("dny", rg_lstati (ls, DNY))
	call clputi ("maxiter", rg_lstati (ls, MAXITER))
	call clputr ("datamin", rg_lstatr (ls, DATAMIN))
	call clputr ("datamax", rg_lstatr (ls, DATAMAX))
	call clputi ("nreject", rg_lstati (ls, NREJECT))
	call clputr ("loreject", rg_lstatr (ls, LOREJECT))
	call clputr ("hireject", rg_lstatr (ls, HIREJECT))
	call rg_lstats (ls, CCDGAIN, Memc[str], SZ_FNAME)
	call clpstr ("gain", Memc[str])
	call rg_lstats (ls, CCDREAD, Memc[str], SZ_FNAME)
	call clpstr ("readnoise", Memc[str])

	call sfree (sp)
end
