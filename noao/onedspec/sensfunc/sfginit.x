include	<gset.h>
include	"sensfunc.h"

# SF_GINIT -- Initialize graphics structure and open the graphics device.
# This includes CL requests for the starting graphs (default is "sr"),
# the mark types (default is "plus box"), and graphics device.

procedure sf_ginit (gp)

pointer	gp		# Graphics structure (returned)

int	i, j
pointer	sp, str, gopen()
errchk	malloc, gopen

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call calloc (gp, LEN_GP, TY_STRUCT)
	do i = 1, SF_NGRAPHS {
	    call malloc (GP_IMAGES(gp,i), SZ_FNAME, TY_CHAR)
	    Memc[GP_IMAGES(gp,i)] = EOS
	    call malloc (GP_SKYS(gp,i), SZ_FNAME, TY_CHAR)
	    Memc[GP_SKYS(gp,i)] = EOS
	}

	# Set the starting graph types.
	call clgstr ("graphs", Memc[str], SZ_FNAME)
	j = str
	for (i=str; Memc[i] != EOS; i=i+1) {
	    switch (Memc[i]) {
	    case 'a','c','e','i','l','r','s':
		Memc[j] = Memc[i]
		j = j + 1
	    }
	}
	Memc[j] = EOS
	if (Memc[str] != EOS)
	    call strcpy (Memc[str], GP_GRAPHS(gp,1), SF_NGRAPHS)
	else
	    call strcpy ("sr", GP_GRAPHS(gp,1), SF_NGRAPHS)

	# Set the starting mark types and colors.
	GP_MARK(gp) = GM_PLUS
	GP_MDEL(gp) = GM_CROSS
	GP_MADD(gp) = GM_BOX
	GP_PLCOLOR(gp) = 2
	GP_CMARK(gp) = 1
	GP_CDEL(gp) = 3
	GP_CADD(gp) = 4
	call clgstr ("marks", Memc[str], SZ_FNAME)
	call sf_marks (gp, Memc[str])
	call clgstr ("colors", Memc[str], SZ_FNAME)
	call sf_colors (gp, Memc[str])

	# Set flux limits
	GP_FMIN(gp) = INDEF
	GP_FMAX(gp) = INDEF

	# Open the graphics device.
	call clgstr ("device", Memc[str], SZ_FNAME)
	GP_GIO(gp) = gopen (Memc[str], NEW_FILE, STDGRAPH)

	call sfree (sp)
end


# SF_GFREE -- Free the graphics structure and close the graphics device.

procedure sf_gfree (gp)

pointer	gp		# Graphics structure

int	i

begin
	if (gp == NULL)
	    return

	call gclose (GP_GIO(gp))
	do i = 1, SF_NGRAPHS {
	    call mfree (GP_IMAGES(gp,i), TY_CHAR)
	    call mfree (GP_SKYS(gp,i), TY_CHAR)
	    if (GP_SHDR(gp,i) != NULL)
		call shdr_close (GP_SHDR(gp,i))
	}
	call mfree (gp, TY_STRUCT)
end
