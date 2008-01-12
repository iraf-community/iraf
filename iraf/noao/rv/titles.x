include	<gset.h>
include "rvpackage.h"
include "rvflags.h"

# GET_PLOT_TITLE - Create a title string for the plot

procedure get_plot_title (rv, title, npts)

pointer	rv					#I RV struct pointer
pointer	title					#I Preallocated pointer to str
int	npts					#I Npts in data

pointer system_id, sp

begin
	# Do the silly title string
	if (RV_WHERE(rv) == BOTTOM && RV_DCFLAG(rv) == -1) {
	    call strcpy ("Pixel Correlation - No velocities will be computed.",
		Memc[title], SZ_LINE)

	} else if (RV_WHERE(rv) == TOP) {
	    call smark (sp)
	    call salloc (system_id, SZ_LINE, TY_CHAR)

	    call sysid (Memc[system_id], SZ_LINE)

	    call sprintf (Memc[title], 4*SZ_LINE,
    "%s\nObject='%s' Reference='%s'\nStar='%s' Temp='%s' npts=%d aperture=%d")
	         call pargstr (Memc[system_id])
	         call pargstr (IMAGE(rv))
	         call pargstr (RIMAGE(rv))
		 call pargstr (OBJNAME(rv))
		 call pargstr (TEMPNAME(rv))
	         call pargi (npts)
		 call pargi (RV_APNUM(rv))
	    call sfree (sp)

	} else						# No title string
	    call strcpy ("", Memc[title], SZ_LINE)
end


# NPLOT_TITLE - Create a plot title for the normalized data

procedure nplot_title (rv, pltype, where, title)

pointer	rv				#I RV struct pointer
int	pltype				#I Type of spectrum being drawn
int	where				#I Where will plot be drawn
char 	title[ARB]			#O title string created

pointer	sp, system_id, buf

begin
	call smark (sp)
	call salloc (system_id, SZ_LINE, TY_CHAR)
	call salloc (buf, 4*SZ_LINE, TY_CHAR)

	if (where == TOP) {
	    call sysid (Memc[system_id], SZ_LINE) 	# Do the sys title
	    call strcpy (Memc[system_id], title, SZ_LINE)
	    call strcat ("\n", title, SZ_LINE)
	} else
	    call strcpy ("", title, SZ_LINE)

	if (pltype == OBJECT_SPECTRUM) {
	    call sprintf (Memc[buf], 2*SZ_LINE, 
		"Normalization of '%s' Aperture # = %d")
	         call pargstr (IMAGE(rv))
		 call pargi (RV_APNUM(rv))
	} else if (pltype == REFER_SPECTRUM) {
	    call sprintf (Memc[buf], 2*SZ_LINE, 
		"Normalization of '%s' Template # = %d")
	         call pargstr (RIMAGE(rv))
		 call pargi (RV_TEMPNUM(rv))
	}
	call strcat (Memc[buf], title, 3*SZ_LINE)

	call sfree (sp)
end


# GET_ANPLOT_TITLE - Get the title string for an antisymmetric noise plot

procedure get_anplot_title (rv, title)

pointer	rv					#I RV struct pointer
pointer	title					#U Pre-allocated title string

pointer system_id, sp

begin
	call smark (sp)
	call salloc (system_id, SZ_LINE, TY_CHAR)

	# Do the silly title string
	call sysid (Memc[system_id], SZ_LINE)

	call sprintf (Memc[title], 3*SZ_LINE,
"%s\nObject='%s' Temp='%s' npts=%d aperture=%d\nStar = '%s' Template = '%s'")
	         call pargstr (Memc[system_id])
		 call pargstr (IMAGE(rv))
		 call pargstr (RIMAGE(rv))
	         call pargi (RV_CCFNPTS(rv))
		 call pargi (RV_APNUM(rv))
		 call pargstr (OBJNAME(rv))
		 call pargstr (TEMPNAME(rv))

	call sfree (sp)
end 
