# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# T_RGCURSOR -- Copy the graphics cursor list to the standard output.  This
# task was originally written as a cl script. 

procedure t_rgcursor 

pointer sp, buf, gp
pointer	gopen()
int	clglstr()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	iferr (gp = gopen ("stdgraph", APPEND, STDGRAPH))
	    gp = NULL

	while (clglstr ("gcur", Memc[buf], SZ_LINE) != EOF) {
	    call printf ("%s\n")
		call pargstr (Memc[buf])
	}

	if (gp != NULL)
	    call gclose (gp)
	call sfree (sp)
end
