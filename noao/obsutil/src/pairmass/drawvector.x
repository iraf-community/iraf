# DRAW_VECTOR -- Draw the projected vector to the screen.

include <gset.h>
include	<mach.h>

procedure draw_vector (def_title, timesys, xvec, yvec, n,
	xmin, xmax, ymin, ymax)

char	def_title[ARB]				#I default plot title
char	timesys[ARB]				#I time system
real	xvec[n], yvec[n]			#I vectors to plot
int	n					#I npts in vectors
real	xmin, xmax				#I x vector min & max
real	ymin, ymax				#I y vector min & max

pointer	sp, gp
pointer	device, marker, xlabel, ylabel, title, suffix
real	wx1, wx2, wy1, wy2, vx1, vx2, vy1, vy2, szm, tol
int	mode, imark
bool	pointmode

pointer	gopen()
real	clgetr()
bool	clgetb(), streq()
int	btoi(), clgeti()

begin
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (marker, SZ_FNAME, TY_CHAR)
	call salloc (xlabel, SZ_LINE,  TY_CHAR)
	call salloc (ylabel, SZ_LINE,  TY_CHAR)
	call salloc (title,  SZ_LINE,  TY_CHAR)
	call salloc (suffix, SZ_FNAME, TY_CHAR)

	call clgstr ("device", Memc[device], SZ_FNAME)
	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	gp = gopen (Memc[device], mode, STDGRAPH)
	tol = 10. * EPSILONR

	if (mode != APPEND) {
	    # Establish window.
	    wx1 = clgetr ("wx1")
	    wx2 = clgetr ("wx2")
	    wy1 = clgetr ("wy1")
	    wy2 = clgetr ("wy2")

	    # Set window limits to defaults if not specified by user.
	    if ((wx2 - wx1) < tol) {
	        wx1 = xmin
	        wx2 = xmax
	    }

	    if ((wy2 - wy1) < tol) {
	        wy1 = ymin
	        wy2 = ymax
	    }

	    call gswind (gp, wx1, wx2, wy1, wy2)
    
	    # Establish viewport.
	    vx1 = clgetr ("vx1")
	    vx2 = clgetr ("vx2")
	    vy1 = clgetr ("vy1")
	    vy2 = clgetr ("vy2")

	    # Set viewport only if specified by user.
	    if ((vx2 - vx1) > tol && (vy2 - vy1) > tol)
	        call gsview (gp, vx1, vx2, vy1, vy2)
	    else {
		if (!clgetb ("fill"))
		    call gseti (gp, G_ASPECT, 1)
	    }
   
            call clgstr ("xlabel", Memc[xlabel], SZ_LINE)
            call clgstr ("ylabel", Memc[ylabel], SZ_LINE)
            call clgstr ("title",  Memc[title],  SZ_LINE)

            if (streq (Memc[title], "default"))
                call strcpy (def_title, Memc[title], SZ_LINE)
            if (streq (Memc[xlabel], "default")) {
                call sprintf (Memc[xlabel], SZ_LINE, "%s Time")
		    call pargstr (timesys)
	    }
    
	    call gseti (gp, G_XNMAJOR, clgeti ("majrx"))
	    call gseti (gp, G_XNMINOR, clgeti ("minrx"))
	    call gseti (gp, G_YNMAJOR, clgeti ("majry"))
	    call gseti (gp, G_YNMINOR, clgeti ("minry"))

	    call gseti (gp, G_ROUND, btoi (clgetb ("round")))

	    if (clgetb ("logx"))
	        call gseti (gp, G_XTRAN, GW_LOG)
	    if (clgetb ("logy"))
	        call gseti (gp, G_YTRAN, GW_LOG)

	    # Draw axes using all this information.
	    call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])
	}
    
	pointmode = clgetb ("pointmode")
        if (pointmode) {
            call clgstr ("marker", Memc[marker], SZ_FNAME)
            szm = clgetr ("szmarker")
            call init_marker (Memc[marker], imark)
        }

        # Now to actually draw the plot.
        if (pointmode)
            call gpmark (gp, xvec, yvec, n, imark, szm, szm)
        else
            call gpline (gp, xvec, yvec, n)
       
	call gflush (gp)
        call gclose (gp)
	call sfree (sp)
end
