# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"imexam.h"

define	MTYPES	"|point|box|plus|cross|circle|hebar|vebar|hline|vline|diamond|"
define	IE_GBUF		0.10	# Buffer around data
define	IE_SZTITLE	512	# Size of multiline title
 

# IE_GRAPH -- Make a graph
# This procedure is used by most of the different graph types to provide
# consistency in features and parameters.  The parameters are read using
# the pset pointer.

procedure ie_graph (gp, mode, pp, param, x, y, npts, label, format)

pointer	gp		# GIO pointer
int	mode		# Mode
pointer	pp		# PSET pointer
char	param[ARB]	# Parameter string
real	x[npts]		# X data
real	y[npts]		# Y data
int	npts		# Number of points
char	label		# Default x label
char	format		# Default x format

int	i, marks[10], linepattern, patterns[4], clgpseti(), btoi(), strdic()
pointer	sp, title, xlabel, ylabel
real	x1, x2, y1, y2, wx1, wx2, wy1, wy2, temp, szmarker
real	clgpsetr(), ie_iformatr()
bool	clgpsetb(), streq()

data	patterns/GL_SOLID, GL_DASHED, GL_DOTTED, GL_DOTDASH/
data	marks/GM_POINT, GM_BOX, GM_PLUS, GM_CROSS, GM_CIRCLE, GM_HEBAR,
	GM_VEBAR, GM_HLINE, GM_VLINE, GM_DIAMOND/

begin
	call smark (sp)
	call salloc (xlabel, SZ_LINE, TY_CHAR)

	# If a new graph setup all the axes and labeling options and then
	# make the graph.

	if (mode == NEW_FILE) {
	    call gclear (gp)

	    linepattern = 0

	    x1 = ie_iformatr (clgpsetr (pp, "x1"), format)
	    x2 = ie_iformatr (clgpsetr (pp, "x2"), format)
	    y1 = clgpsetr (pp, "y1")
	    y2 = clgpsetr (pp, "y2")

	    if (IS_INDEF (x1) || IS_INDEF (x2))
	        call gascale (gp, x, npts, 1)
	    if (IS_INDEF (y1) || IS_INDEF (y2))
	        call gascale (gp, y, npts, 2)

	    call gswind (gp, x1, x2, y1, y2)
	    call ggwind (gp, wx1, wx2, wy1, wy2)

	    temp = wx2 - wx1
	    if (IS_INDEF (x1))
	        wx1 = wx1 - IE_GBUF * temp
	    if (IS_INDEF (x2))
	        wx2 = wx2 + IE_GBUF * temp

	    temp = wy2 - wy1
	    if (IS_INDEF (y1))
	        wy1 = wy1 - IE_GBUF * temp
	    if (IS_INDEF (y2))
	        wy2 = wy2 + IE_GBUF * temp

	    call gswind (gp, wx1, wx2, wy1, wy2)
	    call gsetr (gp, G_ASPECT, 0.)
	    call gseti (gp, G_ROUND, btoi (clgpsetb (pp, "round")))

	    i = GW_LINEAR
	    if (clgpsetb (pp, "logx"))
		i = GW_LOG
	    call gseti (gp, G_XTRAN, i)
	    i = GW_LINEAR
	    if (clgpsetb (pp, "logy"))
		i = GW_LOG
	    call gseti (gp, G_YTRAN, i)

	    if (clgpsetb (pp, "box")) {
	        # Get number of major and minor tick marks.
	        call gseti (gp, G_XNMAJOR, clgpseti (pp, "majrx"))
	        call gseti (gp, G_XNMINOR, clgpseti (pp, "minrx"))
	        call gseti (gp, G_YNMAJOR, clgpseti (pp, "majry"))
	        call gseti (gp, G_YNMINOR, clgpseti (pp, "minry"))

	        # Label tick marks on axes?
		call gsets (gp, G_XTICKFORMAT, format)
	        call gseti (gp, G_LABELTICKS,
		    btoi (clgpsetb (pp, "ticklabels")))

	        # Fetch labels and  plot title string. 
		call salloc (title, IE_SZTITLE, TY_CHAR)
		call salloc (ylabel, SZ_LINE, TY_CHAR)

		if (clgpsetb (pp, "banner")) {
		    call sysid (Memc[title], IE_SZTITLE)
		    call strcat ("\n", Memc[title], IE_SZTITLE)
		    call strcat (param, Memc[title], IE_SZTITLE)
		} else
		    Memc[title] = EOS

		call clgpset (pp, "title", Memc[xlabel], SZ_LINE)
		if (Memc[xlabel] != EOS) {
		    call strcat ("\n", Memc[title], IE_SZTITLE)
		    call strcat (Memc[xlabel], Memc[title], IE_SZTITLE)
		}
		call clgpset (pp, "xlabel", Memc[xlabel], SZ_LINE)
		call clgpset (pp, "ylabel", Memc[ylabel], SZ_LINE)

		if (streq ("wcslabel", Memc[xlabel]))
		    call strcpy (label, Memc[xlabel], SZ_LINE)

	        call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])
	    }
	}

	# Draw the data.
	if (clgpsetb (pp, "pointmode")) {
	    call clgpset (pp, "marker", Memc[xlabel], SZ_LINE)
	    i = strdic (Memc[xlabel], Memc[xlabel], SZ_LINE, MTYPES)
	    if (i == 0)
		i = 2
	    if (marks[i] == GM_POINT)
		szmarker = 0.0
	    else
		szmarker = clgpsetr (pp, "szmarker")
	    call gpmark (gp, x, y, npts, marks[i], szmarker, szmarker)
	} else {
	    linepattern = min (4, linepattern + 1)
	    call gseti (gp, G_PLTYPE, patterns[linepattern])
	    call gpline (gp, x, y, npts)
	}
	call gflush (gp)

	call sfree (sp)
end
