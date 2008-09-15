include	<gset.h>


# Any other type defaults to a connected line.
define  PLOT_TYPES      "|line|lhist|bhist|"
define	LINE		1		# Connected line
define  LHIST		2		# Line histogram
define  BHIST		3		# Box histogram

procedure hgpline (gp, x, y, n, type)

pointer	gp			#I GIO pointer
real	x[ARB]			#I X coordinates
real	y[ARB]			#I Y coordinates
int	n			#I Number of coordinates
char	type[ARB]		#I Plot type

int	i
real	x1, x2, bottom
char	line[5]

int	strdic()

begin
	# Draw lines.
	switch (strdic (type, line, 5, PLOT_TYPES)) {
	case LHIST:
	    x1 = (3 * x[1] - x[2]) / 2
	    call gamove (gp, x1, y[1])
	    do i = 1, n-1 {
		x2 = (x[i] + x[i+1]) / 2
		call gadraw (gp, x2, y[i])
		call gadraw (gp, x2, y[i+1])
		x1 = x2
	    }
	    x2 = (3 * x[n] - x[n-1]) / 2
	    call gadraw (gp, x2, y[n])
	case BHIST:
	    call ggwind (gp, x1, x2, bottom, x1)
	    x1 = (3 * x[1] - x[2]) / 2
	    call gamove (gp, x1, bottom)
	    call gadraw (gp, x1, y[1])
	    do i = 1, n-1 {
		x2 = (x[i] + x[i+1]) / 2
		call gadraw (gp, x2, y[i])
		call gadraw (gp, x2, bottom)
		call gadraw (gp, x2, y[i+1])
		x1 = x2
	    }
	    x2 = (3 * x[n] - x[n-1]) / 2
	    call gadraw (gp, x2, y[n])
	    call gadraw (gp, x2, bottom)
	default:
	    call gpline (gp, x, y, n)
	}
end
