# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imio.h>
include	"mwcs.h"

# MW_SHOW -- Print information about a MWCS object to a file.

procedure mw_show (mw, fd, what)

pointer	mw			#I pointer to MWCS descriptor
int	fd			#I output file
int	what			#I type of output (not used at present)

pointer	wp
int	ndim, nwcs, wcs, i, j
bool	itob()

begin
	ndim = MI_NDIM(mw)
	nwcs = MI_NWCS(mw)

	call fprintf (fd,
	    "MWCS=%x, ndim=%d, nwcs=%d, curwcs=%d(%s), refim=%s\n")
	    call pargi (mw)
	    call pargi (ndim)
	    call pargi (nwcs)

	    wcs = INDEFI
	    do i = 1, MI_NWCS(mw)
		if (MI_WCS(mw) == MI_WCSP(mw,i)) {
		    wcs = i
		    break
		}
	    call pargi (wcs)
	    if (MI_WCS(mw) != NULL) {
		wp = MI_WCS(mw)
		if (WCS_SYSTEM(wp) != NULL)
		    call pargstr (S(mw,WCS_SYSTEM(wp)))
		else
		    call pargstr ("noname")
	    }

	    if (MI_REFIM(mw) != NULL)
		call pargstr (IM_NAME(MI_REFIM(mw)))
	    else
		call pargstr ("none")

	call fprintf (fd, "sbuflen=%d, sbufused=%d, dbuflen=%d, dbufused=%d\n")
	    call pargi (MI_SBUFLEN(mw))
	    call pargi (MI_SBUFUSED(mw))
	    call pargi (MI_DBUFLEN(mw))
	    call pargi (MI_DBUFUSED(mw))

	# Print the axis map.
	call fprintf (fd, "useaxmap=%b, nlogdim=%d")
	    call pargb (itob(MI_USEAXMAP(mw)))
	    call pargi (MI_NLOGDIM(mw))
	    call fprintf (fd, " axno=[")
		do i = 1, ndim {
		    if (i > 1)
			call fprintf (fd, " ")
		    call fprintf (fd, "%d")
			call pargi (MI_AXNO(mw,i))
		}
	    call fprintf (fd, "] axval=[")
		do i = 1, ndim {
		    if (i > 1)
			call fprintf (fd, " ")
		    call fprintf (fd, "%d")
			call pargi (MI_AXVAL(mw,i))
		}
	    call fprintf (fd, "] physax=[")
		do i = 1, ndim {
		    if (i > 1)
			call fprintf (fd, " ")
		    call fprintf (fd, "%d")
			call pargi (MI_PHYSAX(mw,i))
		}
	    call fprintf (fd, "]\n")

	# Print the LTERM.
	call fprintf (fd, "ltv = [")
	    do i = 1, ndim {
		if (i > 1)
		    call fprintf (fd, " ")
		call fprintf (fd, "%g")
		    call pargd (D(mw,MI_LTV(mw)+i-1))
	    }
	    call fprintf (fd, "]\n")

	call fprintf (fd, "ltm = [")
	    do j = 1, ndim {
		if (j > 1)
		    call fprintf (fd, "; ")
		do i = 1, ndim {
		    if (i > 1)
			call fprintf (fd, " ")
		    call fprintf (fd, "%g")
			call pargd (D(mw,MI_LTM(mw)+(j-1)*ndim+i-1))
		}
	    }
	    call fprintf (fd, "]\n")

	# Print the world systems.
	do wcs = 1, nwcs {
	    wp = MI_WCSP(mw,wcs)
	    ndim = WCS_NDIM(wp)

	    call fprintf (fd,
		"WCS %d, ndim=%d, name=%s, nwattr=%d, nfunc=%d\n")
		call pargi (wcs)
		call pargi (ndim)
		if (WCS_SYSTEM(wp) != NULL)
		    call pargstr (S(mw,WCS_SYSTEM(wp)))
		else
		    call pargstr ("noname")
		call pargi (WCS_NWATTR(wp))
		call pargi (WCS_NFUNC(wp))

	    call fprintf (fd, "R = [")
		do i = 1, ndim {
		    if (i > 1)
			call fprintf (fd, " ")
		    call fprintf (fd, "%g")
			call pargd (D(mw,WCS_R(wp)+i-1))
		}
		call fprintf (fd, "]\n")

	    call fprintf (fd, "W = [")
		do i = 1, ndim {
		    if (i > 1)
			call fprintf (fd, " ")
		    call fprintf (fd, "%g")
			call pargd (D(mw,WCS_W(wp)+i-1))
		}
		call fprintf (fd, "]\n")

	    call fprintf (fd, "CD = [")
		do j = 1, ndim {
		    if (j > 1)
			call fprintf (fd, "; ")
		    do i = 1, ndim {
			if (i > 1)
			    call fprintf (fd, " ")
			call fprintf (fd, "%g")
			    call pargd (D(mw,WCS_CD(wp)+(j-1)*ndim+i-1))
		    }
		}
		call fprintf (fd, "]\n")
	}

end
