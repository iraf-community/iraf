# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_MFTITLE -- Write the metafile title.
#
# BOI GKI_MFTITLE L N T
#    
#        L(i)            4 + N
#        N(i)            number of characters in field T
#        T(Nc)           title string identifying metafile

procedure gki_mftitle (fd, title)

int	fd			# output file
char	title[ARB]		# title string

int	epa
int	ip, n
pointer	sp, gki, op
int	strlen()
include	"gki.com"

begin
	call smark (sp)

	n = strlen (title)
	call salloc (gki, GKI_MFTITLE_LEN + n, TY_SHORT)

	# Pack the title name as a SHORT integer array.
	op = gki + GKI_MFTITLE_T - 1
	for (ip=1;  ip <= n;  ip=ip+1) {
	    Mems[op] = title[ip]
	    op = op + 1
	}

	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_MFTITLE]
	    if (epa != 0)
		call zcall2 (epa, Mems[gki+GKI_MFTITLE_T-1], n)
	} else {
	    Mems[gki  ] = BOI
	    Mems[gki+1] = GKI_MFTITLE
	    Mems[gki+2] = GKI_MFTITLE_LEN + n
	    Mems[gki+3] = n
	    call write (gk_fd[fd], Mems[gki], (GKI_MFTITLE_LEN + n) * SZ_SHORT)
	}

	call sfree (sp)
end
