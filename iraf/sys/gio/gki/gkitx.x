# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_TEXT -- Text drawing instruction.
#
# BOI GKI_TEXT L P N T
#    
#        L(i)            6 + N
#        P(p)            starting point of character string
#        N(i)            number of characters in string T
#        T(Nc)           string of N ASCII characters

procedure gki_text (fd, x, y, text)

int	fd			# output file
int	x, y			# position at which text is to be drawn
char	text[ARB]		# text string to be drawn

int	epa
int	ip, n
pointer	sp, gki, op
int	strlen()
include	"gki.com"

begin
	call smark (sp)

	n = strlen (text)
	call salloc (gki, GKI_TEXT_LEN + n, TY_SHORT)

	# Pack the text string as a SHORT integer array.
	op = gki + GKI_TEXT_T - 1
	for (ip=1;  ip <= n;  ip=ip+1) {
	    Mems[op] = text[ip]
	    op = op + 1
	}

	if (IS_INLINE(fd)) {
	    epa = gk_dd[GKI_TEXT]
	    if (epa != 0)
		call zcall4 (epa, x, y, Mems[gki+GKI_TEXT_T-1], n)
	} else {
	    Mems[gki  ] = BOI
	    Mems[gki+1] = GKI_TEXT
	    Mems[gki+2] = GKI_TEXT_LEN + n
	    Mems[gki+GKI_TEXT_L-1]   = GKI_TEXT_LEN + n
	    Mems[gki+GKI_TEXT_P-1]   = x
	    Mems[gki+GKI_TEXT_P-1+1] = y
	    Mems[gki+GKI_TEXT_N-1]   = n

	    call write (gk_fd[fd], Mems[gki], (GKI_TEXT_LEN + n) * SZ_SHORT)
	}

	call sfree (sp)
end
