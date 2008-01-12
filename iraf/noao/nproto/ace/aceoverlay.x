include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	"ace.h"


pointer procedure overlay (ovrly, im)

char	ovrly[ARB]		#I Overlay name
pointer	im			#I Reference image
pointer	ovr			#O Overlay pointer

int	i, j, nc, nl, val
long	v[2]
pointer	sp, fname, pm, buf

int	nowhite(), andi()
bool	pm_linenotempty()
pointer	ods_pmmap(), imstati()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	if (nowhite (ovrly, Memc[fname], SZ_FNAME) == 0) {
	    call sfree (sp)
	    return (NULL)
	}

	if (Memc[fname] == '!') {
	    iferr (call imgstr (im, Memc[fname+1], Memc[fname], SZ_FNAME)) {
		call sfree (sp)
		return (NULL)
	    }
	}

	iferr (ovr = ods_pmmap (Memc[fname], im)) {
	    call sfree (sp)
	    call erract (EA_WARN)
	    return (NULL)
	}

	nc = IM_LEN(ovr,1)
	nl = IM_LEN(ovr,2)
	pm = imstati (ovr, IM_PMDES)

	call salloc (buf, nc, TY_INT)

	v[1] = 1
	do i = 1, nl {
	    v[2] = i
	    if (!pm_linenotempty(pm, v))
		next
	    call pmglpi (pm, v, Memi[buf], 0, nc, 0)
	    do j = 0, nc-1 {
		val = Memi[buf+j]
		if (val == 0)
		    next
		else if (val < NUMSTART)
		    val = 1
		else {
		    val = andi (val, MASK_BNDRY)
		    if (val != 0)
			val = mod (andi (Memi[buf+j], MASK_NUM), 9) + 2
			#val = 1
		}
		Memi[buf+j] = val
	    }
	    call pmplpi (pm, v, Memi[buf], 0, nc, PIX_SRC)
	}

	call sfree (sp)

	return (ovr)
end
