include	<pmset.h>
include	"ace.h"


# BNDRY --  Flag boundary pixels of unsplit objects.
# Assume the boundary flag is not set.

procedure bndry (om, logfd)

pointer	om			#I Object mask
int	logfd			#I Logfile

int	i, c, c1, c2, l, nc, nl, num, bndryval, val, vallast
pointer	sp, v, irl, irlptr, orl, orlptr, bufs, buf1, buf2, buf3

int	andi(), ori()

begin
	call smark (sp)
	call salloc (v, PM_MAXDIM, TY_LONG)

	if (logfd != NULL)
	    call fprintf (logfd, "  Set boundary mask:\n")

	call pm_gsize (om, nc, Meml[v], nl)
	nc = Meml[v]; nl = Meml[v+1]
	Meml[v] = 1

	# Allocate buffers.
	call salloc (irl, 3+3*nc, TY_INT)
	call salloc (orl, 3+3*nc, TY_INT)
	call salloc (bufs, 3, TY_POINTER)
	call salloc (Memi[bufs], nc, TY_INT)
	call salloc (Memi[bufs+1], nc, TY_INT)
	call salloc (Memi[bufs+2], nc, TY_INT)

	Memi[orl+1] = nc

	# First line.
	l = 1
	buf2 = Memi[bufs+mod(l,3)]
	buf3 = Memi[bufs+mod(2,3)]

	Meml[v+1] = l + 1
	call pmglpi (om, Meml[v], Memi[buf3], 0, nc, 0)
	Meml[v+1] = l
	call pmglpi (om, Meml[v], Memi[buf2], 0, nc, 0)
	call pmglri (om, Meml[v], Memi[irl], 0, nc, 0)

	irlptr = irl
	orlptr = orl 
	do i = 2, Memi[irl] {
	    irlptr = irlptr + 3
	    num = Memi[irlptr+2]

	    if (num < NUMSTART || MSPLIT(num)) {
		orlptr = orlptr + 3
		Memi[orlptr] = Memi[irlptr]
		Memi[orlptr+1] = Memi[irlptr+1]
		Memi[orlptr+2] = num
		next
	    }

	    bndryval = MSETFLAG (num, MASK_BNDRY)
	    c1 = Memi[irlptr] - 1
	    c2 = c1 + Memi[irlptr+1] - 1
	    do c = c1, c2
		Memi[buf2+c] = bndryval

	    orlptr = orlptr + 3
	    Memi[orlptr] = Memi[irlptr]
	    Memi[orlptr+1] = Memi[irlptr+1]
	    Memi[orlptr+2] = bndryval
	}
	Memi[orl] = 1 + (orlptr - orl) / 3
	call pmplri (om, Meml[v], Memi[orl], 0, nc, PIX_SRC)

	# Interior lines.
	do l = 2, nl-1 {
	    buf1 = Memi[bufs+mod(l-1,3)]
	    buf2 = Memi[bufs+mod(l,3)]
	    buf3 = Memi[bufs+mod(l+1,3)]

	    Meml[v+1] = l + 1
	    call pmglpi (om, Meml[v], Memi[buf3], 0, nc, 0)
	    Meml[v+1] = l
	    call pmglri (om, Meml[v], Memi[irl], 0, nc, 0)

	    irlptr = irl
	    orlptr = orl
	    do i = 2, Memi[irl] {
		irlptr = irlptr + 3
		num = Memi[irlptr+2]

		if (num < NUMSTART || MSPLIT(num)) {
		    orlptr = orlptr + 3
		    Memi[orlptr] = Memi[irlptr]
		    Memi[orlptr+1] = Memi[irlptr+1]
		    Memi[orlptr+2] = num
		    next
		}

		c1 = Memi[irlptr] - 1
		c2 = c1 + Memi[irlptr+1] - 1
		bndryval = MSETFLAG (num, MASK_BNDRY)

		Memi[buf2+c1] = bndryval

		orlptr = orlptr + 3
		Memi[orlptr] = c1 + 1
		Memi[orlptr+2] = bndryval
		vallast = bndryval

		do c = c1+1, c2-1 {
		    val = num
		    if (Memi[buf3+c-1] != num)
			val = bndryval
		    else if (Memi[buf3+c] != num)
			val = bndryval
		    else if (Memi[buf3+c+1] != num)
			val = bndryval
		    else if (Memi[buf1+c-1] != num && Memi[buf1+c-1]!=bndryval)
			val = bndryval
		    else if (Memi[buf1+c] != num && Memi[buf1+c] != bndryval)
			val = bndryval
		    else if (Memi[buf1+c+1] != num && Memi[buf1+c+1]!=bndryval)
			val = bndryval

		    if (val == bndryval) 
			Memi[buf2+c] = val

		    if (val != vallast) {
			Memi[orlptr+1] = c - Memi[orlptr] + 1
			orlptr = orlptr + 3

			Memi[orlptr] = c + 1
			Memi[orlptr+2] = val
			vallast = val
		    }
		}

		Memi[buf2+c2] = bndryval

		if (vallast != bndryval) {
		    Memi[orlptr+1] = c2 - Memi[orlptr] + 1
		    orlptr = orlptr + 3
		    Memi[orlptr] = c2 + 1
		    Memi[orlptr+1] = 1
		    Memi[orlptr+2] = bndryval
		} else
		    Memi[orlptr+1] = c2 - Memi[orlptr] + 2
	    }

	    Memi[orl] = 1 + (orlptr - orl) / 3
	    call pmplri (om, Meml[v], Memi[orl], 0, nc, PIX_SRC)
	}

	# Last line.
	l = nl
	buf2 = Memi[bufs+mod(l,3)]

	Meml[v+1] = l
	call pmglri (om, Meml[v], Memi[irl], 0, nc, 0)

	irlptr = irl
	orlptr = orl
	do i = 2, Memi[irl] {
	    irlptr = irlptr + 3
	    num = Memi[irlptr+2]

	    if (num < NUMSTART || MSPLIT(num)) {
		orlptr = orlptr + 3
		Memi[orlptr] = Memi[irlptr]
		Memi[orlptr+1] = Memi[irlptr+1]
		Memi[orlptr+2] = num
		next
	    }

	    bndryval = MSETFLAG (num, MASK_BNDRY)
	    c1 = Memi[irlptr] - 1
	    c2 = c1 + Memi[irlptr+1] - 1
	    do c = c1, c2
		Memi[buf2+c] = bndryval

	    orlptr = orlptr + 3
	    Memi[orlptr] = Memi[irlptr]
	    Memi[orlptr+1] = Memi[irlptr+1]
	    Memi[orlptr+2] = bndryval
	}
	Memi[orl] = 1 + (orlptr - orl) / 3
	call pmplri (om, Meml[v], Memi[orl], 0, nc, PIX_SRC)

	call sfree (sp)
end
