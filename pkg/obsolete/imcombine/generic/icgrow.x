# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<pmset.h>
include	"../icombine.h"

# IC_GROW --  Mark neigbors of rejected pixels.
# The rejected pixels (original plus grown) are saved in pixel masks.

procedure ic_grow (out, v, m, n, buf, nimages, npts, pms)

pointer	out			# Output image pointer
long	v[ARB]			# Output vector
pointer	m[ARB]			# Image id pointers
int	n[ARB]			# Number of good pixels
int	buf[npts,nimages]	# Working buffer
int	nimages			# Number of images
int	npts			# Number of output points per line
pointer	pms			# Pointer to array of pixel masks

int	i, j, k, l, line, nl, rop, igrow, nset, or()
real	grow2, i2
pointer	mp, pm, pm_newmask()
errchk	pm_newmask()

include	"../icombine.com"

begin
	if (dflag == D_NONE || grow == 0.)
	    return

	line = v[2]
	nl = IM_LEN(out,2)
	rop = or (PIX_SRC, PIX_DST)

	igrow = grow
	grow2 = grow**2
	do l = 0, igrow {
	    i2 = grow2 - l * l
	    call aclri (buf, npts*nimages)
	    nset = 0
	    do j = 1, npts {
		do k = n[j]+1, nimages {
		    mp = Memi[m[k]+j-1]
		    if (mp == 0)
			next
		    do i = 0, igrow {
			if (i**2 > i2)
			    next
			if (j > i)
			    buf[j-i,mp] = 1
			if (j+i <= npts)
			    buf[j+i,mp] = 1
			nset = nset + 1
		    }
		}
	    }
	    if (nset == 0)
		return

	    if (pms == NULL) {
		call malloc (pms, nimages, TY_POINTER)
		do i = 1, nimages
		    Memi[pms+i-1] = pm_newmask (out, 1)
	    }
	    do i = 1, nimages {
		pm = Memi[pms+i-1]
		v[2] = line - l
		if (v[2] > 0)
		    call pmplpi (pm, v, buf[1,i], 1, npts, rop)
		v[2] = line + l
		if (v[2] <= nl)
		    call pmplpi (pm, v, buf[1,i], 1, npts, rop)
	    }
	}
	v[2] = line
end



# IC_GROW$T --  Reject pixels.

procedure ic_grows (v, d, m, n, buf, nimages, npts, pms)

long	v[ARB]			# Output vector
pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[ARB]			# Number of good pixels
int	buf[ARB]		# Buffer of npts
int	nimages			# Number of images
int	npts			# Number of output points per line
pointer	pms			# Pointer to array of pixel masks

int	i, j, k
pointer	pm
bool	pl_linenotempty()

include	"../icombine.com"

begin
	do k = 1, nimages {
	    pm = Memi[pms+k-1]
	    if (!pl_linenotempty (pm, v))
		next
	    call pmglpi (pm, v, buf, 1, npts, PIX_SRC)
	    do i = 1, npts {
		if (buf[i] == 0)
		    next
		for (j = 1; j <= n[i]; j = j + 1) {
		    if (Memi[m[j]+i-1] == k) {
			if (j < n[i]) {
			    Mems[d[j]+i-1] = Mems[d[n[i]]+i-1]
			    Memi[m[j]+i-1] = Memi[m[n[i]]+i-1]
			}
			n[i] = n[i] - 1
			dflag = D_MIX
			break
		    }
		}
	    }
	}
end

# IC_GROW$T --  Reject pixels.

procedure ic_growi (v, d, m, n, buf, nimages, npts, pms)

long	v[ARB]			# Output vector
pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[ARB]			# Number of good pixels
int	buf[ARB]		# Buffer of npts
int	nimages			# Number of images
int	npts			# Number of output points per line
pointer	pms			# Pointer to array of pixel masks

int	i, j, k
pointer	pm
bool	pl_linenotempty()

include	"../icombine.com"

begin
	do k = 1, nimages {
	    pm = Memi[pms+k-1]
	    if (!pl_linenotempty (pm, v))
		next
	    call pmglpi (pm, v, buf, 1, npts, PIX_SRC)
	    do i = 1, npts {
		if (buf[i] == 0)
		    next
		for (j = 1; j <= n[i]; j = j + 1) {
		    if (Memi[m[j]+i-1] == k) {
			if (j < n[i]) {
			    Memi[d[j]+i-1] = Memi[d[n[i]]+i-1]
			    Memi[m[j]+i-1] = Memi[m[n[i]]+i-1]
			}
			n[i] = n[i] - 1
			dflag = D_MIX
			break
		    }
		}
	    }
	}
end

# IC_GROW$T --  Reject pixels.

procedure ic_growr (v, d, m, n, buf, nimages, npts, pms)

long	v[ARB]			# Output vector
pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[ARB]			# Number of good pixels
int	buf[ARB]		# Buffer of npts
int	nimages			# Number of images
int	npts			# Number of output points per line
pointer	pms			# Pointer to array of pixel masks

int	i, j, k
pointer	pm
bool	pl_linenotempty()

include	"../icombine.com"

begin
	do k = 1, nimages {
	    pm = Memi[pms+k-1]
	    if (!pl_linenotempty (pm, v))
		next
	    call pmglpi (pm, v, buf, 1, npts, PIX_SRC)
	    do i = 1, npts {
		if (buf[i] == 0)
		    next
		for (j = 1; j <= n[i]; j = j + 1) {
		    if (Memi[m[j]+i-1] == k) {
			if (j < n[i]) {
			    Memr[d[j]+i-1] = Memr[d[n[i]]+i-1]
			    Memi[m[j]+i-1] = Memi[m[n[i]]+i-1]
			}
			n[i] = n[i] - 1
			dflag = D_MIX
			break
		    }
		}
	    }
	}
end

# IC_GROW$T --  Reject pixels.

procedure ic_growd (v, d, m, n, buf, nimages, npts, pms)

long	v[ARB]			# Output vector
pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[ARB]			# Number of good pixels
int	buf[ARB]		# Buffer of npts
int	nimages			# Number of images
int	npts			# Number of output points per line
pointer	pms			# Pointer to array of pixel masks

int	i, j, k
pointer	pm
bool	pl_linenotempty()

include	"../icombine.com"

begin
	do k = 1, nimages {
	    pm = Memi[pms+k-1]
	    if (!pl_linenotempty (pm, v))
		next
	    call pmglpi (pm, v, buf, 1, npts, PIX_SRC)
	    do i = 1, npts {
		if (buf[i] == 0)
		    next
		for (j = 1; j <= n[i]; j = j + 1) {
		    if (Memi[m[j]+i-1] == k) {
			if (j < n[i]) {
			    Memd[d[j]+i-1] = Memd[d[n[i]]+i-1]
			    Memi[m[j]+i-1] = Memi[m[n[i]]+i-1]
			}
			n[i] = n[i] - 1
			dflag = D_MIX
			break
		    }
		}
	    }
	}
end
