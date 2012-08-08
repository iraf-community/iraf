include	<imhdr.h>
include	<imset.h>



# FIXCOL -- Linearly interpolate columns across a region.

procedure fixcols (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, j, nx, ny
real	f1, f2, scale
pointer	a, b, c
pointer	imgs2s(), imps2s()

begin
	c = imps2s (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (x1 == 1) {
	    a = imgs2s (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny
		call amovks (Mems[a + i - 1], Mems[c + (i - 1) * nx], nx)
	} else if (x2 == IM_LEN (image, 1)) {
	    a = imgs2s (image, x1 - 1, x1 - 1,  y1, y2)
	    do i = 1, ny
		call amovks (Mems[a + i - 1], Mems[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2s (image, x1 - 1, x1 - 1, y1, y2)
	    b = imgs2s (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny {
		f1 = Mems[a + i - 1]
		f2 = Mems[b + i - 1]
		scale = (f2 - f1) / (nx + 1)
		do j = 1, nx
		    Mems[c + (i - 1) * nx + j - 1] = j * scale + f1
	    }
	}
end



# FIXCOL -- Linearly interpolate columns across a region.

procedure fixcoli (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, j, nx, ny
real	f1, f2, scale
pointer	a, b, c
pointer	imgs2i(), imps2i()

begin
	c = imps2i (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (x1 == 1) {
	    a = imgs2i (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny
		call amovki (Memi[a + i - 1], Memi[c + (i - 1) * nx], nx)
	} else if (x2 == IM_LEN (image, 1)) {
	    a = imgs2i (image, x1 - 1, x1 - 1,  y1, y2)
	    do i = 1, ny
		call amovki (Memi[a + i - 1], Memi[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2i (image, x1 - 1, x1 - 1, y1, y2)
	    b = imgs2i (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny {
		f1 = Memi[a + i - 1]
		f2 = Memi[b + i - 1]
		scale = (f2 - f1) / (nx + 1)
		do j = 1, nx
		    Memi[c + (i - 1) * nx + j - 1] = j * scale + f1
	    }
	}
end



# FIXCOL -- Linearly interpolate columns across a region.

procedure fixcoll (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, j, nx, ny
real	f1, f2, scale
pointer	a, b, c
pointer	imgs2l(), imps2l()

begin
	c = imps2l (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (x1 == 1) {
	    a = imgs2l (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny
		call amovkl (Meml[a + i - 1], Meml[c + (i - 1) * nx], nx)
	} else if (x2 == IM_LEN (image, 1)) {
	    a = imgs2l (image, x1 - 1, x1 - 1,  y1, y2)
	    do i = 1, ny
		call amovkl (Meml[a + i - 1], Meml[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2l (image, x1 - 1, x1 - 1, y1, y2)
	    b = imgs2l (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny {
		f1 = Meml[a + i - 1]
		f2 = Meml[b + i - 1]
		scale = (f2 - f1) / (nx + 1)
		do j = 1, nx
		    Meml[c + (i - 1) * nx + j - 1] = j * scale + f1
	    }
	}
end



# FIXCOL -- Linearly interpolate columns across a region.

procedure fixcolr (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, j, nx, ny
real	f1, f2, scale
pointer	a, b, c
pointer	imgs2r(), imps2r()

begin
	c = imps2r (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (x1 == 1) {
	    a = imgs2r (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny
		call amovkr (Memr[a + i - 1], Memr[c + (i - 1) * nx], nx)
	} else if (x2 == IM_LEN (image, 1)) {
	    a = imgs2r (image, x1 - 1, x1 - 1,  y1, y2)
	    do i = 1, ny
		call amovkr (Memr[a + i - 1], Memr[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2r (image, x1 - 1, x1 - 1, y1, y2)
	    b = imgs2r (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny {
		f1 = Memr[a + i - 1]
		f2 = Memr[b + i - 1]
		scale = (f2 - f1) / (nx + 1)
		do j = 1, nx
		    Memr[c + (i - 1) * nx + j - 1] = j * scale + f1
	    }
	}
end



# FIXCOL -- Linearly interpolate columns across a region.

procedure fixcold (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, j, nx, ny
real	f1, f2, scale
pointer	a, b, c
pointer	imgs2d(), imps2d()

begin
	c = imps2d (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (x1 == 1) {
	    a = imgs2d (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny
		call amovkd (Memd[a + i - 1], Memd[c + (i - 1) * nx], nx)
	} else if (x2 == IM_LEN (image, 1)) {
	    a = imgs2d (image, x1 - 1, x1 - 1,  y1, y2)
	    do i = 1, ny
		call amovkd (Memd[a + i - 1], Memd[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2d (image, x1 - 1, x1 - 1, y1, y2)
	    b = imgs2d (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny {
		f1 = Memd[a + i - 1]
		f2 = Memd[b + i - 1]
		scale = (f2 - f1) / (nx + 1)
		do j = 1, nx
		    Memd[c + (i - 1) * nx + j - 1] = j * scale + f1
	    }
	}
end



# FIXCOL -- Linearly interpolate columns across a region.

procedure fixcolx (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, j, nx, ny
real	f1, f2, scale
pointer	a, b, c
pointer	imgs2x(), imps2x()

begin
	c = imps2x (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (x1 == 1) {
	    a = imgs2x (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny
		call amovkx (Memx[a + i - 1], Memx[c + (i - 1) * nx], nx)
	} else if (x2 == IM_LEN (image, 1)) {
	    a = imgs2x (image, x1 - 1, x1 - 1,  y1, y2)
	    do i = 1, ny
		call amovkx (Memx[a + i - 1], Memx[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2x (image, x1 - 1, x1 - 1, y1, y2)
	    b = imgs2x (image, x2 + 1, x2 + 1, y1, y2)
	    do i = 1, ny {
		f1 = Memx[a + i - 1]
		f2 = Memx[b + i - 1]
		scale = (f2 - f1) / (nx + 1)
		do j = 1, nx
		    Memx[c + (i - 1) * nx + j - 1] = j * scale + f1
	    }
	}
end
