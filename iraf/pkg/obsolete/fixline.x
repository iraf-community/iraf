include	<imhdr.h>
include	<imset.h>



# FIXLINE -- Linearly interpolate lines across a region.

procedure fixlines (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, nx, ny
real	f1, f2
pointer	a, b, c

pointer	imgs2s(), imps2s()

begin
	c = imps2s (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (y1 == 1) {
	    a = imgs2s (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny
		call amovs (Mems[a], Mems[c + (i - 1) * nx], nx)
	} else if (y2 == IM_LEN (image, 2)) {
	    a = imgs2s (image, x1, x2,  y1 - 1, y1 - 1)
	    do i = 1, ny
		call amovs (Mems[a], Mems[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2s (image, x1, x2, y1 - 1, y1 - 1)
	    b = imgs2s (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny {
		f2 = i / (ny + 1.)
		f1 = 1 - f2
		call awsus (Mems[a], Mems[b], Mems[c+(i-1)*nx], nx, f1, f2)
	    }
	}
end



# FIXLINE -- Linearly interpolate lines across a region.

procedure fixlinei (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, nx, ny
real	f1, f2
pointer	a, b, c

pointer	imgs2i(), imps2i()

begin
	c = imps2i (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (y1 == 1) {
	    a = imgs2i (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny
		call amovi (Memi[a], Memi[c + (i - 1) * nx], nx)
	} else if (y2 == IM_LEN (image, 2)) {
	    a = imgs2i (image, x1, x2,  y1 - 1, y1 - 1)
	    do i = 1, ny
		call amovi (Memi[a], Memi[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2i (image, x1, x2, y1 - 1, y1 - 1)
	    b = imgs2i (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny {
		f2 = i / (ny + 1.)
		f1 = 1 - f2
		call awsui (Memi[a], Memi[b], Memi[c+(i-1)*nx], nx, f1, f2)
	    }
	}
end



# FIXLINE -- Linearly interpolate lines across a region.

procedure fixlinel (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, nx, ny
real	f1, f2
pointer	a, b, c

pointer	imgs2l(), imps2l()

begin
	c = imps2l (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (y1 == 1) {
	    a = imgs2l (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny
		call amovl (Meml[a], Meml[c + (i - 1) * nx], nx)
	} else if (y2 == IM_LEN (image, 2)) {
	    a = imgs2l (image, x1, x2,  y1 - 1, y1 - 1)
	    do i = 1, ny
		call amovl (Meml[a], Meml[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2l (image, x1, x2, y1 - 1, y1 - 1)
	    b = imgs2l (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny {
		f2 = i / (ny + 1.)
		f1 = 1 - f2
		call awsul (Meml[a], Meml[b], Meml[c+(i-1)*nx], nx, f1, f2)
	    }
	}
end



# FIXLINE -- Linearly interpolate lines across a region.

procedure fixliner (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, nx, ny
real	f1, f2
pointer	a, b, c

pointer	imgs2r(), imps2r()

begin
	c = imps2r (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (y1 == 1) {
	    a = imgs2r (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny
		call amovr (Memr[a], Memr[c + (i - 1) * nx], nx)
	} else if (y2 == IM_LEN (image, 2)) {
	    a = imgs2r (image, x1, x2,  y1 - 1, y1 - 1)
	    do i = 1, ny
		call amovr (Memr[a], Memr[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2r (image, x1, x2, y1 - 1, y1 - 1)
	    b = imgs2r (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny {
		f2 = i / (ny + 1.)
		f1 = 1 - f2
		call awsur (Memr[a], Memr[b], Memr[c+(i-1)*nx], nx, f1, f2)
	    }
	}
end



# FIXLINE -- Linearly interpolate lines across a region.

procedure fixlined (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, nx, ny
double	f1, f2
pointer	a, b, c

pointer	imgs2d(), imps2d()

begin
	c = imps2d (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (y1 == 1) {
	    a = imgs2d (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny
		call amovd (Memd[a], Memd[c + (i - 1) * nx], nx)
	} else if (y2 == IM_LEN (image, 2)) {
	    a = imgs2d (image, x1, x2,  y1 - 1, y1 - 1)
	    do i = 1, ny
		call amovd (Memd[a], Memd[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2d (image, x1, x2, y1 - 1, y1 - 1)
	    b = imgs2d (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny {
		f2 = i / (ny + 1.)
		f1 = 1 - f2
		call awsud (Memd[a], Memd[b], Memd[c+(i-1)*nx], nx, f1, f2)
	    }
	}
end



# FIXLINE -- Linearly interpolate lines across a region.

procedure fixlinex (image, x1, x2, y1, y2)

pointer	image				# Image pointer
int	x1, x2, y1, y2			# Region to be fixed

int	i, nx, ny
complex	f1, f2
pointer	a, b, c

pointer	imgs2x(), imps2x()

begin
	c = imps2x (image, x1, x2, y1, y2)

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	if (y1 == 1) {
	    a = imgs2x (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny
		call amovx (Memx[a], Memx[c + (i - 1) * nx], nx)
	} else if (y2 == IM_LEN (image, 2)) {
	    a = imgs2x (image, x1, x2,  y1 - 1, y1 - 1)
	    do i = 1, ny
		call amovx (Memx[a], Memx[c + (i - 1) * nx], nx)
	} else {
	    call imseti (image, IM_NBUFS, 2)
	    a = imgs2x (image, x1, x2, y1 - 1, y1 - 1)
	    b = imgs2x (image, x1, x2, y2 + 1, y2 + 1)
	    do i = 1, ny {
		f2 = i / (ny + 1.)
		f1 = 1 - f2
		call awsux (Memx[a], Memx[b], Memx[c+(i-1)*nx], nx, f1, f2)
	    }
	}
end
