include	<imhdr.h>
include	<pmset.h>
include	"xtfixpix.h"




# XT_FP -- Get the specified line of image data and replace bad pixels by
# interpolation.

pointer procedure xt_fps (fp, im, line, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

pointer	imgl2s(), xt_fpss()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2s (im, line))

	col1 = 1
	col2 = IM_LEN(im,1)
	line1 = 1
	line2 = IM_LEN(im,2)

	return (xt_fpss (fp, im, line, col1, col2, line1, line2, fd))
end


# XT_FXS -- Get the specified line of image data and replace bad pixels by
# interpolation within a specified section.

pointer procedure xt_fpss (fp, im, line, col1, col2, line1, line2, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

int	i, j, nc, nl, ncols, c1, c2, l1, l2, l3, l4
long	v[IM_MAXDIM]
real	a, b, c, d, val
short	indef
pointer	pm, data, bp

bool	pm_linenotempty()
pointer	imgl2s(), xt_fpvals()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2s (im, line))

	# Initialize
	pm = FP_PM(fp)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	ncols = FP_NCOLS(fp)
	call amovkl (long(1), v, IM_MAXDIM)
	v[2] = line

	# If there might be column interpolation initialize value arrays.
	if (ncols > 0 && FP_PV1(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_SHORT
	    call malloc (FP_PV1(fp), ncols, FP_PIXTYPE(fp))
	    call malloc (FP_PV2(fp), ncols, FP_PIXTYPE(fp))
	    indef = INDEFS
	    call amovks (indef, Mems[FP_V1(fp,1)], ncols)
	    call amovks (indef, Mems[FP_V2(fp,1)], ncols)
	}

	# If there are no bad pixels in the line and the line contains
	# no column interpolation endpoints return the data directly.
	# Otherwise get the line and fill in any endpoints that may
	# be used later.

	if (!pm_linenotempty (pm, v)) {
	    if (line < FP_LMIN(fp) || line > FP_LMAX(fp))
		return (imgl2s (im, line))
	    else
		return (xt_fpvals (fp, im, line))
	}

	# Get the pixel mask.
	call malloc (bp, nc, TY_SHORT)
	call pmglps (pm, v, Mems[bp], 0, nc, PIX_SRC)
	bp = bp - 1

	# Check if any column interpolation endpoints are needed and
	# set them.  Set any other endpoints on the same lines at
	# the same time.

	if (line >= FP_LMIN(fp) && line < FP_LMAX(fp)) {
	    j = 1
	    do i = col1, col2 {
		if (Mems[bp+i] == FP_CVAL(fp)) {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			if (line>FP_L1(fp,j) && line<FP_L2(fp,j)) {
			    if (IS_INDEFS(Mems[FP_V1(fp,j)]))
				data = xt_fpvals (fp, im, FP_L1(fp,j))
			    if (IS_INDEFS(Mems[FP_V2(fp,j)]))
				data = xt_fpvals (fp, im, FP_L2(fp,j))
			}
		    }
		}
	    }
	}

	# Fix pixels by column or line interpolation.
	if (FP_DATA(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_SHORT
	    call malloc (FP_DATA(fp), nc, FP_PIXTYPE(fp))
	}
	data = FP_DATA(fp)
	call amovs (Mems[xt_fpvals(fp,im,line)], Mems[data], nc)
	j = 1
	for (c1=col1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
	    ;
	while (c1 <= col2) {
	    c1 = c1 - 1
	    for (c2=c1+2; c2<=col2 && Mems[bp+c2]!=0; c2=c2+1)
		;
	    a = INDEFS
	    do i = c1+1, c2-1 {
		if (Mems[bp+i] == FP_LVAL(fp)) {
		    if (IS_INDEFS(a)) {
			if (c1 < col1 && c2 > col2) {
			    c1 = c2 + 1
			    next
			}
			if (c1 >= col1)
			    a = Mems[data+c1-1]
			else
			    a = Mems[data+c2-1]
			if (c2 <= col2)
			    b = (Mems[data+c2-1] - a) / (c2 - c1)
			else
			    b = 0.
		    }
		    val = a + b * (i - c1)
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargs (Mems[data+i-1])
			    call pargr (val)
			if (c1 >= col1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c1)
			    call pargi (line)
			}
			if (c2 <= col2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c2)
			    call pargi (line)
			}
			call fprintf (fd, "\n")
		    }
		} else {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			l1 = FP_L1(fp,j)
			l2 = FP_L2(fp,j)
			if (l1 < line1 && l2 > line2)
			    next
			if (line > l1 && line < l2) {
			    if (l1 >= line1)
				c = Mems[FP_V1(fp,j)]
			    else
				c = Mems[FP_V2(fp,j)]
			    if (l2 <= line2) {
				d = (Mems[FP_V2(fp,j)] - c) / (l2 - l1)
				val = c + d * (line - l1)
			    } else
				val = c
			    l3 = l1
			    l4 = l2
			}
		    }
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargs (Mems[data+i-1])
			    call pargr (val)
			if (l1 >= line1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l3)
			}
			if (l2 <= line2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l4)
			}
			call fprintf (fd, "\n")
		    }
		}
		Mems[data+i-1] = nint (val)
	    }
	    for (c1=c2+1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
		;
	}

	call mfree (bp, TY_SHORT)
	return (data)
end


# XT_FPVAL -- Get data for the specified line and set the values for
# all column interpolation endpoints which occur at that line.

pointer procedure xt_fpvals (fp, im, line)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line

int	i
pointer	data, imgl2s()

begin
	# Set out of bounds values to 0.  These are not used but we need
	# to cancel the INDEF values.
	if (line < 1 || line > IM_LEN(im,2)) {
	    do i = 1, FP_NCOLS(fp) {
		if (line == FP_L1(fp,i))
		    Mems[FP_V1(fp,i)] = 0.
		else if (line == FP_L2(fp,i))
		    Mems[FP_V2(fp,i)] = 0.
	    }
	    return (NULL)
	}

	data = imgl2s (im, line)
	do i = 1, FP_NCOLS(fp) {
	    if (line == FP_L1(fp,i))
		Mems[FP_V1(fp,i)] = Mems[data+FP_COL(fp,i)-1]
	    else if (line == FP_L2(fp,i))
		Mems[FP_V2(fp,i)] = Mems[data+FP_COL(fp,i)-1]
	}

	return (data)
end



# XT_FP -- Get the specified line of image data and replace bad pixels by
# interpolation.

pointer procedure xt_fpi (fp, im, line, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

pointer	imgl2i(), xt_fpsi()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2i (im, line))

	col1 = 1
	col2 = IM_LEN(im,1)
	line1 = 1
	line2 = IM_LEN(im,2)

	return (xt_fpsi (fp, im, line, col1, col2, line1, line2, fd))
end


# XT_FXS -- Get the specified line of image data and replace bad pixels by
# interpolation within a specified section.

pointer procedure xt_fpsi (fp, im, line, col1, col2, line1, line2, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

int	i, j, nc, nl, ncols, c1, c2, l1, l2, l3, l4
long	v[IM_MAXDIM]
real	a, b, c, d, val
int	indef
pointer	pm, data, bp

bool	pm_linenotempty()
pointer	imgl2i(), xt_fpvali()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2i (im, line))

	# Initialize
	pm = FP_PM(fp)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	ncols = FP_NCOLS(fp)
	call amovkl (long(1), v, IM_MAXDIM)
	v[2] = line

	# If there might be column interpolation initialize value arrays.
	if (ncols > 0 && FP_PV1(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_INT
	    call malloc (FP_PV1(fp), ncols, FP_PIXTYPE(fp))
	    call malloc (FP_PV2(fp), ncols, FP_PIXTYPE(fp))
	    indef = INDEFI
	    call amovki (indef, Memi[FP_V1(fp,1)], ncols)
	    call amovki (indef, Memi[FP_V2(fp,1)], ncols)
	}

	# If there are no bad pixels in the line and the line contains
	# no column interpolation endpoints return the data directly.
	# Otherwise get the line and fill in any endpoints that may
	# be used later.

	if (!pm_linenotempty (pm, v)) {
	    if (line < FP_LMIN(fp) || line > FP_LMAX(fp))
		return (imgl2i (im, line))
	    else
		return (xt_fpvali (fp, im, line))
	}

	# Get the pixel mask.
	call malloc (bp, nc, TY_SHORT)
	call pmglps (pm, v, Mems[bp], 0, nc, PIX_SRC)
	bp = bp - 1

	# Check if any column interpolation endpoints are needed and
	# set them.  Set any other endpoints on the same lines at
	# the same time.

	if (line >= FP_LMIN(fp) && line < FP_LMAX(fp)) {
	    j = 1
	    do i = col1, col2 {
		if (Mems[bp+i] == FP_CVAL(fp)) {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			if (line>FP_L1(fp,j) && line<FP_L2(fp,j)) {
			    if (IS_INDEFI(Memi[FP_V1(fp,j)]))
				data = xt_fpvali (fp, im, FP_L1(fp,j))
			    if (IS_INDEFI(Memi[FP_V2(fp,j)]))
				data = xt_fpvali (fp, im, FP_L2(fp,j))
			}
		    }
		}
	    }
	}

	# Fix pixels by column or line interpolation.
	if (FP_DATA(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_INT
	    call malloc (FP_DATA(fp), nc, FP_PIXTYPE(fp))
	}
	data = FP_DATA(fp)
	call amovi (Memi[xt_fpvali(fp,im,line)], Memi[data], nc)
	j = 1
	for (c1=col1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
	    ;
	while (c1 <= col2) {
	    c1 = c1 - 1
	    for (c2=c1+2; c2<=col2 && Mems[bp+c2]!=0; c2=c2+1)
		;
	    a = INDEFI
	    do i = c1+1, c2-1 {
		if (Mems[bp+i] == FP_LVAL(fp)) {
		    if (IS_INDEFI(a)) {
			if (c1 < col1 && c2 > col2) {
			    c1 = c2 + 1
			    next
			}
			if (c1 >= col1)
			    a = Memi[data+c1-1]
			else
			    a = Memi[data+c2-1]
			if (c2 <= col2)
			    b = (Memi[data+c2-1] - a) / (c2 - c1)
			else
			    b = 0.
		    }
		    val = a + b * (i - c1)
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargi (Memi[data+i-1])
			    call pargr (val)
			if (c1 >= col1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c1)
			    call pargi (line)
			}
			if (c2 <= col2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c2)
			    call pargi (line)
			}
			call fprintf (fd, "\n")
		    }
		} else {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			l1 = FP_L1(fp,j)
			l2 = FP_L2(fp,j)
			if (l1 < line1 && l2 > line2)
			    next
			if (line > l1 && line < l2) {
			    if (l1 >= line1)
				c = Memi[FP_V1(fp,j)]
			    else
				c = Memi[FP_V2(fp,j)]
			    if (l2 <= line2) {
				d = (Memi[FP_V2(fp,j)] - c) / (l2 - l1)
				val = c + d * (line - l1)
			    } else
				val = c
			    l3 = l1
			    l4 = l2
			}
		    }
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargi (Memi[data+i-1])
			    call pargr (val)
			if (l1 >= line1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l3)
			}
			if (l2 <= line2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l4)
			}
			call fprintf (fd, "\n")
		    }
		}
		Memi[data+i-1] = nint (val)
	    }
	    for (c1=c2+1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
		;
	}

	call mfree (bp, TY_SHORT)
	return (data)
end


# XT_FPVAL -- Get data for the specified line and set the values for
# all column interpolation endpoints which occur at that line.

pointer procedure xt_fpvali (fp, im, line)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line

int	i
pointer	data, imgl2i()

begin
	# Set out of bounds values to 0.  These are not used but we need
	# to cancel the INDEF values.
	if (line < 1 || line > IM_LEN(im,2)) {
	    do i = 1, FP_NCOLS(fp) {
		if (line == FP_L1(fp,i))
		    Memi[FP_V1(fp,i)] = 0.
		else if (line == FP_L2(fp,i))
		    Memi[FP_V2(fp,i)] = 0.
	    }
	    return (NULL)
	}

	data = imgl2i (im, line)
	do i = 1, FP_NCOLS(fp) {
	    if (line == FP_L1(fp,i))
		Memi[FP_V1(fp,i)] = Memi[data+FP_COL(fp,i)-1]
	    else if (line == FP_L2(fp,i))
		Memi[FP_V2(fp,i)] = Memi[data+FP_COL(fp,i)-1]
	}

	return (data)
end



# XT_FP -- Get the specified line of image data and replace bad pixels by
# interpolation.

pointer procedure xt_fpl (fp, im, line, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

pointer	imgl2l(), xt_fpsl()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2l (im, line))

	col1 = 1
	col2 = IM_LEN(im,1)
	line1 = 1
	line2 = IM_LEN(im,2)

	return (xt_fpsl (fp, im, line, col1, col2, line1, line2, fd))
end


# XT_FXS -- Get the specified line of image data and replace bad pixels by
# interpolation within a specified section.

pointer procedure xt_fpsl (fp, im, line, col1, col2, line1, line2, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

int	i, j, nc, nl, ncols, c1, c2, l1, l2, l3, l4
long	v[IM_MAXDIM]
real	a, b, c, d, val
long	indef
pointer	pm, data, bp

bool	pm_linenotempty()
pointer	imgl2l(), xt_fpvall()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2l (im, line))

	# Initialize
	pm = FP_PM(fp)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	ncols = FP_NCOLS(fp)
	call amovkl (long(1), v, IM_MAXDIM)
	v[2] = line

	# If there might be column interpolation initialize value arrays.
	if (ncols > 0 && FP_PV1(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_LONG
	    call malloc (FP_PV1(fp), ncols, FP_PIXTYPE(fp))
	    call malloc (FP_PV2(fp), ncols, FP_PIXTYPE(fp))
	    indef = INDEFL
	    call amovkl (indef, Meml[FP_V1(fp,1)], ncols)
	    call amovkl (indef, Meml[FP_V2(fp,1)], ncols)
	}

	# If there are no bad pixels in the line and the line contains
	# no column interpolation endpoints return the data directly.
	# Otherwise get the line and fill in any endpoints that may
	# be used later.

	if (!pm_linenotempty (pm, v)) {
	    if (line < FP_LMIN(fp) || line > FP_LMAX(fp))
		return (imgl2l (im, line))
	    else
		return (xt_fpvall (fp, im, line))
	}

	# Get the pixel mask.
	call malloc (bp, nc, TY_SHORT)
	call pmglps (pm, v, Mems[bp], 0, nc, PIX_SRC)
	bp = bp - 1

	# Check if any column interpolation endpoints are needed and
	# set them.  Set any other endpoints on the same lines at
	# the same time.

	if (line >= FP_LMIN(fp) && line < FP_LMAX(fp)) {
	    j = 1
	    do i = col1, col2 {
		if (Mems[bp+i] == FP_CVAL(fp)) {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			if (line>FP_L1(fp,j) && line<FP_L2(fp,j)) {
			    if (IS_INDEFL(Meml[FP_V1(fp,j)]))
				data = xt_fpvall (fp, im, FP_L1(fp,j))
			    if (IS_INDEFL(Meml[FP_V2(fp,j)]))
				data = xt_fpvall (fp, im, FP_L2(fp,j))
			}
		    }
		}
	    }
	}

	# Fix pixels by column or line interpolation.
	if (FP_DATA(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_LONG
	    call malloc (FP_DATA(fp), nc, FP_PIXTYPE(fp))
	}
	data = FP_DATA(fp)
	call amovl (Meml[xt_fpvall(fp,im,line)], Meml[data], nc)
	j = 1
	for (c1=col1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
	    ;
	while (c1 <= col2) {
	    c1 = c1 - 1
	    for (c2=c1+2; c2<=col2 && Mems[bp+c2]!=0; c2=c2+1)
		;
	    a = INDEFL
	    do i = c1+1, c2-1 {
		if (Mems[bp+i] == FP_LVAL(fp)) {
		    if (IS_INDEFL(a)) {
			if (c1 < col1 && c2 > col2) {
			    c1 = c2 + 1
			    next
			}
			if (c1 >= col1)
			    a = Meml[data+c1-1]
			else
			    a = Meml[data+c2-1]
			if (c2 <= col2)
			    b = (Meml[data+c2-1] - a) / (c2 - c1)
			else
			    b = 0.
		    }
		    val = a + b * (i - c1)
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargl (Meml[data+i-1])
			    call pargr (val)
			if (c1 >= col1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c1)
			    call pargi (line)
			}
			if (c2 <= col2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c2)
			    call pargi (line)
			}
			call fprintf (fd, "\n")
		    }
		} else {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			l1 = FP_L1(fp,j)
			l2 = FP_L2(fp,j)
			if (l1 < line1 && l2 > line2)
			    next
			if (line > l1 && line < l2) {
			    if (l1 >= line1)
				c = Meml[FP_V1(fp,j)]
			    else
				c = Meml[FP_V2(fp,j)]
			    if (l2 <= line2) {
				d = (Meml[FP_V2(fp,j)] - c) / (l2 - l1)
				val = c + d * (line - l1)
			    } else
				val = c
			    l3 = l1
			    l4 = l2
			}
		    }
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargl (Meml[data+i-1])
			    call pargr (val)
			if (l1 >= line1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l3)
			}
			if (l2 <= line2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l4)
			}
			call fprintf (fd, "\n")
		    }
		}
		Meml[data+i-1] = nint (val)
	    }
	    for (c1=c2+1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
		;
	}

	call mfree (bp, TY_SHORT)
	return (data)
end


# XT_FPVAL -- Get data for the specified line and set the values for
# all column interpolation endpoints which occur at that line.

pointer procedure xt_fpvall (fp, im, line)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line

int	i
pointer	data, imgl2l()

begin
	# Set out of bounds values to 0.  These are not used but we need
	# to cancel the INDEF values.
	if (line < 1 || line > IM_LEN(im,2)) {
	    do i = 1, FP_NCOLS(fp) {
		if (line == FP_L1(fp,i))
		    Meml[FP_V1(fp,i)] = 0.
		else if (line == FP_L2(fp,i))
		    Meml[FP_V2(fp,i)] = 0.
	    }
	    return (NULL)
	}

	data = imgl2l (im, line)
	do i = 1, FP_NCOLS(fp) {
	    if (line == FP_L1(fp,i))
		Meml[FP_V1(fp,i)] = Meml[data+FP_COL(fp,i)-1]
	    else if (line == FP_L2(fp,i))
		Meml[FP_V2(fp,i)] = Meml[data+FP_COL(fp,i)-1]
	}

	return (data)
end



# XT_FP -- Get the specified line of image data and replace bad pixels by
# interpolation.

pointer procedure xt_fpr (fp, im, line, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

pointer	imgl2r(), xt_fpsr()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2r (im, line))

	col1 = 1
	col2 = IM_LEN(im,1)
	line1 = 1
	line2 = IM_LEN(im,2)

	return (xt_fpsr (fp, im, line, col1, col2, line1, line2, fd))
end


# XT_FXS -- Get the specified line of image data and replace bad pixels by
# interpolation within a specified section.

pointer procedure xt_fpsr (fp, im, line, col1, col2, line1, line2, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

int	i, j, nc, nl, ncols, c1, c2, l1, l2, l3, l4
long	v[IM_MAXDIM]
real	a, b, c, d, val
real	indef
pointer	pm, data, bp

bool	pm_linenotempty()
pointer	imgl2r(), xt_fpvalr()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2r (im, line))

	# Initialize
	pm = FP_PM(fp)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	ncols = FP_NCOLS(fp)
	call amovkl (long(1), v, IM_MAXDIM)
	v[2] = line

	# If there might be column interpolation initialize value arrays.
	if (ncols > 0 && FP_PV1(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_REAL
	    call malloc (FP_PV1(fp), ncols, FP_PIXTYPE(fp))
	    call malloc (FP_PV2(fp), ncols, FP_PIXTYPE(fp))
	    indef = INDEFR
	    call amovkr (indef, Memr[FP_V1(fp,1)], ncols)
	    call amovkr (indef, Memr[FP_V2(fp,1)], ncols)
	}

	# If there are no bad pixels in the line and the line contains
	# no column interpolation endpoints return the data directly.
	# Otherwise get the line and fill in any endpoints that may
	# be used later.

	if (!pm_linenotempty (pm, v)) {
	    if (line < FP_LMIN(fp) || line > FP_LMAX(fp))
		return (imgl2r (im, line))
	    else
		return (xt_fpvalr (fp, im, line))
	}

	# Get the pixel mask.
	call malloc (bp, nc, TY_SHORT)
	call pmglps (pm, v, Mems[bp], 0, nc, PIX_SRC)
	bp = bp - 1

	# Check if any column interpolation endpoints are needed and
	# set them.  Set any other endpoints on the same lines at
	# the same time.

	if (line >= FP_LMIN(fp) && line < FP_LMAX(fp)) {
	    j = 1
	    do i = col1, col2 {
		if (Mems[bp+i] == FP_CVAL(fp)) {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			if (line>FP_L1(fp,j) && line<FP_L2(fp,j)) {
			    if (IS_INDEFR(Memr[FP_V1(fp,j)]))
				data = xt_fpvalr (fp, im, FP_L1(fp,j))
			    if (IS_INDEFR(Memr[FP_V2(fp,j)]))
				data = xt_fpvalr (fp, im, FP_L2(fp,j))
			}
		    }
		}
	    }
	}

	# Fix pixels by column or line interpolation.
	if (FP_DATA(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_REAL
	    call malloc (FP_DATA(fp), nc, FP_PIXTYPE(fp))
	}
	data = FP_DATA(fp)
	call amovr (Memr[xt_fpvalr(fp,im,line)], Memr[data], nc)
	j = 1
	for (c1=col1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
	    ;
	while (c1 <= col2) {
	    c1 = c1 - 1
	    for (c2=c1+2; c2<=col2 && Mems[bp+c2]!=0; c2=c2+1)
		;
	    a = INDEFR
	    do i = c1+1, c2-1 {
		if (Mems[bp+i] == FP_LVAL(fp)) {
		    if (IS_INDEFR(a)) {
			if (c1 < col1 && c2 > col2) {
			    c1 = c2 + 1
			    next
			}
			if (c1 >= col1)
			    a = Memr[data+c1-1]
			else
			    a = Memr[data+c2-1]
			if (c2 <= col2)
			    b = (Memr[data+c2-1] - a) / (c2 - c1)
			else
			    b = 0.
		    }
		    val = a + b * (i - c1)
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargr (Memr[data+i-1])
			    call pargr (val)
			if (c1 >= col1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c1)
			    call pargi (line)
			}
			if (c2 <= col2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c2)
			    call pargi (line)
			}
			call fprintf (fd, "\n")
		    }
		} else {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			l1 = FP_L1(fp,j)
			l2 = FP_L2(fp,j)
			if (l1 < line1 && l2 > line2)
			    next
			if (line > l1 && line < l2) {
			    if (l1 >= line1)
				c = Memr[FP_V1(fp,j)]
			    else
				c = Memr[FP_V2(fp,j)]
			    if (l2 <= line2) {
				d = (Memr[FP_V2(fp,j)] - c) / (l2 - l1)
				val = c + d * (line - l1)
			    } else
				val = c
			    l3 = l1
			    l4 = l2
			}
		    }
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargr (Memr[data+i-1])
			    call pargr (val)
			if (l1 >= line1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l3)
			}
			if (l2 <= line2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l4)
			}
			call fprintf (fd, "\n")
		    }
		}
		Memr[data+i-1] = val
	    }
	    for (c1=c2+1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
		;
	}

	call mfree (bp, TY_SHORT)
	return (data)
end


# XT_FPVAL -- Get data for the specified line and set the values for
# all column interpolation endpoints which occur at that line.

pointer procedure xt_fpvalr (fp, im, line)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line

int	i
pointer	data, imgl2r()

begin
	# Set out of bounds values to 0.  These are not used but we need
	# to cancel the INDEF values.
	if (line < 1 || line > IM_LEN(im,2)) {
	    do i = 1, FP_NCOLS(fp) {
		if (line == FP_L1(fp,i))
		    Memr[FP_V1(fp,i)] = 0.
		else if (line == FP_L2(fp,i))
		    Memr[FP_V2(fp,i)] = 0.
	    }
	    return (NULL)
	}

	data = imgl2r (im, line)
	do i = 1, FP_NCOLS(fp) {
	    if (line == FP_L1(fp,i))
		Memr[FP_V1(fp,i)] = Memr[data+FP_COL(fp,i)-1]
	    else if (line == FP_L2(fp,i))
		Memr[FP_V2(fp,i)] = Memr[data+FP_COL(fp,i)-1]
	}

	return (data)
end



# XT_FP -- Get the specified line of image data and replace bad pixels by
# interpolation.

pointer procedure xt_fpd (fp, im, line, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

pointer	imgl2d(), xt_fpsd()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2d (im, line))

	col1 = 1
	col2 = IM_LEN(im,1)
	line1 = 1
	line2 = IM_LEN(im,2)

	return (xt_fpsd (fp, im, line, col1, col2, line1, line2, fd))
end


# XT_FXS -- Get the specified line of image data and replace bad pixels by
# interpolation within a specified section.

pointer procedure xt_fpsd (fp, im, line, col1, col2, line1, line2, fd)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line
int	fd			#I File descriptor for pixel list

int	col1, col2		#I Section of interest
int	line1, line2		#I Section of interest

int	i, j, nc, nl, ncols, c1, c2, l1, l2, l3, l4
long	v[IM_MAXDIM]
double	a, b, c, d, val
double	indef
pointer	pm, data, bp

bool	pm_linenotempty()
pointer	imgl2d(), xt_fpvald()

begin
	# If there are no bad pixels just get the image line and return.
	if (fp == NULL)
	    return (imgl2d (im, line))

	# Initialize
	pm = FP_PM(fp)
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	ncols = FP_NCOLS(fp)
	call amovkl (long(1), v, IM_MAXDIM)
	v[2] = line

	# If there might be column interpolation initialize value arrays.
	if (ncols > 0 && FP_PV1(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_DOUBLE
	    call malloc (FP_PV1(fp), ncols, FP_PIXTYPE(fp))
	    call malloc (FP_PV2(fp), ncols, FP_PIXTYPE(fp))
	    indef = INDEFD
	    call amovkd (indef, Memd[FP_V1(fp,1)], ncols)
	    call amovkd (indef, Memd[FP_V2(fp,1)], ncols)
	}

	# If there are no bad pixels in the line and the line contains
	# no column interpolation endpoints return the data directly.
	# Otherwise get the line and fill in any endpoints that may
	# be used later.

	if (!pm_linenotempty (pm, v)) {
	    if (line < FP_LMIN(fp) || line > FP_LMAX(fp))
		return (imgl2d (im, line))
	    else
		return (xt_fpvald (fp, im, line))
	}

	# Get the pixel mask.
	call malloc (bp, nc, TY_SHORT)
	call pmglps (pm, v, Mems[bp], 0, nc, PIX_SRC)
	bp = bp - 1

	# Check if any column interpolation endpoints are needed and
	# set them.  Set any other endpoints on the same lines at
	# the same time.

	if (line >= FP_LMIN(fp) && line < FP_LMAX(fp)) {
	    j = 1
	    do i = col1, col2 {
		if (Mems[bp+i] == FP_CVAL(fp)) {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			if (line>FP_L1(fp,j) && line<FP_L2(fp,j)) {
			    if (IS_INDEFD(Memd[FP_V1(fp,j)]))
				data = xt_fpvald (fp, im, FP_L1(fp,j))
			    if (IS_INDEFD(Memd[FP_V2(fp,j)]))
				data = xt_fpvald (fp, im, FP_L2(fp,j))
			}
		    }
		}
	    }
	}

	# Fix pixels by column or line interpolation.
	if (FP_DATA(fp) == NULL) {
	    FP_PIXTYPE(fp) = TY_DOUBLE
	    call malloc (FP_DATA(fp), nc, FP_PIXTYPE(fp))
	}
	data = FP_DATA(fp)
	call amovd (Memd[xt_fpvald(fp,im,line)], Memd[data], nc)
	j = 1
	for (c1=col1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
	    ;
	while (c1 <= col2) {
	    c1 = c1 - 1
	    for (c2=c1+2; c2<=col2 && Mems[bp+c2]!=0; c2=c2+1)
		;
	    a = INDEFD
	    do i = c1+1, c2-1 {
		if (Mems[bp+i] == FP_LVAL(fp)) {
		    if (IS_INDEFD(a)) {
			if (c1 < col1 && c2 > col2) {
			    c1 = c2 + 1
			    next
			}
			if (c1 >= col1)
			    a = Memd[data+c1-1]
			else
			    a = Memd[data+c2-1]
			if (c2 <= col2)
			    b = (Memd[data+c2-1] - a) / (c2 - c1)
			else
			    b = 0.
		    }
		    val = a + b * (i - c1)
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargd (Memd[data+i-1])
			    call pargd (val)
			if (c1 >= col1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c1)
			    call pargi (line)
			}
			if (c2 <= col2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (c2)
			    call pargi (line)
			}
			call fprintf (fd, "\n")
		    }
		} else {
		    for (; j<=ncols && FP_COL(fp,j)!=i; j=j+1)
			;
		    for (; j<=ncols && FP_COL(fp,j)==i; j=j+1) {
			l1 = FP_L1(fp,j)
			l2 = FP_L2(fp,j)
			if (l1 < line1 && l2 > line2)
			    next
			if (line > l1 && line < l2) {
			    if (l1 >= line1)
				c = Memd[FP_V1(fp,j)]
			    else
				c = Memd[FP_V2(fp,j)]
			    if (l2 <= line2) {
				d = (Memd[FP_V2(fp,j)] - c) / (l2 - l1)
				val = c + d * (line - l1)
			    } else
				val = c
			    l3 = l1
			    l4 = l2
			}
		    }
		    if (fd != NULL) {
			call fprintf (fd, "%4d %4d %8g %8g")
			    call pargi (i)
			    call pargi (line)
			    call pargd (Memd[data+i-1])
			    call pargd (val)
			if (l1 >= line1) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l3)
			}
			if (l2 <= line2) {
			    call fprintf (fd, " %4d %4d")
			    call pargi (i)
			    call pargi (l4)
			}
			call fprintf (fd, "\n")
		    }
		}
		Memd[data+i-1] = val
	    }
	    for (c1=c2+1; c1<=col2 && Mems[bp+c1]==0; c1=c1+1)
		;
	}

	call mfree (bp, TY_SHORT)
	return (data)
end


# XT_FPVAL -- Get data for the specified line and set the values for
# all column interpolation endpoints which occur at that line.

pointer procedure xt_fpvald (fp, im, line)

pointer	fp			#I FIXPIX pointer
pointer	im			#I Image pointer
int	line			#I Line

int	i
pointer	data, imgl2d()

begin
	# Set out of bounds values to 0.  These are not used but we need
	# to cancel the INDEF values.
	if (line < 1 || line > IM_LEN(im,2)) {
	    do i = 1, FP_NCOLS(fp) {
		if (line == FP_L1(fp,i))
		    Memd[FP_V1(fp,i)] = 0.
		else if (line == FP_L2(fp,i))
		    Memd[FP_V2(fp,i)] = 0.
	    }
	    return (NULL)
	}

	data = imgl2d (im, line)
	do i = 1, FP_NCOLS(fp) {
	    if (line == FP_L1(fp,i))
		Memd[FP_V1(fp,i)] = Memd[data+FP_COL(fp,i)-1]
	    else if (line == FP_L2(fp,i))
		Memd[FP_V2(fp,i)] = Memd[data+FP_COL(fp,i)-1]
	}

	return (data)
end


