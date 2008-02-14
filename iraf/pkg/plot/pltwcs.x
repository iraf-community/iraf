# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mwset.h>

# PLT_WCS -- Return WCS coordinate vector and label for the specified
# logical axis of an image.  The transform pointer must be from logical
# coordinates to the desired final coordinates and opened with all
# axes selected.  This routine supplies the appropriate image section
# element in the case of dimensional reduction and the specified value
# for the other image axes when evaluating coordinates.  The label
# string is input with the system name in order to generate an appropriate
# label.

procedure plt_wcs (im, mw, ct, axis, axvals, x1, x2, x, npts, label, format,
	maxchar)

pointer	im			# image descriptor
pointer	mw			# mwcs descriptor
pointer	ct			# coordinate descriptor
int	axis			# logical axis
real	axvals[ARB]		# axis values for nonselected logical axes
real	x1			# starting logical pixel coordinate
real	x2			# ending logical pixel coordinate
real	x[ARB]			# output vector
int	npts			# number of points
char	label[ARB]		# input system label, output coordinate label
char	format[ARB]		# output coordinate format
int	maxchar			# maximum characters in label and format

int	i, j, wcsdim, paxis, mw_stati()
real	dx
pointer	sp, axno, axval, xin, xout, str1, str2
bool	streq()
errchk	mw_gwattrs

begin
	call smark (sp)
	call salloc (axno, IM_MAXDIM, TY_INT)
	call salloc (axval, IM_MAXDIM, TY_INT)
	call salloc (xin, IM_MAXDIM, TY_REAL)
	call salloc (xout, IM_MAXDIM, TY_REAL)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	call mw_seti (mw, MW_USEAXMAP, NO)
	wcsdim = mw_stati (mw, MW_NDIM)
	call mw_gaxmap (mw, Memi[axno], Memi[axval], wcsdim)

	paxis = 0
	do i = 1, wcsdim {
	    j = Memi[axno+i-1]
	    if (j == axis)
		paxis = i
	    else if (j == 0)
		Memr[xin+i-1] = 1
	    else
		Memr[xin+i-1] = axvals[j]
	}
	if (paxis == 0) {		# Bug workaround
	    paxis = 1
	    do i = 1, wcsdim {
		j = i
		if (j == axis)
		    paxis = i
		else if (j == 0)
		    Memr[xin+i-1] = 1
		else
		    Memr[xin+i-1] = axvals[j]
	    }
	}

	if (npts > 1)
	    dx = (x2 - x1) / (npts - 1)

	do i = 1, npts {
	    Memr[xin+paxis-1] = x1 + (i - 1) * dx
	    call mw_ctranr (ct, Memr[xin], Memr[xout], wcsdim)
	    x[i] = Memr[xout+paxis-1]
	}

	# Set coordinate label
	format[1] = EOS
	if (streq (label, "logical")) {
	    if (axis == 1)
		call strcpy ("Column (pixels)", label, maxchar)
	    else if (axis == 2)
		call strcpy ("Line (pixels)", label, maxchar)
	    else
		call strcpy ("Pixels", label, maxchar)
	} else if (streq (label, "physical")) {
	    if (paxis == 1)
		call strcpy ("Column (pixels)", label, maxchar)
	    else if (paxis == 2)
		call strcpy ("Line (pixels)", label, maxchar)
	    else
		call strcpy ("Pixels", label, maxchar)
	} else {
	    label[1] = EOS
	    ifnoerr (call mw_gwattrs (mw,paxis,"label",Memc[str1],SZ_LINE)) {
		ifnoerr (call mw_gwattrs (mw, paxis, "units", Memc[str2],
		    SZ_LINE)) {
		    call sprintf (label, maxchar, "%s (%s)")
			call pargstr (Memc[str1])
			call pargstr (Memc[str2])
		} else {
		    call sprintf (label, maxchar, "%s")
			call pargstr (Memc[str1])
		}
	    }

	    ifnoerr (call mw_gwattrs (mw,paxis,"format",Memc[str1],SZ_LINE))
		call strcpy (Memc[str1], format, maxchar)
	}

	call sfree (sp)
end


# PLT_WCSCOORD -- Return 2D WCS coordinate

procedure plt_wcscoord (im, mw, ct, wcs, format, col, line, value, str, maxchar)

pointer	im			# image descriptor
pointer	mw			# mwcs descriptor
pointer	ct			# coordinate descriptor
char	wcs[ARB]		# WCS type
char	format[ARB]		# default format
int	col			# logical column
int	line			# logical line
real	value			# pixel value
char	str[maxchar]		# coordinate string
int	maxchar			# maximum length of coordinate string

int	i, j, k, wcsdim, mw_stati()
pointer	sp, axno, axval, axis, xin, xout, fmt, temp
bool	streq()
errchk	mw_gwattrs

begin
	if (streq (wcs, "logical")) {
	    call sprintf (str, maxchar, "pixel=[%d,%d] value=%g\n")
		call pargi (col)
		call pargi (line)
		call pargr (value)
	    return
	}

	call smark (sp)
	call salloc (axno, IM_MAXDIM, TY_INT)
	call salloc (axval, IM_MAXDIM, TY_INT)
	call salloc (axis, IM_MAXDIM, TY_INT)
	call salloc (xin, IM_MAXDIM, TY_REAL)
	call salloc (xout, IM_MAXDIM, TY_REAL)
	call salloc (fmt, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call aclri (Memi[axis], IM_MAXDIM)

	# Map the logical to physical coordinates
	call mw_seti (mw, MW_USEAXMAP, NO)
	wcsdim = mw_stati (mw, MW_NDIM)
	call mw_gaxmap (mw, Memi[axno], Memi[axval], wcsdim)

	k = 0
	do i = 1, wcsdim {
	    j = Memi[axno+i-1]
	    if (j != 0) {
		Memi[axis+k] = i
		k = k + 1
	    }
	    if (j == 1)
		Memr[xin+i-1] = col
	    else if (j == 2)
		Memr[xin+i-1] = line
	    else 
		Memr[xin+i-1] = 1
	}
	if (k == 0) {		# Bug workaround
	    do i = 1, wcsdim {
		 Memi[axno+i-1] = i
		j = Memi[axno+i-1]
		if (j != 0) {
		    Memi[axis+k] = i
		    k = k + 1
		}
		if (j == 1)
		    Memr[xin+i-1] = col
		else if (j == 2)
		    Memr[xin+i-1] = line
		else 
		    Memr[xin+i-1] = 1
	    }
	}

	# Do the coordinate transform
	call mw_ctranr (ct, Memr[xin], Memr[xout], wcsdim)

	# Print the coordinates and data value using appropriate formats
	j = Memi[axis]
	k = Memi[axis+1]
	if (k == 0)
	    k = mod (j, 2) + 1
	i = min (j, k)
	j = max (j, k)
	if (streq (wcs, "physical")) {
	    call sprintf (str, maxchar,
		"pixel=[%d,%d], physical=[%d,%d], value=%g\n")
		call pargi (col)
		call pargi (line)
		call pargi (nint (Memr[xout+i-1]))
		call pargi (nint (Memr[xout+j-1]))
		call pargr (value)
	} else {
	    call sprintf (str, maxchar, "pixel=[%d,%d], world=[")
		call pargi (col)
		call pargi (line)

	    call strcpy (format, Memc[fmt], SZ_FNAME)
	    if (Memc[fmt] == EOS)
		iferr (call mw_gwattrs (mw,i,"format",Memc[fmt],SZ_FNAME))
		    call strcpy ("%g", Memc[fmt], SZ_FNAME)
	    call sprintf (Memc[temp], SZ_FNAME, Memc[fmt])
		call pargr (Memr[xout+i-1])
	    call strcat (Memc[temp], str, maxchar)
	    call strcat (",", str, maxchar)
	    call strcpy (format, Memc[fmt], SZ_FNAME)
	    if (Memc[fmt] == EOS)
		iferr (call mw_gwattrs (mw,j,"format",Memc[fmt],SZ_FNAME))
		    call strcpy ("%g", Memc[fmt], SZ_FNAME)
	    call sprintf (Memc[temp], SZ_FNAME, Memc[fmt])
		call pargr (Memr[xout+j-1])
	    call strcat (Memc[temp], str, maxchar)

	    call sprintf (Memc[temp], SZ_FNAME, "] value=%g\n")
		call pargr (value)
	    call strcat (Memc[temp], str, maxchar)
	}

	call sfree (sp)
end


# PLT_IFORMATR -- Determine the inverse formatted real value
# This temporary routine is used to account for scaling of the H and M formats.

real procedure plt_iformatr (value, format)

real	value			# Value to be inverse formated
char	format[ARB]		# Format

int	strldxs()

begin
	if (strldxs ("HM", format) > 0)
	    return (value * 15.)
	else
	    return (value)
end
