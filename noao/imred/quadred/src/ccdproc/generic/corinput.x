include	<imhdr.h>
include	"ccdred.h"


# CORINPUT -- Get an input image line, fix the bad pixels, and trim.
# Return the corrected input line in the output array.

procedure corinputs (in, line, ccd, output, ncols)

pointer	in		# Input IMIO pointer
int	line		# Corrected output line
pointer	ccd		# CCD pointer
short	output[ncols]	# Output data (returned)
int	ncols		# Number of output columns

int	i, inline
pointer	inbuf, imgl2s()

begin
	# Determine the input line in terms of the trimmed output line.
	if (IN_SEC(ccd) == NULL)
	    inline = IN_L1(ccd) + line - 1
	else {
	    do i = 1, IN_NSEC(ccd) {
		if (line < OUT_SL1(ccd,i) || line > OUT_SL2(ccd,i))
		    next
		inline = IN_SL1(ccd,i) + line - OUT_SL1(ccd,i)
		break
	    }
	}

	# If there are bad lines call a procedure to fix them.  Otherwise
	# read the image line directly.

	if (NBADLINES(ccd) != 0)
	    call lfixs (in, inline, Mems[BADLINES(ccd)], IM_LEN(in,1),
		IM_LEN(in,2), NBADLINES(ccd), inbuf)
	else
	    inbuf = imgl2s (in, inline)

	# IF there are bad columns call a procedure to fix them.
	if (NBADCOLS(ccd) != 0)
	    call cfixs (inline, Mems[BADCOLS(ccd)], IM_LEN(in,1),
		IM_LEN(in,2), NBADCOLS(ccd), Mems[inbuf])

	# Move the pixels to the output line.
	if (IN_SEC(ccd) == NULL)
	    call amovs (Mems[inbuf+IN_C1(ccd)-OUT_C1(ccd)], output, ncols)
	else {
	    do i = 1, IN_NSEC(ccd) {
		if (inline < IN_SL1(ccd,i) || inline > IN_SL2(ccd,i))
		    next
		call amovs (Mems[inbuf+IN_SC1(ccd,i)-OUT_C1(ccd)],
		    output[OUT_SC1(ccd,i)], OUT_SC2(ccd,i)-OUT_SC1(ccd,i)+1)
	    }
	}
end


# CFIX -- Interpolate across bad columns defined in the bad column array.

procedure cfixs (line, badcols, ncols, nlines, nbadcols, data)

int	line				# Line to be fixed
short	badcols[2, nlines, nbadcols]	# Bad column array
int	ncols				# Number of columns
int	nlines				# Number of lines
int	nbadcols			# Number of bad column regions
short	data[ncols]			# Data to be fixed

short	val
real	del
int	i, j, col1, col2

begin
	do i = 1, nbadcols {
	    col1 = badcols[1, line, i]
	    if (col1 == 0)			# No bad columns
		return
	    col2 = badcols[2, line, i]
	    if (col1 == 1) {			# Bad first column
		val = data[col2+1]
		do j = col1, col2
		    data[j] = val
	    } else if (col2 == ncols) {		# Bad last column
		val = data[col1-1]
		do j = col1, col2
		    data[j] = val
	    } else {				# Interpolate
		del = (data[col2+1] - data[col1-1]) / (col2 - col1 + 2)
		val = data[col1-1] + del
		do j = col1, col2
		    data[j] = val + (j - col1) * del
	    }
	}
end


# LFIX -- Get image line and replace bad pixels by interpolation from
# neighboring lines.  Internal buffers are used to keep the last fixed
# line and the next good line.  They are allocated with LFIXINIT and
# freed with LFIXFREE.

procedure lfixs (im, line, badlines, ncols, nlines, nbadlines, data)

pointer	im				# IMIO pointer
int	line				# Line to be obtained and fixed
short	badlines[2,nlines,nbadlines]	# Bad line region array
int	ncols				# Number of columns in image
int	nlines				# Number of lines in images
int	nbadlines			# Number of bad line regions
pointer	data				# Data line pointer (returned)

real	wt1, wt2
int	i, nextgood, lastgood, col1, col2
pointer	imgl2s()

pointer	lastbuf, nextbuf
common	/lfixcom/ lastbuf, nextbuf, lastgood

begin
	# If this line has bad pixels replace them.  Otherwise just
	# read the line.

	if (badlines[1, line, 1] != 0) {
	    # Save the last line which has already been fixed.
	    if (line != 1)
		call amovs (Mems[data], Mems[lastbuf], ncols)

	    # Determine the next line with no bad line pixels.  Note that
	    # this requirement is overly strict since the bad columns
	    # may not be the same in neighboring lines.

	    nextgood = 0
	    do i = line+1, nlines {
		if (badlines[1, i, 1] == 0) {
		    nextgood = i
		    break
		}
	    }

	    # If the next good line is not the same as previously
	    # read the data line and store it in a buffer.

	    if ((nextgood != lastgood) && (nextgood != 0)) {
		data = imgl2s (im, nextgood)
		call amovs (Mems[data], Mems[nextbuf], ncols)
		lastgood = nextgood
	    }

	    # Get the data line.
	    data = imgl2s (im, line)

	    # Interpolate the bad columns.  At the ends of the image use
	    # extension otherwise use linear interpolation.

	    if (line == 1) {				# First line is bad
		do i = 1, nbadlines {
		    col1 = badlines[1,line,i] - 1
		    if (col1 == -1)
			break
		    col2 = badlines[2,line,i]
		    call amovs (Mems[nextbuf+col1], Mems[data+col1],
			col2-col1)
		}
	    } else if (nextgood == 0) {			# Last line is bad
		do i = 1, nbadlines {
		    col1 = badlines[1,line,i] - 1
		    if (col1 == -1)
			break
		    col2 = badlines[2,line,i]
		    call amovs (Mems[lastbuf+col1], Mems[data+col1],
			col2-col1)
		}
	    } else {					# Interpolate
		wt1 = 1. / (nextgood - line + 1)
		wt2 = 1. - wt1
		do i = 1, nbadlines {
		    col1 = badlines[1,line,i] - 1
		    if (col1 == -1)
			break
		    col2 = badlines[2,line,i] - 1
		    call awsus (Mems[nextbuf+col1], Mems[lastbuf+col1],
			Mems[data+col1], col2-col1+1, wt1, wt2)
		}
	    }
	} else
	    data = imgl2s (im, line)
end


# LFIXINIT -- Allocate internal buffers.

procedure lfixinits (im)

pointer	im		# IMIO pointer

int	lastgood
pointer	lastbuf, nextbuf
common	/lfixcom/ lastbuf, nextbuf, lastgood

begin
	call malloc (lastbuf, IM_LEN(im,1), TY_SHORT)
	call malloc (nextbuf, IM_LEN(im,1), TY_SHORT)
	lastgood=0
end

# LFIXFREE -- Free memory when the last line has been obtained.

procedure lfixfrees ()

int	lastgood
pointer	lastbuf, nextbuf
common	/lfixcom/ lastbuf, nextbuf, lastgood

begin
	call mfree (lastbuf, TY_SHORT)
	call mfree (nextbuf, TY_SHORT)
end

# CORINPUT -- Get an input image line, fix the bad pixels, and trim.
# Return the corrected input line in the output array.

procedure corinputr (in, line, ccd, output, ncols)

pointer	in		# Input IMIO pointer
int	line		# Corrected output line
pointer	ccd		# CCD pointer
real	output[ncols]	# Output data (returned)
int	ncols		# Number of output columns

int	i, inline
pointer	inbuf, imgl2r()

begin
	# Determine the input line in terms of the trimmed output line.
	if (IN_SEC(ccd) == NULL)
	    inline = IN_L1(ccd) + line - 1
	else {
	    do i = 1, IN_NSEC(ccd) {
		if (line < OUT_SL1(ccd,i) || line > OUT_SL2(ccd,i))
		    next
		inline = IN_SL1(ccd,i) + line - OUT_SL1(ccd,i)
		break
	    }
	}

	# If there are bad lines call a procedure to fix them.  Otherwise
	# read the image line directly.

	if (NBADLINES(ccd) != 0)
	    call lfixr (in, inline, Mems[BADLINES(ccd)], IM_LEN(in,1),
		IM_LEN(in,2), NBADLINES(ccd), inbuf)
	else
	    inbuf = imgl2r (in, inline)

	# IF there are bad columns call a procedure to fix them.
	if (NBADCOLS(ccd) != 0)
	    call cfixr (inline, Mems[BADCOLS(ccd)], IM_LEN(in,1),
		IM_LEN(in,2), NBADCOLS(ccd), Memr[inbuf])

	# Move the pixels to the output line.
	if (IN_SEC(ccd) == NULL)
	    call amovr (Memr[inbuf+IN_C1(ccd)-OUT_C1(ccd)], output, ncols)
	else {
	    do i = 1, IN_NSEC(ccd) {
		if (inline < IN_SL1(ccd,i) || inline > IN_SL2(ccd,i))
		    next
		call amovr (Memr[inbuf+IN_SC1(ccd,i)-OUT_C1(ccd)],
		    output[OUT_SC1(ccd,i)], OUT_SC2(ccd,i)-OUT_SC1(ccd,i)+1)
	    }
	}
end


# CFIX -- Interpolate across bad columns defined in the bad column array.

procedure cfixr (line, badcols, ncols, nlines, nbadcols, data)

int	line				# Line to be fixed
short	badcols[2, nlines, nbadcols]	# Bad column array
int	ncols				# Number of columns
int	nlines				# Number of lines
int	nbadcols			# Number of bad column regions
real	data[ncols]			# Data to be fixed

real	val
real	del
int	i, j, col1, col2

begin
	do i = 1, nbadcols {
	    col1 = badcols[1, line, i]
	    if (col1 == 0)			# No bad columns
		return
	    col2 = badcols[2, line, i]
	    if (col1 == 1) {			# Bad first column
		val = data[col2+1]
		do j = col1, col2
		    data[j] = val
	    } else if (col2 == ncols) {		# Bad last column
		val = data[col1-1]
		do j = col1, col2
		    data[j] = val
	    } else {				# Interpolate
		del = (data[col2+1] - data[col1-1]) / (col2 - col1 + 2)
		val = data[col1-1] + del
		do j = col1, col2
		    data[j] = val + (j - col1) * del
	    }
	}
end


# LFIX -- Get image line and replace bad pixels by interpolation from
# neighboring lines.  Internal buffers are used to keep the last fixed
# line and the next good line.  They are allocated with LFIXINIT and
# freed with LFIXFREE.

procedure lfixr (im, line, badlines, ncols, nlines, nbadlines, data)

pointer	im				# IMIO pointer
int	line				# Line to be obtained and fixed
short	badlines[2,nlines,nbadlines]	# Bad line region array
int	ncols				# Number of columns in image
int	nlines				# Number of lines in images
int	nbadlines			# Number of bad line regions
pointer	data				# Data line pointer (returned)

real	wt1, wt2
int	i, nextgood, lastgood, col1, col2
pointer	imgl2r()

pointer	lastbuf, nextbuf
common	/lfixcom/ lastbuf, nextbuf, lastgood

begin
	# If this line has bad pixels replace them.  Otherwise just
	# read the line.

	if (badlines[1, line, 1] != 0) {
	    # Save the last line which has already been fixed.
	    if (line != 1)
		call amovr (Memr[data], Memr[lastbuf], ncols)

	    # Determine the next line with no bad line pixels.  Note that
	    # this requirement is overly strict since the bad columns
	    # may not be the same in neighboring lines.

	    nextgood = 0
	    do i = line+1, nlines {
		if (badlines[1, i, 1] == 0) {
		    nextgood = i
		    break
		}
	    }

	    # If the next good line is not the same as previously
	    # read the data line and store it in a buffer.

	    if ((nextgood != lastgood) && (nextgood != 0)) {
		data = imgl2r (im, nextgood)
		call amovr (Memr[data], Memr[nextbuf], ncols)
		lastgood = nextgood
	    }

	    # Get the data line.
	    data = imgl2r (im, line)

	    # Interpolate the bad columns.  At the ends of the image use
	    # extension otherwise use linear interpolation.

	    if (line == 1) {				# First line is bad
		do i = 1, nbadlines {
		    col1 = badlines[1,line,i] - 1
		    if (col1 == -1)
			break
		    col2 = badlines[2,line,i]
		    call amovr (Memr[nextbuf+col1], Memr[data+col1],
			col2-col1)
		}
	    } else if (nextgood == 0) {			# Last line is bad
		do i = 1, nbadlines {
		    col1 = badlines[1,line,i] - 1
		    if (col1 == -1)
			break
		    col2 = badlines[2,line,i]
		    call amovr (Memr[lastbuf+col1], Memr[data+col1],
			col2-col1)
		}
	    } else {					# Interpolate
		wt1 = 1. / (nextgood - line + 1)
		wt2 = 1. - wt1
		do i = 1, nbadlines {
		    col1 = badlines[1,line,i] - 1
		    if (col1 == -1)
			break
		    col2 = badlines[2,line,i] - 1
		    call awsur (Memr[nextbuf+col1], Memr[lastbuf+col1],
			Memr[data+col1], col2-col1+1, wt1, wt2)
		}
	    }
	} else
	    data = imgl2r (im, line)
end


# LFIXINIT -- Allocate internal buffers.

procedure lfixinitr (im)

pointer	im		# IMIO pointer

int	lastgood
pointer	lastbuf, nextbuf
common	/lfixcom/ lastbuf, nextbuf, lastgood

begin
	call malloc (lastbuf, IM_LEN(im,1), TY_REAL)
	call malloc (nextbuf, IM_LEN(im,1), TY_REAL)
	lastgood=0
end

# LFIXFREE -- Free memory when the last line has been obtained.

procedure lfixfreer ()

int	lastgood
pointer	lastbuf, nextbuf
common	/lfixcom/ lastbuf, nextbuf, lastgood

begin
	call mfree (lastbuf, TY_REAL)
	call mfree (nextbuf, TY_REAL)
end

