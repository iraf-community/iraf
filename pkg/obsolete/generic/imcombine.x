# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<error.h>
include	<syserr.h>

define	SUM		1	# Sum of the images
define	AVERAGE		2	# Average of the images
define	MEDIAN		3	# Median of the images
define	MINREJECT	4	# Reject minimum
define	MAXREJECT	5	# Reject maximin
define	MINMAXREJECT	6	# Reject minimum and maximum
# 	newline		7
define	THRESHOLD	8	# Absolute threshold clip
define	SIGCLIP		9	# Sigma clip using sigma at each point
define	AVSIGCLIP	10	# Sigma clip using average sigma

# IMCOMBINE -- Combine images
#
# The memory and open file descriptor limits are checked and an attempt
# to recover is made either by setting the image pixel files to be
# closed after I/O or by notifying the calling program that memory
# ran out and the IMIO buffer size should be reduced.  After the checks
# a procedure for the selected combine option is called.
# Because there may be several failure modes when reaching the file
# limits we first assume an error is due to the file limit, except for
# out of memory, and close some pixel files.  If the error then repeats
# on accessing the pixels the error is passed back.


procedure imcombines (log, in, nimages, out, sig, bufsize, option)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
pointer	sig			# Sigma IMIO pointer
int	bufsize			# IMIO buffer size
int	option			# Combine option

char	str[1]
int	i, j, fd, stropen(), errcode(), imstati()
pointer	data, imgl1s()
pointer	impl1r()

begin
	# Reserve FD for string operations.
	fd = stropen (str, 1, NEW_FILE)

	# Do I/O to the output images.
	i = IM_LEN(out, 1)
	call imseti (out, IM_BUFSIZE, bufsize)
	data = impl1r (out)
	call aclrr (Memr[data], i)
	if (sig != NULL) {
	    data = impl1r (sig)
	    call aclrr (Memr[data], i)
	}

	# Do I/O from the input images.
	do i = 1, nimages {
	    call imseti (in[i], IM_BUFSIZE, bufsize)
	    iferr (data = imgl1s (in[i])) {
		switch (errcode()) {
	        case SYS_MFULL:
		    call strclose (fd)
		    call erract (EA_ERROR)
		default:
		    if (imstati (in[i], IM_CLOSEFD) == YES) {
			call strclose (fd)
		        call error (1, "imcombine - Too many images to combine")
		    }
		    do j = i-2, nimages
		        call imseti (in[j], IM_CLOSEFD, YES)
	            data = imgl1s (in[i])
		}
	    }
	}

	call strclose (fd)

	switch (option) {
	case SUM:
	    call imc_sums (log, in, out, nimages)
	case AVERAGE:
	    call imc_averages (log, in, out, sig, nimages)
	case MEDIAN:
	    call imc_medians (log, in, out, sig, nimages)
	case SIGCLIP:
	    call imc_sigclips (log, in, out, sig, nimages)
	case AVSIGCLIP:
	    call imc_asigclips (log, in, out, sig, nimages)
	case THRESHOLD:
	    call imc_thresholds (log, in, out, sig, nimages)
	case MINREJECT:
	    call imc_minrejs (log, in, out, sig, nimages)
	case MAXREJECT:
	    call imc_maxrejs (log, in, out, sig, nimages)
	case MINMAXREJECT:
	    call imc_mmrejs (log, in, out, sig, nimages)
	}
end

procedure imcombinei (log, in, nimages, out, sig, bufsize, option)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
pointer	sig			# Sigma IMIO pointer
int	bufsize			# IMIO buffer size
int	option			# Combine option

char	str[1]
int	i, j, fd, stropen(), errcode(), imstati()
pointer	data, imgl1i()
pointer	impl1r()

begin
	# Reserve FD for string operations.
	fd = stropen (str, 1, NEW_FILE)

	# Do I/O to the output images.
	i = IM_LEN(out, 1)
	call imseti (out, IM_BUFSIZE, bufsize)
	data = impl1r (out)
	call aclrr (Memr[data], i)
	if (sig != NULL) {
	    data = impl1r (sig)
	    call aclrr (Memr[data], i)
	}

	# Do I/O from the input images.
	do i = 1, nimages {
	    call imseti (in[i], IM_BUFSIZE, bufsize)
	    iferr (data = imgl1i (in[i])) {
		switch (errcode()) {
	        case SYS_MFULL:
		    call strclose (fd)
		    call erract (EA_ERROR)
		default:
		    if (imstati (in[i], IM_CLOSEFD) == YES) {
			call strclose (fd)
		        call error (1, "imcombine - Too many images to combine")
		    }
		    do j = i-2, nimages
		        call imseti (in[j], IM_CLOSEFD, YES)
	            data = imgl1i (in[i])
		}
	    }
	}

	call strclose (fd)

	switch (option) {
	case SUM:
	    call imc_sumi (log, in, out, nimages)
	case AVERAGE:
	    call imc_averagei (log, in, out, sig, nimages)
	case MEDIAN:
	    call imc_mediani (log, in, out, sig, nimages)
	case SIGCLIP:
	    call imc_sigclipi (log, in, out, sig, nimages)
	case AVSIGCLIP:
	    call imc_asigclipi (log, in, out, sig, nimages)
	case THRESHOLD:
	    call imc_thresholdi (log, in, out, sig, nimages)
	case MINREJECT:
	    call imc_minreji (log, in, out, sig, nimages)
	case MAXREJECT:
	    call imc_maxreji (log, in, out, sig, nimages)
	case MINMAXREJECT:
	    call imc_mmreji (log, in, out, sig, nimages)
	}
end

procedure imcombinel (log, in, nimages, out, sig, bufsize, option)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
pointer	sig			# Sigma IMIO pointer
int	bufsize			# IMIO buffer size
int	option			# Combine option

char	str[1]
int	i, j, fd, stropen(), errcode(), imstati()
pointer	data, imgl1l()
pointer	impl1r()

begin
	# Reserve FD for string operations.
	fd = stropen (str, 1, NEW_FILE)

	# Do I/O to the output images.
	i = IM_LEN(out, 1)
	call imseti (out, IM_BUFSIZE, bufsize)
	data = impl1r (out)
	call aclrr (Memr[data], i)
	if (sig != NULL) {
	    data = impl1r (sig)
	    call aclrr (Memr[data], i)
	}

	# Do I/O from the input images.
	do i = 1, nimages {
	    call imseti (in[i], IM_BUFSIZE, bufsize)
	    iferr (data = imgl1l (in[i])) {
		switch (errcode()) {
	        case SYS_MFULL:
		    call strclose (fd)
		    call erract (EA_ERROR)
		default:
		    if (imstati (in[i], IM_CLOSEFD) == YES) {
			call strclose (fd)
		        call error (1, "imcombine - Too many images to combine")
		    }
		    do j = i-2, nimages
		        call imseti (in[j], IM_CLOSEFD, YES)
	            data = imgl1l (in[i])
		}
	    }
	}

	call strclose (fd)

	switch (option) {
	case SUM:
	    call imc_suml (log, in, out, nimages)
	case AVERAGE:
	    call imc_averagel (log, in, out, sig, nimages)
	case MEDIAN:
	    call imc_medianl (log, in, out, sig, nimages)
	case SIGCLIP:
	    call imc_sigclipl (log, in, out, sig, nimages)
	case AVSIGCLIP:
	    call imc_asigclipl (log, in, out, sig, nimages)
	case THRESHOLD:
	    call imc_thresholdl (log, in, out, sig, nimages)
	case MINREJECT:
	    call imc_minrejl (log, in, out, sig, nimages)
	case MAXREJECT:
	    call imc_maxrejl (log, in, out, sig, nimages)
	case MINMAXREJECT:
	    call imc_mmrejl (log, in, out, sig, nimages)
	}
end

procedure imcombiner (log, in, nimages, out, sig, bufsize, option)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
pointer	sig			# Sigma IMIO pointer
int	bufsize			# IMIO buffer size
int	option			# Combine option

char	str[1]
int	i, j, fd, stropen(), errcode(), imstati()
pointer	data, imgl1r()
pointer	impl1r()

begin
	# Reserve FD for string operations.
	fd = stropen (str, 1, NEW_FILE)

	# Do I/O to the output images.
	i = IM_LEN(out, 1)
	call imseti (out, IM_BUFSIZE, bufsize)
	data = impl1r (out)
	call aclrr (Memr[data], i)
	if (sig != NULL) {
	    data = impl1r (sig)
	    call aclrr (Memr[data], i)
	}

	# Do I/O from the input images.
	do i = 1, nimages {
	    call imseti (in[i], IM_BUFSIZE, bufsize)
	    iferr (data = imgl1r (in[i])) {
		switch (errcode()) {
	        case SYS_MFULL:
		    call strclose (fd)
		    call erract (EA_ERROR)
		default:
		    if (imstati (in[i], IM_CLOSEFD) == YES) {
			call strclose (fd)
		        call error (1, "imcombine - Too many images to combine")
		    }
		    do j = i-2, nimages
		        call imseti (in[j], IM_CLOSEFD, YES)
	            data = imgl1r (in[i])
		}
	    }
	}

	call strclose (fd)

	switch (option) {
	case SUM:
	    call imc_sumr (log, in, out, nimages)
	case AVERAGE:
	    call imc_averager (log, in, out, sig, nimages)
	case MEDIAN:
	    call imc_medianr (log, in, out, sig, nimages)
	case SIGCLIP:
	    call imc_sigclipr (log, in, out, sig, nimages)
	case AVSIGCLIP:
	    call imc_asigclipr (log, in, out, sig, nimages)
	case THRESHOLD:
	    call imc_thresholdr (log, in, out, sig, nimages)
	case MINREJECT:
	    call imc_minrejr (log, in, out, sig, nimages)
	case MAXREJECT:
	    call imc_maxrejr (log, in, out, sig, nimages)
	case MINMAXREJECT:
	    call imc_mmrejr (log, in, out, sig, nimages)
	}
end

procedure imcombined (log, in, nimages, out, sig, bufsize, option)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
pointer	sig			# Sigma IMIO pointer
int	bufsize			# IMIO buffer size
int	option			# Combine option

char	str[1]
int	i, j, fd, stropen(), errcode(), imstati()
pointer	data, imgl1d()
pointer	impl1d()

begin
	# Reserve FD for string operations.
	fd = stropen (str, 1, NEW_FILE)

	# Do I/O to the output images.
	i = IM_LEN(out, 1)
	call imseti (out, IM_BUFSIZE, bufsize)
	data = impl1d (out)
	call aclrd (Memd[data], i)
	if (sig != NULL) {
	    data = impl1d (sig)
	    call aclrd (Memd[data], i)
	}

	# Do I/O from the input images.
	do i = 1, nimages {
	    call imseti (in[i], IM_BUFSIZE, bufsize)
	    iferr (data = imgl1d (in[i])) {
		switch (errcode()) {
	        case SYS_MFULL:
		    call strclose (fd)
		    call erract (EA_ERROR)
		default:
		    if (imstati (in[i], IM_CLOSEFD) == YES) {
			call strclose (fd)
		        call error (1, "imcombine - Too many images to combine")
		    }
		    do j = i-2, nimages
		        call imseti (in[j], IM_CLOSEFD, YES)
	            data = imgl1d (in[i])
		}
	    }
	}

	call strclose (fd)

	switch (option) {
	case SUM:
	    call imc_sumd (log, in, out, nimages)
	case AVERAGE:
	    call imc_averaged (log, in, out, sig, nimages)
	case MEDIAN:
	    call imc_mediand (log, in, out, sig, nimages)
	case SIGCLIP:
	    call imc_sigclipd (log, in, out, sig, nimages)
	case AVSIGCLIP:
	    call imc_asigclipd (log, in, out, sig, nimages)
	case THRESHOLD:
	    call imc_thresholdd (log, in, out, sig, nimages)
	case MINREJECT:
	    call imc_minrejd (log, in, out, sig, nimages)
	case MAXREJECT:
	    call imc_maxrejd (log, in, out, sig, nimages)
	case MINMAXREJECT:
	    call imc_mmrejd (log, in, out, sig, nimages)
	}
end

procedure imcombinex (log, in, nimages, out, sig, bufsize, option)

int	log			# Log file descriptor
pointer	in[nimages]		# Input IMIO pointers
int	nimages			# Number of input images
pointer	out			# Output IMIO pointer
pointer	sig			# Sigma IMIO pointer
int	bufsize			# IMIO buffer size
int	option			# Combine option

char	str[1]
int	i, j, fd, stropen(), errcode(), imstati()
pointer	data, imgl1x()
pointer	impl1x()

begin
	# Reserve FD for string operations.
	fd = stropen (str, 1, NEW_FILE)

	# Do I/O to the output images.
	i = IM_LEN(out, 1)
	call imseti (out, IM_BUFSIZE, bufsize)
	data = impl1x (out)
	call aclrx (Memx[data], i)
	if (sig != NULL) {
	    data = impl1x (sig)
	    call aclrx (Memx[data], i)
	}

	# Do I/O from the input images.
	do i = 1, nimages {
	    call imseti (in[i], IM_BUFSIZE, bufsize)
	    iferr (data = imgl1x (in[i])) {
		switch (errcode()) {
	        case SYS_MFULL:
		    call strclose (fd)
		    call erract (EA_ERROR)
		default:
		    if (imstati (in[i], IM_CLOSEFD) == YES) {
			call strclose (fd)
		        call error (1, "imcombine - Too many images to combine")
		    }
		    do j = i-2, nimages
		        call imseti (in[j], IM_CLOSEFD, YES)
	            data = imgl1x (in[i])
		}
	    }
	}

	call strclose (fd)

	switch (option) {
	case SUM:
	    call imc_sumx (log, in, out, nimages)
	case AVERAGE:
	    call imc_averagex (log, in, out, sig, nimages)
	case MEDIAN:
	    call imc_medianx (log, in, out, sig, nimages)
	case THRESHOLD:
	    call imc_thresholdx (log, in, out, sig, nimages)
	case MINREJECT:
	    call imc_minrejx (log, in, out, sig, nimages)
	case MAXREJECT:
	    call imc_maxrejx (log, in, out, sig, nimages)
	case MINMAXREJECT:
	    call imc_mmrejx (log, in, out, sig, nimages)
	}
end

