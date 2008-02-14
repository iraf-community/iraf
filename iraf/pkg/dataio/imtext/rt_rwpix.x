# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include <ctype.h>
include "imtext.h"

# RT_RINIT -- Initialize buffer and buffer pointer for reading text.  

procedure rt_rinit ()

int	ip
char	text_buf[SZ_LINE]
common	/rpix_init/ ip, text_buf

begin
	ip = 1
	text_buf[1] = EOS
end


# RT_OUTPUT_LINEL -- Put line of long pixels to image from text file.

procedure rt_output_linel (tf, format, bufptr, npix)

int	tf		# File descriptor for input text file
int	format		# Format of pixels in text file (integer/ floating)
pointer	bufptr		# Pointer to image line to be filled
int	npix		# Number of pixels per image line

pointer	sp, dbl_buf, cplx_buf
errchk	rt_ripixels, rt_rfpixels, rt_rcpixels

begin
	call smark (sp)

	switch (format) {
	case INT_FORM:
		call salloc (dbl_buf, npix, TY_DOUBLE)
		call rt_ripixels (tf, Memd[dbl_buf], npix)
		call achtdl (Memd[dbl_buf], Meml[bufptr], npix)
	case FP_FORM:
		call salloc (dbl_buf, npix, TY_DOUBLE)
		call rt_rfpixels (tf, Memd[dbl_buf], npix)
		call achtdl (Memd[dbl_buf], Meml[bufptr], npix)
	case CPX_FORM:
		call salloc (cplx_buf, npix, TY_COMPLEX)
		call rt_rcpixels (tf, Memx[cplx_buf], npix)
		call achtxl (Memx[cplx_buf], Meml[bufptr], npix)
	}

	call sfree (sp)
end


# RT_OUTPUT_LINED -- Put line of double pixels to image from text file.

procedure rt_output_lined (tf, format, bufptr, npix)

int	tf		# File descriptor for input text file
int	format		# Format of pixels in text file (integer/ floating)
pointer	bufptr		# Pointer to image line to be filled
int	npix		# Number of pixels per image line

pointer	sp, cplx_buf
errchk	rt_ripixels, rt_rfpixels, rt_rcpixels

begin
	call smark (sp)

	switch (format) {
	case INT_FORM:
		call rt_ripixels (tf, Memd[bufptr], npix)
	case FP_FORM:
		call rt_rfpixels (tf, Memd[bufptr], npix)
	case CPX_FORM:
		call salloc (cplx_buf, npix, TY_COMPLEX)
		call rt_rcpixels (tf, Memx[cplx_buf], npix)
		call achtxd (Memx[cplx_buf], Memd[bufptr], npix)
	}

	call sfree (sp)
end


# RT_OUTPUT_LINEX -- Put line of complex pixels to image from text file.

procedure rt_output_linex (tf, format, bufptr, npix)

int	tf		# File descriptor for input text file
int	format		# Format of pixels in text file (integer/ floating)
pointer	bufptr		# Pointer to image line to be filled
int	npix		# Number of pixels per image line

pointer	sp, dbl_buf
errchk	rt_ripixels, rt_rfpixels, rt_rcpixels

begin
	call smark (sp)

	switch (format) {
	case INT_FORM:
	    call salloc (dbl_buf, npix, TY_DOUBLE)
	    call rt_ripixels (tf, Memd[dbl_buf], npix)
	    call achtdx (Memd[dbl_buf], Memx[bufptr], npix)
	case FP_FORM:
	    call salloc (dbl_buf, npix, TY_DOUBLE)
	    call rt_rfpixels (tf, Memd[dbl_buf], npix)
	    call achtdx (Memd[dbl_buf], Memx[bufptr], npix)
	case CPX_FORM:
	    call rt_rcpixels (tf, Memx[bufptr], npix)
	}

	call sfree (sp)
end


# RT_RIPIXELS -- read integer pixels free format from text file into a 
# type double real buffer.

procedure rt_ripixels (tf, dbl_out, npix)

int	tf		# File descriptor for input text file
double	dbl_out[ARB]	# Output pixel array
int	npix		# Number of pixels to output

bool	neg
int	i, sum, ip_start, ip
char	text_buf[SZ_LINE]
common	/rpix_init/ ip, text_buf
int	getline()
errchk	getline

begin
	# Read values until satisfied
	for (i=0;  i < npix;  ) {
	    sum = 0

	    # Position to first non white space character
	    while (IS_WHITE (text_buf[ip]))
	        ip = ip + 1
	    ip_start = ip

	    neg = (text_buf[ip] == '-')
	    if (neg)
	        ip = ip + 1

	    while (IS_DIGIT (text_buf[ip])) {
		sum = sum * 10 + TO_INTEG (text_buf[ip])
		ip = ip + 1
	    }

	    if (ip == ip_start) {
	        if (getline (tf, text_buf) == EOF) {
		    call eprintf ("Premature EOF seen by rt_ripixels\n")
		    break
		}
	        ip = 1

	    } else {
		i = i + 1
		if (neg)
		    dbl_out[i] = double (-sum)
		else
		    dbl_out[i] = double ( sum)
	    }
	} 
end


# RT_RFPIXELS -- read floating point pixels free format from text file into a 
# double floating point buffer. 

procedure rt_rfpixels (tf, dbl_out, npix)

int	tf		# File descriptor for text file
double	dbl_out[npix]	# Output pixel buffer
int	npix		# Number of pixels to output

int	i, nchars
double	dval
int	gctod(), getline()

int	ip
char	text_buf[SZ_LINE]
common	/rpix_init/ ip, text_buf
errchk	gctod, getline

begin
	# Read values until satisfied
	for (i=0;  i < npix;  ) {
	    nchars = gctod (text_buf, ip, dval)

	    if (nchars == 0) {
	        if (getline (tf, text_buf) == EOF) {
		    call eprintf ("Premature EOF seen in rt_rfpixels\n")
		    break
		}
	        ip = 1

	    } else {
		i = i + 1
		dbl_out[i] = dval
	    }
	}
end


# RT_RCPIXELS -- read complex pixels free format from text file into a 
# complex floating point buffer. 

procedure rt_rcpixels (tf, cplx_out, npix)

int	tf		# File descriptor for text file
complex cplx_out[npix]  # Output pixel buffer
int	npix		# Number of pixels to output

int	i, nchars
complex	xval
int	gctox(), getline()

int	ip
char	text_buf[SZ_LINE]
common	/rpix_init/ ip, text_buf
errchk	gctox, getline

begin
	# Read values until satisfied
	for (i=0;  i < npix;  ) {
	    nchars = gctox (text_buf, ip, xval)

	    if (nchars == 0) {
	        if (getline (tf, text_buf) == EOF) {
		    call eprintf ("Premature EOF seen in rt_rcpixels\n")
		    break
		}
	        ip = 1

	    } else {
		i = i + 1
		cplx_out[i] = xval
	    }
	}
end


# RT_SKIP_LINES -- Skip lines of text file.

int procedure rt_skip_lines (tf, nskip)

int	tf		# File descriptor of text file
int	nskip		# Number of lines to skip

pointer	sp, buffer
int	i
int	fscan()

begin
	call smark (sp)
	call salloc (buffer, SZ_LINE, TY_CHAR)

	for (i = 1; i <= nskip; i = i + 1) {
	    if (fscan (tf) == EOF) {
		call sfree (sp)
		return (EOF)
	    } else
		call gargstr (Memc[buffer], SZ_LINE)
	}

	call sfree (sp)
	return (OK)
end
