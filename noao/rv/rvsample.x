include <gset.h>
include <ctype.h>
include "rvpackage.h"
include "rvflags.h"
include "rvsample.h"

# SAMP_OPEN - Open a Sample structure.

procedure samp_open (ssp)

pointer	ssp					#I Sample struct pointer

errchk	calloc

begin
	iferr (call calloc (ssp, SZ_SAMPSTRUCT, TY_STRUCT))
	    call error (0, "Error opening sample structure.")

	iferr {
	    call calloc (SR_SRANGE(ssp), MAX_SAMPLES, TY_REAL)
	    call calloc (SR_ERANGE(ssp), MAX_SAMPLES, TY_REAL)
	    call calloc (SR_NPSAMP(ssp), MAX_SAMPLES, TY_INT)
	} then
	    call error (0, "Error allocating sample structure.")
end


# SAMP_CLOSE - Close (free) a sample structure.

procedure samp_close (ssp)

pointer	ssp					#I Sample struct pointer

begin
	if (ssp == NULL)
	    return

	call mfree (SR_SRANGE(ssp), TY_REAL)
	call mfree (SR_ERANGE(ssp), TY_REAL)
	call mfree (SR_NPSAMP(ssp), TY_INT)
	SR_COUNT(ssp) = 0
	
	call mfree (ssp, TY_STRUCT)
end


# RV_LOAD_SAMPLE - Given a string in 'ranges' format, decode it and load 
# the sample structure.  Returns an ERR of OK.

int procedure rv_load_sample (ssp, s)

pointer	ssp					#I Sample struct pointer
char	s[ARB]					#I Range string to parse

pointer	sp, buf, rv
int	ip, i, j, rcount, units
int	sample_units()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	rv = SR_PARENT(ssp)
	ip = 1
	if (!IS_DIGIT(s[ip]))
	     ip = ip + 1			# Units specified

	rcount = 0
	while (s[ip] != EOS) {
	    rcount = rcount + 1
	    for (j=1; j<=2; j=j+1) { 		# Collect the numbers
	        while (IS_WHITE(s[ip])) 	# Skip leading white space
		    ip = ip + 1;	

		call aclrs (Memc[buf], SZ_LINE)
		for (i=0; (s[ip]=='.'||IS_DIGIT(s[ip]))&&i<SZ_LINE; i=i+1) { 
	            Memc[buf+i] = s[ip]
	            ip = ip + 1
		    if (s[ip] == EOS)
			break
	    	}

		call sscan (Memc[buf])
		if (j == 1) {
		    call gargr (SRANGE(ssp,rcount))
	    	    repeat {			# skip ahead to next number
		        ip = ip + 1
	    	    } until (IS_DIGIT(s[ip]) || s[ip] == EOS)

		} else if (j == 2)
		    call gargr (ERANGE(ssp,rcount))
	    }

	    if (s[ip] != EOS) {
	        repeat {			# skip ahead to delimeter
	            ip = ip + 1
	        } until (IS_DIGIT(s[ip]) || s[ip] == EOS)
	    }
	    if (s[ip] == EOS)
		break
	}

	SR_COUNT(ssp) = rcount
	if (RV_DCFLAG(rv) == -1 || RV_PIXCORR(rv) == YES) {
	    SR_UNITS(ssp) = PIXELS
	} else {
	    units = sample_units (s)
	    if (SR_UNITS(ssp) != NULL && SR_UNITS(ssp) != units) {
	        call rv_errmsg ("Range units are mixed from previous entry.")
	        call sfree (sp)
	        return (ERR)
	    } else
	        SR_UNITS(ssp) = units
	}

	if (SR_UNITS(ssp) == ERR) {
	    call rv_errmsg ("Unable to determine range units in range string.")
	    call sfree (sp)
	    return (ERR)
	}

	call sort_ranges (ssp)
	call sfree (sp)
	return (OK)
end


# RV_MAKE_RANGE_STRING - Given the struct pointer return a string in 'ranges'
# format describing the content of the range arrays.

procedure rv_make_range_string (ssp, str)

pointer	ssp				#I Sample struct pointer
char	str[SZ_LINE]			#O output range string

pointer	sp, buf
pointer	rv
int	i

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call aclrs (str, SZ_LINE)

	rv = SR_PARENT(ssp)
	if (SR_COUNT(ssp) == ALL_SPECTRUM) {
	    call strcpy ("*", str, SZ_FNAME)

	} else {
	    if (SR_UNITS(ssp) == PIXELS || RV_DCFLAG(rv) == -1 ||
		RV_PIXCORR(rv) == YES) {
		    SR_UNITS(ssp) = PIXELS
		    call strcpy ("P", str, SZ_LINE)
	    } else
		call strcpy ("A", str, SZ_LINE)
	    do i = 1, SR_COUNT(ssp) {
	       call aclrs (Memc[buf], SZ_LINE)
	       call sprintf (Memc[buf], SZ_LINE, "%-.2f-%-.2f")
	  	    call pargr (SRANGE(ssp,i))
		    call pargr (ERANGE(ssp,i))
	       call strcat (Memc[buf], str, SZ_LINE)
	       if (i != SR_COUNT(ssp))		    # No ',' after last range
		    call strcat (",", str, SZ_LINE)
	    }
	}

	call sfree (sp)
end


# RV_MARK_REGIONS - Cycle through the range list and mark each region.

procedure rv_mark_regions (ssp, gp)

pointer	ssp					#I Sample struct pointer
pointer	gp					#I GIO pointer

pointer	rv
real	left, right
int	i

double	dex()
int	gstati()

begin
	if (SR_COUNT(ssp) == ALL_SPECTRUM || gp == NULL)
	    return

	rv = SR_PARENT(ssp)
	if (gstati(gp, G_PLTYPE) != GL_CLEAR)
	   call gseti (gp, G_PLCOLOR, C_GREEN)
	do i = 1, SR_COUNT(ssp) {
	    left  = SRANGE(ssp,i)
	    right = ERANGE(ssp,i)
	    if (RV_PIXCORR(rv) == NO && RV_DCFLAG(rv) != -1 && 
		SR_UNITS(ssp) == PIXELS) {
		    left  = real (dex(SR_W0(ssp)+(left-1)*SR_WPC(ssp)))
		    right = real (dex(SR_W0(ssp)+(right-1)*SR_WPC(ssp)))
	    }

	    call gseti (gp, G_WCS, 1)
	    if (SR_IMTYPE(ssp) == REFER_SPECTRUM) 
	        call gsview (gp, 0.115, 0.95, 0.125, 0.5)
	    else 
	        call gsview (gp, 0.115, 0.95, 0.51, 0.865)
	    call mark_range (gp, left, right)
	}
	if (gstati(gp, G_PLTYPE) != GL_CLEAR)
	   call gseti (gp, G_PLCOLOR, C_FOREGROUND)
end


# RV_ERASE_REGIONS - Erase the regions drawn to the screen.

procedure rv_erase_regions (ssp, gp)

pointer	ssp					#I Sample struct pointer
pointer	gp					#I GIO pointer

begin
	if (gp == NULL)
	    return

	call gseti (gp, G_PLTYPE, GL_CLEAR)
	call gseti (gp, G_PLCOLOR, C_BACKGROUND)
	call rv_mark_regions (ssp, gp)
	call gseti (gp, G_PLTYPE, GL_SOLID) 
	call gseti (gp, G_PLCOLOR, C_FOREGROUND)
end


# APPEND_RANGE - Append another region to the range string

procedure append_range (rv, ssp, left, right)

pointer	rv					#I RV struct pointer
pointer	ssp					#I Sample struct pointer
real	left, right				#I WCS of region endpoints

pointer	gp
int	i, j, k

define	add_samp_		99

begin
	gp = RV_GP(rv)					# intializations

	# Check if we have enough room for it.
	if(SR_COUNT(ssp)+1 > MAX_SAMPLES) {
	    call rv_errmsg ("Cannot add another sample (max_samples=%d).")
		call pargi (MAX_SAMPLES)
	    return
	}

	# Convert a wavelength range to pixels if necessary.
	if (SR_UNITS(ssp) == PIXELS && RV_DCFLAG(SR_PARENT(ssp)) != -1) {
	    left = (log10(left) - SR_W0(ssp)) / SR_WPC(ssp) + 1
	    right = (log10(right) - SR_W0(ssp)) / SR_WPC(ssp) + 1
	}

	# Now figure out what to do with it.  Here we edit the existing
	# sample so they make more sense or else just add the range and
	# sort it.
	if (SR_COUNT(ssp) == ALL_SPECTRUM) {		# special case
	    SR_COUNT(ssp) = 1
	    SRANGE(ssp,1) = left
	    ERANGE(ssp,1) = right
	    if (RV_DCFLAG(rv) == -1 || RV_PIXCORR(rv) == YES)
		SR_UNITS(ssp) = PIXELS
	    else
		SR_UNITS(ssp) = LAMBDA
	    call rv_mark_regions (ssp, gp)
	    return
	}
	do i = 1, SR_COUNT(ssp) {
	    if (left >= SRANGE(ssp,i) && left <= ERANGE(ssp,i)) {
		j = 1 				# Find the right side.
		while (j <= SR_COUNT(ssp)) {
		    if (right >= SRANGE(ssp,j) && right <= ERANGE(ssp,j)) {
			if ((j-i) == 0)		# within the same sample
			    return
			else if ((j-i) == 1) {	# in the next sample
			    right = ERANGE(ssp,j)
			    call rv_erase_regions (ssp, gp)
			    call delete_samp (rv, ssp, SRANGE(ssp,j))
			    ERANGE(ssp,i) = right
			    call sort_ranges (ssp)
			    call rv_mark_regions (ssp, gp)
			    return
			} else if ((j-i) > 1) {	# skipped a few samples
			    right = ERANGE(ssp,j)
			    call rv_erase_regions (ssp, gp)
			    do k = i+1, j-1
			        call delete_samp (rv, ssp, SRANGE(ssp,k))
			    ERANGE(ssp,i) = right
			    call sort_ranges (ssp)
			    call rv_mark_regions (ssp, gp)
			    return
			}
			
		    }
		    j = j + 1
		}
		if (j == SR_COUNT(ssp)+1) {	# right is between samples
		    call rv_erase_regions (ssp, gp)
		    ERANGE(ssp,i) = right
		    call rv_mark_regions (ssp, gp)
		    return
		}
	    }
	    if (right >= SRANGE(ssp,i) && right <= ERANGE(ssp,i)) {
		call rv_erase_regions (ssp, gp)
		SRANGE(ssp,i) = left
		call rv_mark_regions (ssp, gp)
		return
	    }
	}

	# The endpoints aren't included within another region.  Let's see if 
	# they then enclose one....
	do i = 1, SR_COUNT(ssp) {
	    if (SRANGE(ssp,i) >= left && SRANGE(ssp,i) <= right) {
		j = 1
		while (j<SR_COUNT(ssp) && right>=ERANGE(ssp,j)) {
		    j = j + 1
		}
		if (j == i) {			# in the next sample
		    call delete_samp (rv, ssp, SRANGE(ssp,j))
		} else if ((j-i) >= 1) {		# skipped a few samples
		    do k = i, j
		        call delete_samp (rv, ssp, SRANGE(ssp,k))
		}
		break
	    }
	}

	# Just add it to the list.
	SR_COUNT(ssp) = SR_COUNT(ssp) + 1
	SRANGE(ssp,SR_COUNT(ssp)) = left
	ERANGE(ssp,SR_COUNT(ssp)) = right
	call rv_mark_regions (ssp, gp)
	call sort_ranges (ssp)
end


# DELETE_SAMP - Delte a sample range from the list.

procedure delete_samp (rv, ssp, x)

pointer	rv					#I RV struct pointer
pointer	ssp					#I Sample struct pointer
real	x					#I WCS value of region to delete

double	dex()
real	l, r
pointer	gp
int	i, j

begin
	gp = RV_GP(rv)
	if (SR_COUNT(ssp) == 1)	{			# special case
	    call rv_erase_regions (ssp, RV_GP(rv))
	    SR_COUNT(ssp) = ALL_SPECTRUM
	    return
	}

	# Find the sample to delete.
	if (SR_UNITS(ssp) == PIXELS && RV_DCFLAG(rv) != -1)
	    x = (log10(x) - SR_W0(ssp)) / SR_WPC(ssp) + 1

	do i = 1, SR_COUNT(ssp) {
	    if (x >= SRANGE(ssp,i) && x <= ERANGE(ssp,i)) {
	        if (SR_IMTYPE(ssp) == OBJECT_SPECTRUM)
	            call gsview (gp, 0.115, 0.95, 0.51, 0.865)
	        else if (SR_IMTYPE(ssp) == REFER_SPECTRUM)
	            call gsview (gp, 0.115, 0.95, 0.125, 0.5)
		if (RV_PIXCORR(rv) == NO && RV_DCFLAG(rv) != -1 && 
	    	    SR_UNITS(ssp) == PIXELS) {
	        	l = real (dex(SR_W0(ssp)+(SRANGE(ssp,i)-1)*SR_WPC(ssp)))
	        	r = real (dex(SR_W0(ssp)+(ERANGE(ssp,i)-1)*SR_WPC(ssp)))
		} else {
		    l = SRANGE(ssp,i)
		    r = ERANGE(ssp,i)
		}
		call erase_range (gp, l, r)
		if (i == SR_COUNT(ssp)) {
		    SR_COUNT(ssp) = SR_COUNT(ssp) - 1
		} else {
		    do j = i, SR_COUNT(ssp)-1 {
		        SRANGE(ssp,j) = SRANGE(ssp,j+1)
		        ERANGE(ssp,j) = ERANGE(ssp,j+1)
		    }
		    SR_COUNT(ssp) = SR_COUNT(ssp) - 1
		}
		return
	    }
	}
end


# MARK_RANGE - Mark the region selected on the screen with a bar.

procedure mark_range (gp, left, right)

pointer	gp					#I GIO pointer
real	left, right				#I WCS of boundaries of region

real	x1, x2, y1, y2,	y

begin
	if (gp == NULL)
	    return
	
	call ggwind (gp, x1, x2, y1, y2)
	y = y1 + (y2-y1)/20.0			# Put it near bottom 5%

	call gamove (gp, left, y)		# Draw the horizontal bar
	call gadraw (gp, right, y)

	call gamove (gp, left, y1) 		# draw the cross bars at ends
	call gadraw (gp, left, y1+(2*(y-y1)))
	call gamove (gp, right, y1)
	call gadraw (gp, right, y1+(2*(y-y1)))
	call gflush (gp)
end


# ERASE_RANGE - Erase the range drawn to the screen.

procedure erase_range (gp, left, right)

pointer	gp					#I GIO pointer
real	left, right				#I Range endpoints

begin
	if (gp == NULL)
	    return

	call gseti (gp, G_PLCOLOR, C_BACKGROUND)
	call mark_range (gp, left, right)
	call gseti (gp, G_PLCOLOR, C_FOREGROUND) 
end


# SAMPLE_UNITS - Figure out what units the regions string is used in

int procedure sample_units (str)

char	str[ARB]				#I range specification string

begin
	if (str[1] == 'a' || str[1] == 'A' || IS_DIGIT(str[1]))
	    return (LAMBDA)
	else if (str[1] == 'p' || str[1] == 'P')
	    return (PIXELS)
	else {
	    return (ERR)
	}
end


# SORT_RANGES - Sort the ranges structure from left to right in terms of
# regions to simplify masking process

procedure sort_ranges (ssp)

pointer	ssp					#I Sample struct pointer

int	i, j, npts
real	temp

begin
	if (SR_COUNT(ssp) == 1 || SR_COUNT(ssp) == ALL_SPECTRUM)
	    return

	# Now do a simple insertion sort of the ranges
	npts = SR_COUNT(ssp)
	do i = 2, npts {
	    j = i
	    while (SRANGE(ssp,j) < SRANGE(ssp,j-1)) {
		# Swap 'em
		temp = SRANGE(ssp,j-1)
		SRANGE(ssp,j-1) = SRANGE(ssp,j)
		SRANGE(ssp,j) = temp

		temp = ERANGE(ssp,j-1)
		ERANGE(ssp,j-1) = ERANGE(ssp,j)
		ERANGE(ssp,j) = temp

		j = j - 1
	    }
	}
end
