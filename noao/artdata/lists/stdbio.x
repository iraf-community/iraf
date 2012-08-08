include <pkg/dttext.h>
include "starlist.h"

# ST_DTINIT -- Write the header to the database.

procedure st_dtinit (dt, st, starlist, sseed, lseed)

pointer	dt				# pointer to output database
pointer	st				# pointer to structure
char	starlist[ARB]			# name of output text file
long	sseed				# spatial function seed
long	lseed				# luminsosity function seed

begin
	call dtptime (dt)
	call dtput (dt, "# begin\t%s\n")
	    call pargstr (starlist)

	# Write out the spatial density function parameters.
	call dtput (dt, "#\tspatial\t\t%s\n")
	switch (ST_SPATIAL(st)) {
	case ST_UNIFORM:
	    call pargstr ("uniform")
	case ST_HUBBLE:
	    call pargstr ("hubble")
	    call dtput (dt, "#\txcenter\t\t%g\n")
		call pargr (ST_XC(st))
	    call dtput (dt, "#\tycenter\t\t%g\n")
		call pargr (ST_YC(st))
	    call dtput (dt, "#\tcoreradius\t%g\n")
		call pargr (ST_CORE(st))
	    call dtput (dt, "#\tbaseline\t%g\n")
		call pargr (ST_BASE(st))
	case ST_SPFILE:
	    call pargstr (ST_SFILE(st))
	    call dtput (dt, "#\txcenter\t\t%g\n")
		call pargr (ST_XC(st))
	    call dtput (dt, "#\tycenter\t\t%g\n")
		call pargr (ST_YC(st))
	}
	call dtput (dt, "#\txmin\t\t%g\n")
	    call pargr (ST_XMIN(st))
	call dtput (dt, "#\txmax\t\t%g\n")
	    call pargr (ST_XMAX(st))
	call dtput (dt, "#\tymin\t\t%g\n")
	    call pargr (ST_YMIN(st))
	call dtput (dt, "#\tymax\t\t%g\n")
	    call pargr (ST_YMAX(st))

	# Write out the luminsosity function parameters.
	call dtput (dt, "#\tluminosity\t%s\n")
	switch (ST_LUMINOSITY(st)) {
	case ST_UNIFORM:
	    call pargstr ("uniform")
	case ST_POWLAW:
	    call pargstr ("powlaw")
	    call dtput (dt, "#\tpower\t\t%g\n")
	        call pargr (ST_POWER(st))
	case ST_SALPETER:
	    call pargstr ("salpeter")
	    call dtput (dt, "#\tmzero\t\t%g\n")
	        call pargr (ST_MZERO(st))
	case ST_BANDS:
	    call pargstr ("bands")
	    call dtput (dt, "#\tmzero\t\t%g\n")
	        call pargr (ST_MZERO(st))
	    call dtput (dt, "#\talpha\t\t%g\n")
	        call pargr (ST_ALPHA(st))
	    call dtput (dt, "#\tbeta\t\t%g\n")
	        call pargr (ST_BETA(st))
	    call dtput (dt, "#\tdelta\t\t%g\n")
	        call pargr (ST_DELTA(st))
	    call dtput (dt, "#\tmstar\t\t%g\n")
	        call pargr (ST_MSTAR(st))
	case ST_LFFILE:
	    call pargstr (ST_LFILE(st))
	}
	call dtput (dt, "#\tminmag\t\t%g\n")
	    call pargr (ST_MINMAG(st))
	call dtput (dt, "#\tmaxmag\t\t%g\n")
	    call pargr (ST_MAXMAG(st))

	# Save the spatial density function fitting parameters.
	call dtput (dt, "#\tnssample\t%d\n")
	    call pargi (ST_NSSAMPLE(st))
	call dtput (dt, "#\tsorder\t\t%d\n")
	    call pargi (ST_SORDER(st))
	call dtput (dt, "#\tsseed\t\t%d\n")
	    call pargl (sseed)

	# Save the luminosity function fitting parameters.
	call dtput (dt, "#\tnlsample\t%d\n")
	    call pargi (ST_NLSAMPLE(st))
	call dtput (dt, "#\tlorder\t\t%d\n")
	    call pargi (ST_LORDER(st))
	call dtput (dt, "#\tlseed\t\t%d\n")
	    call pargl (lseed)

	# Save the number of stars.
	call dtput (dt, "#\tnstars\t\t%d\n")
	    call pargi (ST_NSTARS(st))
end


# ST_DTGINIT -- Write the GALLIST header to the database.

procedure st_dtginit (dt, st, galaxies, sseed, lseed)

pointer	dt				# pointer to database
pointer	st				# pointer to starlist structure
char	galaxies[ARB]			# name of output text file
long	sseed				# spatial function seed
long	lseed				# luminsosity function seed

begin
	call dtptime (dt)
	call dtput (dt, "# begin\t%s\n")
	    call pargstr (galaxies)

	# Save the spatial distribution function parameters.
	call dtput (dt, "#\tspatial\t\t%s\n")
	switch (ST_SPATIAL(st)) {
	case ST_UNIFORM:
	    call pargstr ("uniform")
	case ST_HUBBLE:
	    call pargstr ("hubble")
	    call dtput (dt, "#\txcenter\t\t%g\n")
		call pargr (ST_XC(st))
	    call dtput (dt, "#\tycenter\t\t%g\n")
		call pargr (ST_YC(st))
	    call dtput (dt, "#\tcoreradius\t%g\n")
		call pargr (ST_CORE(st))
	    call dtput (dt, "#\tbaseline\t%g\n")
		call pargr (ST_BASE(st))
	case ST_SPFILE:
	    call pargstr (ST_SFILE(st))
	    call dtput (dt, "#\txcenter\t\t%g\n")
		call pargr (ST_XC(st))
	    call dtput (dt, "#\tycenter\t\t%g\n")
		call pargr (ST_YC(st))
	}
	call dtput (dt, "#\txmin\t\t%g\n")
	    call pargr (ST_XMIN(st))
	call dtput (dt, "#\txmax\t\t%g\n")
	    call pargr (ST_XMAX(st))
	call dtput (dt, "#\tymin\t\t%g\n")
	    call pargr (ST_YMIN(st))
	call dtput (dt, "#\tymax\t\t%g\n")
	    call pargr (ST_YMAX(st))

	# Save the luminsosity function parameters.
	call dtput (dt, "#\tluminosity\t%s\n")
	switch (ST_LUMINOSITY(st)) {
	case ST_UNIFORM:
	    call pargstr ("uniform")
	case ST_POWLAW:
	    call pargstr ("powlaw")
	    call dtput (dt, "#\tpower\t\t%g\n")
		call pargr (ST_POWER(st))
	case ST_SCHECTER:
	    call pargstr ("shechter")
	    call dtput (dt, "#\tmzero\t\t%g\n")
	        call pargr (ST_MZERO(st))
	    call dtput (dt, "#\talpha\t\t%g\n")
		call pargr (ST_ALPHA(st))
	    call dtput (dt, "#\tmstar\t\t%g\n")
		call pargr (ST_MSTAR(st))
	case ST_LFFILE:
	    call pargstr (ST_LFILE(st))
	}
	call dtput (dt, "#\tminmag\t\t%g\n")
	    call pargr (ST_MINMAG(st))
	call dtput (dt, "#\tmaxmag\t\t%g\n")
	    call pargr (ST_MAXMAG(st))
	call dtput (dt, "#\teradius\t\t%g\n")
	    call pargr (ST_ERADIUS(st))
	call dtput (dt, "#\tsradius\t\t%g\n")
	    call pargr (ST_SRADIUS(st))

	call dtput (dt, "#\tegalmix\t\t%g\n")
	    call pargr (ST_EGALMIX(st))
	call dtput (dt, "#\tar\t\t%g\n")
	    call pargr (ST_AR(st))
	call dtput (dt, "#\tabsorption\t%g\n")
	    call pargr (ST_ABSORPTION(st))
	call dtput (dt, "#\tz\t\t%g\n")
	    call pargr (ST_Z(st))

	# Save the spatial distribution fitting parameters.
	call dtput (dt, "#\tnssample\t%d\n")
	    call pargi (ST_NSSAMPLE(st))
	call dtput (dt, "#\tsorder\t\t%d\n")
	    call pargi (ST_SORDER(st))
	call dtput (dt, "#\tsseed\t\t%d\n")
	    call pargl (sseed)

	# Save the spatial function fitting parameters.
	call dtput (dt, "#\tnlsample\t%d\n")
	    call pargi (ST_NLSAMPLE(st))
	call dtput (dt, "#\tlorder\t\t%d\n")
	    call pargi (ST_LORDER(st))
	call dtput (dt, "#\tlseed\t\t%d\n")
	    call pargl (lseed)

	# Save the number of stars.
	call dtput (dt, "#\tngals\t\t%d\n")
	    call pargi (ST_NSTARS(st))
end


# ST_DTWRITE -- Write the starlist to the database.

procedure st_dtwrite (dt, x, y, mag, nstars)

pointer	dt		# pointer to the output database
real	x[ARB]		# array of x coordinates
real	y[ARB]		# array of y coordinates
real	mag[ARB]	# array of magnitude values
int	nstars		# number of stars

int	i, j
pointer	sp, index

begin
	call smark (sp)
	call salloc (index, nstars, TY_INT)
	call st_qsort (y, Memi[index], Memi[index], nstars)

	do i = 1, nstars {
	    j = Memi[index+i-1]
	    call dtput (dt, "\t%8.3f  %8.3f  %7.3f\n")
		call pargr (x[j])
		call pargr (y[j])
		call pargr (mag[j])
	}

	call sfree (sp)
end


# ST_DTGWRITE -- Procedure to write the galaxy list to the database

procedure st_dtgwrite (dt, x, y, mag, egal, axis, round, phi, nstars)

pointer	dt		# pointer to database
real	x[ARB]		# x values
real	y[ARB]		# y values
real	mag[ARB]	# magnitude values
int	egal[ARB]	# galaxy types
real	axis[ARB]	# galaxy diameters
real	round[ARB]	# galaxy roundness
real	phi[ARB]	# galaxy position angles
int	nstars		# number of stars

int	i, j
pointer	sp, index

begin
	call smark (sp)
	call salloc (index, nstars, TY_INT)
	call st_qsort (y, Memi[index], Memi[index], nstars)

	do i = 1, nstars {
	    j = Memi[index+i-1]
	    call dtput (dt,
	        "\t%8.3f  %8.3f  %7.3f  %7s  %7.2f  %5.3f  %5.1f\n")
		call pargr (x[j])
		call pargr (y[j])
		call pargr (mag[j])
		if (egal[j] == ST_DEVAUC)
		    call pargstr ("devauc")
		else
		    call pargstr ("expdisk")
		call pargr (axis[j])
		call pargr (round[j])
		call pargr (phi[j])
	}

	call sfree (sp)
end


# ST_QSORT -- Vector Quicksort. In this version the index array is
# sorted.

define	LOGPTR		20			# log2(maxpts) (1e6)

procedure st_qsort (data, a, b, npix)

real	data[ARB]		# data array
int	a[ARB], b[ARB]		# index array
int	npix			# number of pixels

int	i, j, lv[LOGPTR], p, uv[LOGPTR], temp
real	pivot

begin
	# Initialize the indices for an inplace sort.
	do i = 1, npix
	    a[i] = i
	call amovi (a, b, npix)

	# Initialize.
	p = 1
	lv[1] = 1
	uv[1] = npix

	# Sort.
	while (p > 0) {

	    # If only one elem in subset pop stack otherwise pivot line.
	    if (lv[p] >= uv[p])
		p = p - 1
	    else {
		i = lv[p] - 1
		j = uv[p]
		pivot = data[b[j]]

		while (i < j) {
		    for (i=i+1;  data[b[i]] < pivot;  i=i+1)
			;
		    for (j=j-1;  j > i;  j=j-1)
			if (data[b[j]] <= pivot)
			    break
		    if (i < j) {		# Out of order pair
			temp = b[j]		# Interchange elements
			b[j] = b[i]
			b[i] = temp
		    }
		}

		j = uv[p]			# Move pivot to position i
		temp = b[j]			# Interchange elements
		b[j] = b[i]
		b[i] = temp

		if (i-lv[p] < uv[p] - i) {	# Stack so shorter done first
		    lv[p+1] = lv[p]
		    uv[p+1] = i - 1
		    lv[p] = i + 1
		} else {
		    lv[p+1] = i + 1
		    uv[p+1] = uv[p]
		    uv[p] = i - 1
		}

		p = p + 1			# Push onto stack
	    }
	}
end
