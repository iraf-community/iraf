include "../lib/obsfile.h"

define	LOGPTR	32			# log2(maxpts) (4e9)

# PH_1C4R2ISORT -- Vector quicksort on the first and second indices arrays,
# where the second index is used to resolve ambiguities in the first index.
# An additional 4 input arrays are sorted as well.

procedure ph_1c4r2isort (label, d1, d2, d3, d4, findex, sindex, npix)

char	label[DEF_LENLABEL,ARB]	# the char array
real	d1[ARB]			# the first input data array
real	d2[ARB]			# the second input data array
real	d3[ARB]			# the third input data array
real	d4[ARB]			# the fourth input data array
int	findex[ARB]		# first index array which is sorted on
int	sindex[ARB]		# second index array which is sorted on
int	npix			# number of pixels

real	tempr
int	fpivot, spivot, tempi
int	i, j, k, p, lv[LOGPTR], uv[LOGPTR]
char	tempc[DEF_LENLABEL]
int	ph_2icompare()
define	swapi {tempi=$1;$1=$2;$2=tempi}
define	swapr {tempr=$1;$1=$2;$2=tempr}
define	swapc {call strcpy ($1, tempc, DEF_LENLABEL);call strcpy ($2, $1, DEF_LENLABEL);call strcpy (tempc, $2, DEF_LENLABEL)}

begin
	lv[1] = 1
	uv[1] = npix
	p = 1

	while (p > 0) {
	    if (lv[p] >= uv[p])			# only one elem in this subset
		p = p - 1			# pop stack
	    else {
		# Dummy do loop to trigger the Fortran optimizer.
		do p = p, ARB {
		    i = lv[p] - 1
		    j = uv[p]

		    # Select as the pivot the element at the center of the
		    # array, to avoid quadratic behavior on an already sorted
		    # array.

		    k = (lv[p] + uv[p]) / 2
		    swapr (d1[j], d1[k])
		    swapr (d2[j], d2[k])
		    swapr (d3[j], d3[k])
		    swapr (d4[j], d4[k])
		    swapc (label[1,j], label[1,k])
		    swapi (findex[j], findex[k])
		    swapi (sindex[j], sindex[k])
		    fpivot = findex[j]			 # pivot line
		    spivot = sindex[j]

		    while (i < j) {
			for (i=i+1; ph_2icompare (findex[i], sindex[i], fpivot,
			    spivot) < 0; i=i+1)
			    ;
			for (j=j-1;  j > i;  j=j-1)
			    if (ph_2icompare (findex[j], sindex[j], fpivot,
			        spivot) <= 0)
				break
			if (i < j) {                     # switch elements
			    swapr (d1[i], d1[j])
			    swapr (d2[i], d2[j])
			    swapr (d3[i], d3[j])
			    swapr (d4[i], d4[j])
		    	    swapc (label[1,i], label[1,j])
			    swapi (sindex[i], sindex[j])
			    swapi (findex[i], findex[j]) # interchange elements
			}
		    }

		    j = uv[p]			# move pivot to position i
		    swapr (d1[i], d1[j])
		    swapr (d2[i], d2[j])
		    swapr (d3[i], d3[j])
		    swapr (d4[i], d4[j])
		    swapc (label[1,i], label[1,j])
		    swapi (sindex[i], sindex[j])
		    swapi (findex[i], findex[j])	# interchange elements

		    if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
			lv[p+1] = lv[p]
			uv[p+1] = i - 1
			lv[p] = i + 1
		    } else {
			lv[p+1] = i + 1
			uv[p+1] = uv[p]
			uv[p] = i - 1
		    }

		    break
		}
		p = p + 1			# push onto stack
	    }
	}
end


# PH_4R2ISORT -- Vector quicksort on the first and second indices arrays,
# where the second index is used to resolve ambiguities in the first index.
# An additional 4 input arrays are sorted as well.

procedure ph_4r2isort (d1, d2, d3, d4, findex, sindex, npix)

real	d1[ARB]			# the first input data array
real	d2[ARB]			# the second input data array
real	d3[ARB]			# the third input data array
real	d4[ARB]			# the fourth input data array
int	findex[ARB]		# first index array which is sorted on
int	sindex[ARB]		# second index array which is sorted on
int	npix			# number of pixels

int	fpivot, spivot, tempi
int	i, j, k, p, lv[LOGPTR], uv[LOGPTR]
real	tempr
int	ph_2icompare()
define	swapi {tempi=$1;$1=$2;$2=tempi}
define	swapr {tempr=$1;$1=$2;$2=tempr}

begin
	lv[1] = 1
	uv[1] = npix
	p = 1

	while (p > 0) {
	    if (lv[p] >= uv[p])			# only one elem in this subset
		p = p - 1			# pop stack
	    else {
		# Dummy do loop to trigger the Fortran optimizer.
		do p = p, ARB {
		    i = lv[p] - 1
		    j = uv[p]

		    # Select as the pivot the element at the center of the
		    # array, to avoid quadratic behavior on an already sorted
		    # array.

		    k = (lv[p] + uv[p]) / 2
		    swapr (d1[j], d1[k])
		    swapr (d2[j], d2[k])
		    swapr (d3[j], d3[k])
		    swapr (d4[j], d4[k])
		    swapi (findex[j], findex[k])
		    swapi (sindex[j], sindex[k])
		    fpivot = findex[j]			 # pivot line
		    spivot = sindex[j]

		    while (i < j) {
			for (i=i+1; ph_2icompare (findex[i], sindex[i], fpivot,
			    spivot) < 0; i=i+1)
			    ;
			for (j=j-1;  j > i;  j=j-1)
			    if (ph_2icompare (findex[j], sindex[j], fpivot,
			        spivot) <= 0)
				break
			if (i < j) {                     # switch elements
			    swapr (d1[i], d1[j])
			    swapr (d2[i], d2[j])
			    swapr (d3[i], d3[j])
			    swapr (d4[i], d4[j])
			    swapi (sindex[i], sindex[j])
			    swapi (findex[i], findex[j]) # interchange elements
			}
		    }

		    j = uv[p]			# move pivot to position i
		    swapr (d1[i], d1[j])
		    swapr (d2[i], d2[j])
		    swapr (d3[i], d3[j])
		    swapr (d4[i], d4[j])
		    swapi (sindex[i], sindex[j])
		    swapi (findex[i], findex[j])	# interchange elements

		    if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
			lv[p+1] = lv[p]
			uv[p+1] = i - 1
			lv[p] = i + 1
		    } else {
			lv[p+1] = i + 1
			uv[p+1] = uv[p]
			uv[p] = i - 1
		    }

		    break
		}
		p = p + 1			# push onto stack
	    }
	}
end


# PH_3RIRSORT -- Vector quicksort on the index and the rindex array,
# where the rindex array is used to resolve ambiguities in the index array.
# The three real arrays are sorted as well.

procedure ph_3rirsort (d1, d2, d3, index, rindex, npix)

real	d1[ARB]			# the first input data array
real	d2[ARB]			# the second input data array
real	d3[ARB]			# the third input data array
int	index[ARB]		# first index array which is sorted on
real	rindex[ARB]		# second index array which is sorted on
int	npix			# number of pixels

int	fpivot, tempi
int	i, j, k, p, lv[LOGPTR], uv[LOGPTR]
real	rpivot, tempr
int	ph_ircompare()
define	swapi {tempi=$1;$1=$2;$2=tempi}
define	swapr {tempr=$1;$1=$2;$2=tempr}

begin
	lv[1] = 1
	uv[1] = npix
	p = 1

	while (p > 0) {
	    if (lv[p] >= uv[p])			# only one elem in this subset
		p = p - 1			# pop stack
	    else {
		# Dummy do loop to trigger the Fortran optimizer.
		do p = p, ARB {
		    i = lv[p] - 1
		    j = uv[p]

		    # Select as the pivot the element at the center of the
		    # array, to avoid quadratic behavior on an already sorted
		    # array.

		    k = (lv[p] + uv[p]) / 2
		    swapr (d1[j], d1[k])
		    swapr (d2[j], d2[k])
		    swapr (d3[j], d3[k])
		    swapi (index[j], index[k])
		    swapr (rindex[j], rindex[k])
		    fpivot = index[j]			 # pivot line
		    rpivot = rindex[j]

		    while (i < j) {
			for (i=i+1; ph_ircompare (index[i], rindex[i], fpivot,
			    rpivot) < 0; i=i+1)
			    ;
			for (j=j-1;  j > i;  j=j-1)
			    if (ph_ircompare (index[j], rindex[j], fpivot,
			        rpivot) <= 0)
				break
			if (i < j) {                     # switch elements
			    swapr (d1[i], d1[j])
			    swapr (d2[i], d2[j])
			    swapr (d3[i], d3[j])
			    swapr (rindex[i], rindex[j])
			    swapi (index[i], index[j]) # interchange elements
			}
		    }

		    j = uv[p]			# move pivot to position i
		    swapr (d1[i], d1[j])
		    swapr (d2[i], d2[j])
		    swapr (d3[i], d3[j])
		    swapr (rindex[i], rindex[j])
		    swapi (index[i], index[j])	# interchange elements

		    if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
			lv[p+1] = lv[p]
			uv[p+1] = i - 1
			lv[p] = i + 1
		    } else {
			lv[p+1] = i + 1
			uv[p+1] = uv[p]
			uv[p] = i - 1
		    }

		    break
		}
		p = p + 1			# push onto stack
	    }
	}
end


# PH_QSORT -- Vector quicksort. In this version the index array is
# sorted.

procedure ph_qsort (data, index, npix, offset)

real	data[ARB]		# data array
int	index[ARB]		# index array
int	npix			# number of pixels
int	offset			# the index offset

int	i, j, lv[LOGPTR], p, uv[LOGPTR], temp
real	pivot

begin
	# Initialize the indices for an inplace sort.
	do i = 1, npix
	    index[i] = i

	p = 1
	lv[1] = 1
	uv[1] = npix
	while (p > 0) {

	    # If only one elem in subset pop stack otherwise pivot line.
	    if (lv[p] >= uv[p])
		p = p - 1
	    else {
		i = lv[p] - 1
		j = uv[p]
		pivot = data[index[j]]

		while (i < j) {
		    for (i=i+1;  data[index[i]] < pivot;  i=i+1)
			;
		    for (j=j-1;  j > i;  j=j-1)
			if (data[index[j]] <= pivot)
			    break
		    if (i < j) {		# out of order pair
			temp = index[j]		# interchange elements
			index[j] = index[i]
			index[i] = temp
		    }
		}

		j = uv[p]			# move pivot to position i
		temp = index[j]			# interchange elements
		index[j] = index[i]
		index[i] = temp

		if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
		    lv[p+1] = lv[p]
		    uv[p+1] = i - 1
		    lv[p] = i + 1
		} else {
		    lv[p+1] = i + 1
		    uv[p+1] = uv[p]
		    uv[p] = i - 1
		}

		p = p + 1			# push onto stack
	    }
	}

	do i = 1, npix
	    index[i] = index[i] + offset - 1
end


# PH_2ICOMPARE -- Comparison routine for PH_4R2ISORT.

int procedure ph_2icompare (findex, sindex, fpivot, spivot)

int	findex		# the first index value
int	sindex		# the second index value
int	fpivot		# the first pivot value
int	spivot		# the second pivot value

begin
	if (findex < fpivot)
	    return (-1)
	else if (findex > fpivot)
	    return (1)
	else if (sindex < spivot)
	    return (-1)
	else if (sindex > spivot)
	    return (1)
	else 
	    return (0)
end


# PH_IRCOMPARE -- Comparison routine for PH_3RIRSORT.

int procedure ph_ircompare (findex, sindex, fpivot, spivot)

int	findex		# the first index value
real	sindex		# the second index value
int	fpivot		# the first pivot value
real	spivot		# the second pivot value

begin
	if (findex < fpivot)
	    return (-1)
	else if (findex > fpivot)
	    return (1)
	else if (sindex < spivot)
	    return (-1)
	else if (sindex > spivot)
	    return (1)
	else 
	    return (0)
end


# PH_5R3ISORT -- Vector quicksort on the first and second indices arrays,
# where the second index is used to resolve ambiguities in the first index.
# An additional 5 input arrays are sorted as well.

procedure ph_5r3isort (findex, sindex, i1, d1, d2, d3, d4, d5, naperts, npix)

int	findex[ARB]		# first index array which is sorted on
int	sindex[ARB]		# second index array which is sorted on
int	i1[ARB]			# the 3 integer array
real	d1[ARB]			# the first input data array
real	d2[ARB]			# the second input data array
real	d3[naperts,ARB]		# the third input data array
real	d4[naperts,ARB]		# the fourth input data array
real	d5[naperts,ARB]		# the fifth input data array
int	naperts			# number of apertures
int	npix			# number of pixels

int	fpivot, spivot, tempi
int	i, j, k, l,p, lv[LOGPTR], uv[LOGPTR]
real	tempr
int	ph_2aicompare()
define	swapi {tempi=$1;$1=$2;$2=tempi}
define	swapr {tempr=$1;$1=$2;$2=tempr}

begin
	lv[1] = 1
	uv[1] = npix
	p = 1

	while (p > 0) {
	    if (lv[p] >= uv[p])			# only one elem in this subset
		p = p - 1			# pop stack
	    else {
		# Dummy do loop to trigger the Fortran optimizer.
		do p = p, ARB {
		    i = lv[p] - 1
		    j = uv[p]

		    # Select as the pivot the element at the center of the
		    # array, to avoid quadratic behavior on an already sorted
		    # array.

		    k = (lv[p] + uv[p]) / 2
		    swapi (findex[j], findex[k])
		    swapi (sindex[j], sindex[k])
		    swapi (i1[j], i1[k]) 
		    swapr (d1[j], d1[k])
		    swapr (d2[j], d2[k])
		    do  l = 1, naperts {
		        swapr (d3[l,j], d3[l,k])
		        swapr (d4[l,j], d4[l,k])
		        swapr (d5[l,j], d5[l,k])
		    }
		    fpivot = findex[j]			 # pivot line
		    spivot = sindex[j]

		    while (i < j) {
			for (i=i+1; ph_2aicompare (findex[i], sindex[i], fpivot,
			    spivot) < 0; i=i+1)
			    ;
			for (j=j-1;  j > i;  j=j-1)
			    if (ph_2aicompare (findex[j], sindex[j], fpivot,
			        spivot) <= 0)
				break
			if (i < j) {                     # switch elements
			    swapi (sindex[i], sindex[j])
			    swapi (findex[i], findex[j]) # interchange elements
			    swapi (i1[i], i1[j])
			    swapr (d1[i], d1[j])
			    swapr (d2[i], d2[j])
		    	    do  l = 1, naperts {
			        swapr (d3[l,i], d3[l,j])
			        swapr (d4[l,i], d5[l,j])
			        swapr (d5[l,i], d5[l,j])
			    }
			}
		    }

		    j = uv[p]			   # move pivot to position i
		    swapi (sindex[i], sindex[j])
		    swapi (findex[i], findex[j])   # interchange elements
		    swapi (i1[i], i1[j])
		    swapr (d1[i], d1[j])
		    swapr (d2[i], d2[j])
		    do  l = 1, naperts {
		        swapr (d3[l,i], d3[l,j])
		        swapr (d4[l,i], d4[l,j])
		        swapr (d5[l,i], d5[l,j])
		    }

		    if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
			lv[p+1] = lv[p]
			uv[p+1] = i - 1
			lv[p] = i + 1
		    } else {
			lv[p+1] = i + 1
			uv[p+1] = uv[p]
			uv[p] = i - 1
		    }

		    break
		}
		p = p + 1			# push onto stack
	    }
	}
end


# PH_2AICOMPARE -- Comparison routine for PH_5R3ISORT.

int procedure ph_2aicompare (findex, sindex, fpivot, spivot)

int	findex		# the first index value
int	sindex		# the second index value
int	fpivot		# the first pivot value
int	spivot		# the second pivot value

begin
	if (findex < fpivot)
	    return (-1)
	else if (findex > fpivot)
	    return (1)
	else if (sindex < spivot)
	    return (-1)
	else if (sindex > spivot)
	    return (1)
	else 
	    return (0)
end
