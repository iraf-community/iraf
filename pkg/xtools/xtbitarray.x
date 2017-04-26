include	<mach.h>

# XT_BAITARRAY -- Routines to manage a 2D bit array.
# One use for this is to hold a large boolean mask in the minimum amount of
# memory for random I/O.

define	BA_LEN	6				# Length of structure
define	BA_NC	Memi[$1]			# Number of columns
define	BA_NL	Memi[$1+1]			# Number of lines 
define	BA_NBE	Memi[$1+2]			# Number of bits per element
define	BA_NEW	Memi[$1+3]			# Number of elements per word
define	BA_MAX	Memi[$1+4]			# Maximum value
define	BA_DATA	Memi[$1+5]			# Data pointer


# XT_BAOPEN -- Open the bit array by allocating a structure and memory.

pointer procedure xt_baopen (nc, nl, maxval)

int	nc, nl				#I Size of bit array to open
int	maxval				#I Maximum value
pointer	ba				#R Bitarray structure

int	nbits
errchk	calloc

begin
	nbits = SZB_CHAR * SZ_INT * 8

	call calloc (ba, BA_LEN, TY_STRUCT)
	BA_NC(ba) = nc
	BA_NL(ba) = nl
	BA_MAX(ba) = maxval
        BA_NBE(ba) = int (log(real(maxval))/log(2.)+1.)
	BA_NBE(ba) = min (BA_NBE(ba), nbits)
	BA_NEW(ba) = nbits / BA_NBE(ba)
	call calloc (BA_DATA(ba),
	    (BA_NC(ba) * BA_NL(ba) + BA_NEW(ba) - 1) / BA_NEW(ba), TY_INT)
	return (ba)
end


# XT_BACLOSE -- Close the bit array by freeing memory.

procedure xt_baclose (ba)

pointer	ba				#U Bitarray structure

begin
	call mfree (BA_DATA(ba), TY_INT)
	call mfree (ba, TY_STRUCT)
end


# XT_BAPS -- Put short data.

procedure xt_baps (ba, c, l, data, n)

pointer	ba				#I Bitarray structure
int	c, l				#I Starting element
short	data[n]				#I Input data array
int	n				#I Number of data values

int	i, j, k, m, val

begin
	k = (c - 1) + BA_NC(ba) * (l - 1) - 1
	do m = 1, n {
	    k = k + 1
	    j = k / BA_NEW(ba)
	    i = BA_NBE(ba) * mod (k, BA_NEW(ba)) + 1
	    val = min (data[m], BA_MAX(ba))
	    call bitpak (val, Memi[BA_DATA(ba)+j], i, BA_NBE(ba))
	}
end


# XT_BAGS -- Get short data.

procedure xt_bags (ba, c, l, data, n)

pointer	ba				#I Bitarray structure
int	c, l				#I Starting element
short	data[n]				#I Output data array
int	n				#I Number of data values

int	i, j, k, m, bitupk()

begin
	k = (c - 1) + BA_NC(ba) * (l - 1) - 1
	do m = 1, n {
	    k = k + 1
	    j = k / BA_NEW(ba)
	    i = BA_NBE(ba) * mod (k, BA_NEW(ba)) + 1
	    data[m] = bitupk (Memi[BA_DATA(ba)+j], i, BA_NBE(ba))
	}
end


# XT_BAPI -- Put integer data.

procedure xt_bapi (ba, c, l, data, n)

pointer	ba				#I Bitarray structure
int	c, l				#I Starting element
int	data[n]				#I Input data array
int	n				#I Number of data values

int	i, j, k, m, val

begin
	k = (c - 1) + BA_NC(ba) * (l - 1) - 1
	do m = 1, n {
	    k = k + 1
	    j = k / BA_NEW(ba)
	    i = BA_NBE(ba) * mod (k, BA_NEW(ba)) + 1
	    val = min (data[m], BA_MAX(ba))
	    call bitpak (val, Memi[BA_DATA(ba)+j], i, BA_NBE(ba))
	}
end


# XT_BAGI -- Get integer data.

procedure xt_bagi (ba, c, l, data, n)

pointer	ba				#I Bitarray structure
int	c, l				#I Starting element
int	data[n]				#I Output data array
int	n				#I Number of data values

int	i, j, k, m, bitupk()

begin
	k = (c - 1) + BA_NC(ba) * (l - 1) - 1
	do m = 1, n {
	    k = k + 1
	    j = k / BA_NEW(ba)
	    i = BA_NBE(ba) * mod (k, BA_NEW(ba)) + 1
	    data[m] = bitupk (Memi[BA_DATA(ba)+j], i, BA_NBE(ba))
	}
end
