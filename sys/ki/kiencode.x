# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# KI_ENCODE -- Encode an integer into the indicated number of chars for
# transmission between machines via a byte stream.

procedure ki_encode (data, str, nchars)

long	data			# data value to be encoded
char	str[nchars]		# output chars
int	nchars			# number of chars to be encoded

int	i
long	v, nv

begin
	v = abs (data)

	do i = 1, nchars {
	    nv = v / 128
	    str[i] = v - (nv * 128)
	    v = nv
	}

	if (data < 0)
	    if (str[1] == 0)
		str[1] = -128
	    else
		str[1] = -str[1]
end


# KI_DECODE -- Decode the long integer value encoded by ki_encode, returning
# the long integer value as the function value.

long procedure ki_decode (str, nchars)

char	str[ARB]		# string to be decoded
int	nchars			# number of chars to decode

bool	neg
int	pow, i
long	sum

begin
	sum = str[1]
	neg = (sum < 0)
	if (neg)
	    if (sum == -128)
		sum = 0
	    else
		sum = -sum

	pow = 1

	do i = 2, nchars {
	    pow = pow * 128
	    sum = sum + (str[i] * pow)
	}

	if (neg)
	    return (-sum)
	else
	    return (sum)
end
