# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	SZ_NUMBUF	32		# buffer for extracting numbers

# QP_CTOD -- Return as a double the next numeric token from the input stream.
# This differs from the standard FMTIO procedures only in that colon is not
# considered a numeric character (as used in sexagesimal numbers).

int procedure qp_ctod (str, ip, dval)

char	str[ARB]			#I input string
int	ip				#U pointer into input string
double	dval				#O double value

int	nchars, op, i
char	numbuf[SZ_NUMBUF]
int	ctod()

begin
	i = ip
	do op = 1, SZ_NUMBUF
	    if (str[i] != ':' && str[i] != EOS) {
		numbuf[op] = str[i]
		i = i + 1
	    } else
		break
	
	i = 1
	numbuf[op] = EOS
	nchars = ctod (numbuf, i, dval)
	ip = ip + i - 1

	return (nchars)
end
