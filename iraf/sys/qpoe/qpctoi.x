# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<lexnum.h>

# QP_CTO[IL] -- Decode an integer token from the input string, advancing the
# input pointer the the first character following the decoded number, and
# returning the number of numeric characters decoded as the function value.
# This is equivalent to the standard CTOI except that it calls LEXNUM first
# to determine the radix of the input number, hence can deals with hex and
# octal numbers as well as decimal.

int procedure qp_ctol (str, ip, lval)

char	str[ARB]			#I input string
int	ip				#U pointer into input string
long	lval				#O long integer value

int	ip_save, base, nchars
int	gctol(), lexnum()

begin
	ip_save = ip
	switch (lexnum (str, ip, nchars)) {
	case LEX_OCTAL:
	    base = 8
	case LEX_HEX:
	    base = 16
	default:
	    base = 10
	}

	ip = ip_save
	return (gctol (str, ip, lval, base))
end


int procedure qp_ctoi (str, ip, ival)

char	str[ARB]			#I input string
int	ip				#U pointer into input string
int	ival				#O integer value

long	lval
int	rv
int	qp_ctol()

begin
	rv = qp_ctol (str, ip, lval)
	ival = lval
	return (rv)
end
