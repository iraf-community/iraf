# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


# PF_FCTN -- function required by REGRES to define polynomial function
# This routine taken from Bevington, Page 177.

real procedure pf_fctn (x, i, j, jterms)

real	x[ARB]
int	i, j
int	jterms[ARB]
int	jexp

begin
	jexp = jterms[j]
	return (x[i] ** jexp)
end
