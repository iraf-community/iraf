# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	SZ_NODENAME	9

# MTFILE -- Test a filename to see if it is the name of a magtape file.
# A magtape file is characterized by the filename prefix "mt" (ingnoring
# the nodename prefix if any). 

int procedure mtfile (fname)

char	fname[ARB]			#I filename to be tested

int	ip, junk
char	nodename[SZ_NODENAME]
int	ki_extnode()

begin
	ip = ki_extnode (fname, nodename, SZ_NODENAME, junk) + 1

	if (fname[ip] == 'm' && fname[ip+1] == 't')
	    return (YES)
	else
	    return (NO)
end
