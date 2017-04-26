# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# SYSID -- Return a line of text identifying the current user, machine, and
# version of IRAF, and containing the current date and time.  The format is
# as follows:
#
#	NOAO/IRAF V1.3 username@lyra Tue 09:47:50 27-Aug-85
#
# The string "NOAO/IRAF V1.3" is given by the value of the environment variable
# "version", defined in lib$clpackage.cl (unless redefined by the user).  The
# string "username" is the value of the environment variable "userid", defined
# by the user in the login.cl file.  The output string is not terminated by a
# newline.

procedure sysid (outstr, maxch)

char	outstr[maxch]			# receives id string
int	maxch

pointer	sp, buf
int	op, nchars
int	envfind(), gstrcpy()
long	clktime()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	nchars = envfind ("version", outstr, maxch)
	if (nchars <= 0)
	    nchars = gstrcpy ("NOAO/IRAF", outstr, maxch)

	op = nchars + 1
	outstr[op] = ' '
	op = op + 1

	# The variable "userid" is defined in the user's login.cl file.  This
	# gives the user the opportunity to set the value of this string to
	# something other than their host system login name.

	nchars = envfind ("userid", Memc[buf], SZ_LINE)

	op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	outstr[op] = '@'
	op = op + 1

	call gethost (Memc[buf], SZ_LINE)
	op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	outstr[op] = ' '
	op = op + 1

	call cnvtime (clktime(long(0)), Memc[buf], SZ_LINE)
	op = op + gstrcpy (Memc[buf], outstr[op], maxch-op+1)
	outstr[op] = EOS

	call sfree (sp)
end
