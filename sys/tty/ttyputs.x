# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# TTYPUTS -- Put an EOS delimited  control string to the output file.

procedure ttyputs (fd, tty, ctrlstr, afflncnt)

int	fd			# output file
pointer	tty			# terminal descriptor
char	ctrlstr[ARB]		# control sequence to be output
int	afflncnt		# number of lines affected
int	strlen()

begin
	call ttywrite (fd, tty, ctrlstr, strlen(ctrlstr), afflncnt)
end
