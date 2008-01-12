# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FSTDFILE -- Determine if the named file is a standard stream, and if so,
# return its file descriptor.

int procedure fstdfile (fname, ofd)

char	fname[ARB]
int	ofd
bool	streq()

begin
	ofd = NULL

	if (fname[1] != 'S' || fname[2] != 'T') {
	    return (NO)
	} else if (streq (fname, "STDIN")) {
	    ofd = STDIN
	    return (YES)
	} else if (streq (fname, "STDOUT")) {
	    ofd = STDOUT
	    return (YES)
	} else if (streq (fname, "STDERR")) {
	    ofd = STDERR
	    return (YES)
	} else if (streq (fname, "STDGRAPH")) {
	    ofd = STDGRAPH
	    return (YES)
	} else if (streq (fname, "STDIMAGE")) {
	    ofd = STDIMAGE
	    return (YES)
	} else if (streq (fname, "STDPLOT")) {
	    ofd = STDPLOT
	    return (YES)
	} else
	    return (NO)
end
