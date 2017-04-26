# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>

# IMPARSE -- Parse an image specification into the cluster name, cluster index,
# cluster size, kernel section, and image section fields.
#
#	Syntax:		cluster[cl_index/cl_size][ksection][section]
#
# where all fields are optional except the cluster name.  In the limiting case
# (cl_size = 1) the cluster name and image name are the same.  CL_INDEX and
# CL_SIZE must be simple nonnegative decimal integer constants, if given.  The
# [ character must be escaped to be included in the filename of the cluster.
#
# NOTE -- The image specification syntax is not frozen and further changes
# are likely.  Use of this routine outside IMIO is not recommended as the
# calling sequence may change.  Use imgname and imgsection instead.

procedure imparse (imspec, cluster, sz_cluster, ksection, sz_ksection,
	section, sz_section, cl_index, cl_size)

char	imspec[ARB]		# full image specification
char	cluster[ARB]		# receives cluster name
int	sz_cluster		# max chars in cluster name
char	ksection[ARB]		# receives kernel section
int	sz_ksection		# max chars in kernel section name
char	section[ARB]		# receives image section
int	sz_section		# max chars in image section name
int	cl_index		# receives cluster index (default -1)
int	cl_size			# receives cluster size (default -1)

pointer	sp, cp, secbuf
int	ip, op, lbrack, level, ch, n
bool	is_ksection, sect_out, ksect_out
int	stridx()
errchk	syserrs

begin
	call smark (sp)
	call salloc (secbuf, SZ_LINE, TY_CHAR)

	ip = 1
	op = 1

	# Extract cluster name.  The first (unescaped) [ marks the start of
	# either the cl_index subscript or a section field.

	for (ch=imspec[ip];  ch != EOS && ch != '[';  ch=imspec[ip]) {
	    if (ch == '\\' && imspec[ip+1] == '[') {
		cluster[op] = '\\'
		op = op + 1
		cluster[op] = '['
		ip = ip + 1
	    } else
		cluster[op] = ch

	    op = min (sz_cluster, op + 1)
	    ip = ip + 1
	}

	cluster[op] = EOS
	ksection[1] = EOS
	section[1]  = EOS
	lbrack      = ip
	cl_index    = -1
	cl_size     = -1

	if (ch == EOS) {
	    call sfree (sp)
	    return
	}

	# If we have a [...] field, determine whether it is a cl_index
	# subscript or a kernel or image section.  A cl_index subscript is
	# anything with the syntax [ddd] or [ddd/ddd]; anything else is a
	# kernel or image section.

	ip = ip + 1
	n = -1

	for (ch=imspec[ip];  ch != EOS;  ch=imspec[ip]) {
	    if (IS_DIGIT(ch)) {
		if (n < 0)
		    n = 0
		n = (n * 10) + TO_INTEG(ch)
	    } else if (ch == '/') {
		cl_index = max (n, 1)
		n = -1
	    } else if (ch == ']') {
		ip = ip + 1
		break
	    } else {
		# Not a cl_index subscript; must be a section.
		ip = lbrack
		n = -1
		break
	    }
	    ip = ip + 1
	}

	if (cl_index < 0)
	    cl_index = n
	else
	    cl_size = n

	# The rest of the input string consists of the kernel and image
	# sections, if any.

	sect_out = false
	ksect_out = false

	while (imspec[ip] == '[') {
	    is_ksection = false
	    cp = secbuf
	    level = 0

	    for (ch=imspec[ip];  ch != EOS;  ch=imspec[ip]) {
		if (ch == '[')
		    level = level + 1
		else if (ch == ']')
		    level = level - 1
		else if (!is_ksection)
		    if (stridx (imspec[ip], " 0123456789+-:*,") == 0)
			is_ksection = true

		Memc[cp] = ch
		cp = cp + 1
		ip = ip + 1

		if (level == 0)
		    break
	    }
	    Memc[cp] = EOS

	    if (level != 0)
		call syserrs (SYS_IMSYNSEC, imspec)
	    if (is_ksection) {
		if (ksect_out)
		    call syserrs (SYS_IMSYNSEC, imspec)
		call strcpy (Memc[secbuf], ksection, sz_ksection)
		ksect_out = true
	    } else {
		if (sect_out)
		    call syserrs (SYS_IMSYNSEC, imspec)
		call strcpy (Memc[secbuf], section, sz_section)
		sect_out = true
	    }

	    while (imspec[ip] != EOS && imspec[ip] != '[')
		ip = ip + 1
	}

	call sfree (sp)
end
