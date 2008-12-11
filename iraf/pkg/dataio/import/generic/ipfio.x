include <mach.h>
include <fset.h>
include "../import.h"

define	DEBUG	false


# IP_GSTR --  Get a string of the specifed length from the given offset.

procedure ip_gstr (fd, offset, len, outstr)

int	fd
long	offset
int	len
char    outstr[ARB]

size_t	sz_val
long	l_val
size_t	c_1, c_2
long	nstat
pointer sp, buf
long	read()
long	modl()

begin
	c_1 = 1
	c_2 = 2
        call smark (sp)
	sz_val = len+2
        call salloc (buf, sz_val, TY_CHAR)
	call aclrc (Memc[buf], sz_val)
	call aclrc (outstr, sz_val)

        call ip_lseek (fd, offset)
	sz_val = len
        nstat = read (fd, Memc[buf], sz_val)

	sz_val = len
	l_val = 2
        if (modl(offset,l_val) == 0 && offset > 1)
            call bytmov (Memc[buf], c_2, Memc[buf], c_1, sz_val)
        call chrupk (Memc[buf], c_1, outstr, c_1, sz_val)

	if (DEBUG) { call eprintf ("ip_gstr: :%s: len=%d\n"); 
	    call pargstr(outstr) ; call pargi (len) }
        call sfree (sp)
end


# IP_GETB --  Get a byte from the given offset.

short procedure ip_getb (fd, offset)

int	fd
long	offset

long	l_val
size_t	c_1, c_2
short	val
long	nstat
char    buf[2]
long	read()
long	modl()

begin
	c_1 = 1
	c_2 = 2
        call ip_lseek (fd, offset)
        nstat = read (fd, buf, c_2)

	l_val = 2
        if (modl(offset,l_val) == 0)
            call bytmov (buf, c_2, buf, c_1, c_2)
        call chrupk (buf, c_1, buf, c_1, c_2)

	if (DEBUG) { call eprintf ("ip_getb: %d\n"); call pargs(buf[1]) }
	if (buf[1] < 0)
	    val = buf[1] + 256
	else
	    val = buf[1]
        return (val)
end


# IP_GETU --  Get a unsigned short integer from the given offset.

int procedure ip_getu (fd, offset)

int	fd
long	offset

int	val
short	ip_gets()

begin
	val = ip_gets (fd, offset)
	if (val < 0)
	    val = val + 65536
	return (val)
end

# IP_GET[silrd] --  Get a value of <type> from the given offset.



short procedure ip_gets (fd, offset)

int	fd
long	offset

size_t	sz_val
long	nstat
short   val
long	read()

begin
        call ip_lseek (fd, offset)
	sz_val = SZ_SHORT * SZB_CHAR
        nstat = read (fd, val, sz_val)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargs(val) }
        return (val)
end


int procedure ip_geti (fd, offset)

int	fd
long	offset

size_t	sz_val
long	nstat
int   val
long	read()

begin
        call ip_lseek (fd, offset)
	sz_val = SZ_INT * SZB_CHAR
	# arg2: incompatible pointer
        nstat = read (fd, val, sz_val)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargi(val) }
        return (val)
end


long procedure ip_getl (fd, offset)

int	fd
long	offset

size_t	sz_val
long	nstat
long   val
long	read()

begin
        call ip_lseek (fd, offset)
	sz_val = SZ_LONG * SZB_CHAR
	# arg2: incompatible pointer
        nstat = read (fd, val, sz_val)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargl(val) }
        return (val)
end


real procedure ip_getr (fd, offset)

int	fd
long	offset

size_t	sz_val
long	nstat
real   val
long	read()

begin
        call ip_lseek (fd, offset)
	sz_val = SZ_REAL * SZB_CHAR
	# arg2: incompatible pointer
        nstat = read (fd, val, sz_val)
	call ieeupkr (val)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargr(val) }
        return (val)
end


double procedure ip_getd (fd, offset)

int	fd
long	offset

size_t	sz_val
long	nstat
double   val
long	read()

begin
        call ip_lseek (fd, offset)
	sz_val = SZ_DOUBLE * SZB_CHAR
	# arg2: incompatible pointer
        nstat = read (fd, val, sz_val)
	call ieeupkd (val)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargd(val) }
        return (val)
end


# IP_GETN --  Get a native floating point number from the given offset.

real procedure ip_getn (fd, offset)

int	fd
long	offset

size_t	sz_val
long	nstat
real	rval
long	read()

begin
	call ip_lseek (fd, offset)
	sz_val = SZ_REAL
	# arg2: incompatible pointer
	nstat = read (fd, rval, sz_val)

	if (DEBUG) { call eprintf ("ip_getn: %g\n"); call pargr(rval) }
	return (rval)
end


# IP_GETN8 --  Get a native double precision floating point number from the 
# given offset.

double procedure ip_getn8 (fd, offset)

int	fd
long	offset

size_t	sz_val
long	nstat
double	dval
long	read()

begin
	call ip_lseek (fd, offset)
	sz_val = SZ_DOUBLE
	# arg2: incompatible pointer
	nstat = read (fd, dval, sz_val)

	if (DEBUG) { call eprintf ("ip_getn8: %g\n"); call pargd(dval) }
	return (dval)
end


# IP_AGETB -- Get an array of bytes from the file.  The data pointer is
# allocated if necessary and contains the data on output.

procedure ip_agetb (fd, ptr, len)

int	fd					#i file descriptor
pointer ptr					#i data pointer
size_t	len					#i length of array

long	l_val
size_t	c_1, c_2
size_t	nval
pointer sp, buf
long	fp, nstat
long	read(), modl()
long	ip_lnote()

begin
	c_1 = 1
	c_2 = 2

        fp = ip_lnote(fd)
	l_val = 2
        if (modl(fp,l_val) == 0 && fp != 1)
	    nval = len
	else
	    nval = len + 1

        call smark (sp)
        call salloc (buf, nval, TY_CHAR)

        if (ptr == NULL)
            call malloc (ptr, nval * SZB_CHAR, TY_CHAR)
        nstat = read (fd, Memc[buf], nval / SZB_CHAR + 1)

        fp = ip_lnote(fd)
	l_val = 2
        if (modl(fp,l_val) == 0 && fp != 1)
            call bytmov (Memc[buf], c_2, Memc[buf], c_1, nval)
	# arg1: incompatible pointer
        call achtbc (Memc[buf], Memc[ptr], len)

        call sfree (sp)
end


# IP_AGETU -- Get an array of <type> from the file.  The data pointer is
# allocated if necessary and contains the data on output.

procedure ip_agetu (fd, ptr, len)

int	fd					#i file descriptor
pointer ptr					#i data pointer
size_t	len					#i length of array

begin
	call ip_agets (fd, ptr, len)
	call achtsu (Mems[ptr], Mems[ptr], len)
end


# IP_AGET[silrd] -- Get an array of <type> from the file.  The data pointer is
# allocated if necessary and contains the data on output.


procedure ip_agets (fd, ptr, len)

int	fd					#i file descriptor
pointer ptr					#i data pointer
size_t	len					#i length of array

long	nstat
long	read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_SHORT)
        nstat = read (fd, Mems[ptr], len * SZ_SHORT)
end


procedure ip_ageti (fd, ptr, len)

int	fd					#i file descriptor
pointer ptr					#i data pointer
size_t	len					#i length of array

long	nstat
long	read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_INT)
	# arg2: incompatible pointer
        nstat = read (fd, Memi[ptr], len * SZ_INT)
end


procedure ip_agetl (fd, ptr, len)

int	fd					#i file descriptor
pointer ptr					#i data pointer
size_t	len					#i length of array

long	nstat
long	read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_LONG)
	# arg2: incompatible pointer
        nstat = read (fd, Meml[ptr], len * SZ_LONG)
end


procedure ip_agetr (fd, ptr, len)

int	fd					#i file descriptor
pointer ptr					#i data pointer
size_t	len					#i length of array

long	nstat
long	read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_REAL)
	# arg2: incompatible pointer
        nstat = read (fd, Memr[ptr], len * SZ_REAL)
	call ieevupkr (Memr[ptr], Memr[ptr], len)
end


procedure ip_agetd (fd, ptr, len)

int	fd					#i file descriptor
pointer ptr					#i data pointer
size_t	len					#i length of array

long	nstat
long	read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_DOUBLE)
	# arg2: incompatible pointer
        nstat = read (fd, Memd[ptr], len * SZ_DOUBLE)
	call ieevupkd (Memd[ptr], Memd[ptr], len)
end



# IP_AGETN -- Get an array of native floats from the file.  The data pointer is
# allocated if necessary and contains the data on output.

procedure ip_agetn (fd, ptr, len)

int	fd					#i file descriptor
pointer ptr					#i data pointer
size_t	len					#i length of array

long	nstat
long	read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_REAL)
	# arg2: incompatible pointer
        nstat = read (fd, Memr[ptr], len * SZ_REAL)
end


# IP_AGETN8 -- Get an array of native doubles from the file.  The data pointer
# is allocated if necessary and contains the data on output.

procedure ip_agetn8 (fd, ptr, len)

int	fd					#i file descriptor
pointer ptr					#i data pointer
size_t	len					#i length of array

long	nstat
long	read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_DOUBLE)
	# arg2: incompatible pointer
        nstat = read (fd, Memd[ptr], len * SZ_DOUBLE)
end


# -----------------------------------------------------------------
# ------------------ UTILITY FILE I/O FUNCTIONS -------------------
# -----------------------------------------------------------------


define	BLKSIZE		1024

# IP_LINE -- Return the offset of the start of the given line number.

long procedure ip_line (fd, line)

int	fd					#i input file descriptor
long	line					#i line number to search

size_t	sz_val
size_t	c_1, c_2
long	l_val
int	i
size_t	nread
pointer	sp, cbuf, buf
long	nl, fsize, offset

long	read()
long	fstatl()

define	done_ 	99
define	err_ 	98

begin
	c_1 = 1
	c_2 = 2
	if (line == 1) {
	    return (1)
	} else {
	    call smark (sp)
	    sz_val = BLKSIZE
	    call salloc (buf, sz_val, TY_CHAR)
	    call salloc (cbuf, sz_val, TY_CHAR)

	    # Rewind file descriptor
	    l_val = BOF
	    call ip_lseek (fd, l_val)
	    nl = 1
	    offset = 1
	    
	    nread = BLKSIZE / SZB_CHAR
	    fsize = fstatl (fd, F_FILESIZE)
	    while (read (fd, Memc[buf], nread) != EOF) {
		# Convert it to spp chars.
		l_val = nread
	        call ip_lskip (fd, l_val)
		sz_val = BLKSIZE
        	call chrupk (Memc[buf], c_1, Memc[cbuf], c_1, sz_val)
		do i = 1, BLKSIZE {
		    if (Memc[cbuf+i-1] == '\n') {
		        nl = nl + 1
		        offset = offset + 1
		        if (nl == line)
			    goto done_
	 	    } else
		        offset = offset + 1
		    if (offset >= fsize)
		        goto err_
		}
	    }
err_	    call sfree (sp)
	    l_val = BOF
	    call ip_lseek (fd, l_val)
	    return (ERR)

done_	    if (DEBUG) { call eprintf("ip_line: '%s'\n"); call pargl(offset) }
	    call sfree (sp)
	    call ip_lseek (fd, offset)
	    return (offset)
	}
end


# IP_LOCATE -- Return the offset of the start of the given pattern.

long procedure ip_locate (fd, offset, pattern)

int	fd                                      #i input file descriptor
long	offset					#i offset to begin search
char	pattern[ARB]                            #i pattern to locate

size_t	sz_val
size_t	c_1
long	l_val
pointer	sp, cbuf, buf
size_t	nread
int	patlen, loc
long	fsize, cur_offset

long	fstatl()
int	strsearch(), strlen()
long	read()

define	done_	99

begin
	c_1 = 1
        # Rewind file descriptor
        call ip_lseek (fd, offset)
	cur_offset = offset

	call smark (sp)
	sz_val = BLKSIZE
	call salloc (buf, sz_val, TY_CHAR)
	call salloc (cbuf, sz_val, TY_CHAR)

	if (DEBUG) { call eprintf("ip_loc: offset %d\n"); call pargl(offset)}

	nread = BLKSIZE / SZB_CHAR
	fsize = fstatl (fd, F_FILESIZE)
	patlen = strlen (pattern)
        while (read (fd, Memc[buf], nread) != EOF) {
            # Convert it to spp chars.
	    l_val = nread
	    call ip_lskip (fd, l_val)
	    sz_val = BLKSIZE
            call chrupk (Memc[buf], c_1, Memc[cbuf], c_1, sz_val)
	    loc = strsearch (Memc[cbuf], pattern)
	    if (loc != 0) {
  		cur_offset = cur_offset + loc - 1 - patlen
        	goto done_
            } else {
		# Allow some overlap in case the pattern broke over the blocks.
                cur_offset = cur_offset + BLKSIZE - 2 * patlen
		call ip_lseek (fd, cur_offset)
		if (cur_offset + BLKSIZE > fsize)
		    nread = fsize - cur_offset + 1
            }
        }
	call sfree (sp)
	l_val = BOF
	call ip_lseek (fd, l_val)
        return (ERR)

done_	if (DEBUG) { call eprintf("ip_loc: %d\n"); call pargl(cur_offset)}
	call sfree (sp)
	call ip_lseek (fd, offset)
	return (cur_offset)
end


# IP_LSEEK -- Set the file position as a byte offset.

procedure ip_lseek (fd, offset)

int	fd					#i file descriptor
long	offset					#i requested offset

long	l_val
long    cur_offset, where, fsize
long	fstatl(), modl()
common  /fiocom/ cur_offset

begin
	if (offset == BOF || offset == ERR) {
            cur_offset = 1
	    l_val = BOF
            call seek (fd, l_val)
	} else {
	    fsize = fstatl (fd, F_FILESIZE) * SZB_CHAR
            cur_offset = min (fsize, offset)
	    l_val = 2
	    where = min (fsize, (offset/SZB_CHAR+modl(offset,l_val)))
            call seek (fd, where)
	}
end


# IP_LNOTE -- Note the file position as a byte offset.

long procedure ip_lnote (fd)

int	fd					#i file descriptor (unused)

long    cur_offset
common  /fiocom/ cur_offset

begin
	return (cur_offset)
end


# IP_LSKIP -- Bump the file position by a byte offset.

procedure ip_lskip (fd, skip)

int	fd                                      #i file descriptor
long	skip

long    cur_offset
common  /fiocom/ cur_offset

begin
        call ip_lseek (fd, cur_offset+skip)
end
