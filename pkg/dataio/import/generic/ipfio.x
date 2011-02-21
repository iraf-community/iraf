include <mach.h>
include <fset.h>
include "../import.h"

define	DEBUG	false


# IP_GSTR --  Get a string of the specifed length from the given offset.

procedure ip_gstr (fd, offset, len, outstr)

int     fd
int	offset
int	len
char    outstr[ARB]

int     nstat, read()
pointer sp, buf

begin
        call smark (sp)
        call salloc (buf, len+2, TY_CHAR)
	call aclrc (Memc[buf], len+2)
	call aclrc (outstr, len+2)

        call ip_lseek (fd, offset)
        nstat = read (fd, Memc[buf], len)

        if (mod(offset,2) == 0 && offset > 1)
            call bytmov (Memc[buf], 2, Memc[buf], 1, len)
        call chrupk (Memc[buf], 1, outstr, 1, len)

	if (DEBUG) { call eprintf ("ip_gstr: :%s: len=%d\n"); 
	    call pargstr(outstr) ; call pargi (len) }
        call sfree (sp)
end


# IP_GETB --  Get a byte from the given offset.

short procedure ip_getb (fd, offset)

int     fd
int	offset

int     nstat, read()
short	val
char    buf[2]

begin
        call ip_lseek (fd, offset)
        nstat = read (fd, buf, 2)

        if (mod(offset,2) == 0)
            call bytmov (buf, 2, buf, 1, 2)
        call chrupk (buf, 1, buf, 1, 2)

	if (DEBUG) { call eprintf ("ip_getb: %d\n"); call pargs(buf[1]) }
	if (buf[1] < 0)
	    val = buf[1] + 256
	else
	    val = buf[1]
        return (val)
end


# IP_GETU --  Get a unsigned short integer from the given offset.

int procedure ip_getu (fd, offset)

int     fd
int	offset

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

int     fd
int	offset

int     nstat, read()
short   val

begin
        call ip_lseek (fd, offset)
            nstat = read (fd, val, SZ_SHORT * SZB_CHAR)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargs(val) }
        return (val)
end


int procedure ip_geti (fd, offset)

int     fd
int	offset

int     nstat, read()
int   val

begin
        call ip_lseek (fd, offset)
            nstat = read (fd, val, SZ_INT32 * SZB_CHAR)
	    if (SZ_INT != SZ_INT32)
		call iupk32 (val, val, 1)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargi(val) }
        return (val)
end


long procedure ip_getl (fd, offset)

int     fd
int	offset

int     nstat, read()
long   val

begin
        call ip_lseek (fd, offset)
            nstat = read (fd, val, SZ_INT32 * SZB_CHAR)
	    if (SZ_INT != SZ_INT32)
		call iupk32 (val, val, 1)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargl(val) }
        return (val)
end


real procedure ip_getr (fd, offset)

int     fd
int	offset

int     nstat, read()
real   val

begin
        call ip_lseek (fd, offset)
            nstat = read (fd, val, SZ_REAL * SZB_CHAR)
	call ieeupkr (val)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargr(val) }
        return (val)
end


double procedure ip_getd (fd, offset)

int     fd
int	offset

int     nstat, read()
double   val

begin
        call ip_lseek (fd, offset)
            nstat = read (fd, val, SZ_DOUBLE * SZB_CHAR)
	call ieeupkd (val)

	if (DEBUG) { call eprintf ("ip_get: %g\n"); call pargd(val) }
        return (val)
end


# IP_GETN --  Get a native floating point number from the given offset.

real procedure ip_getn (fd, offset)

int     fd
int	offset

int     nstat, read()
real	rval

begin
	call ip_lseek (fd, offset)
	nstat = read (fd, rval, SZ_REAL)

	if (DEBUG) { call eprintf ("ip_getn: %g\n"); call pargr(rval) }
	return (rval)
end


# IP_GETN8 --  Get a native double precision floating point number from the 
# given offset.

double procedure ip_getn8 (fd, offset)

int     fd
int	offset

int     nstat, read()
double	dval

begin
	call ip_lseek (fd, offset)
	nstat = read (fd, dval, SZ_DOUBLE)

	if (DEBUG) { call eprintf ("ip_getn8: %g\n"); call pargd(dval) }
	return (dval)
end


# IP_AGETB -- Get an array of bytes from the file.  The data pointer is
# allocated if necessary and contains the data on output.

procedure ip_agetb (fd, ptr, len)

int     fd					#i file descriptor
pointer ptr					#i data pointer
int     len					#i length of array

pointer sp, buf
int     fp, nval, nstat
int     ip_lnote(), read()

begin
        fp = ip_lnote(fd)
        if (mod(fp,2) == 0 && fp != 1)
	    nval = len
	else
	    nval = len + 1

        call smark (sp)
        call salloc (buf, nval, TY_CHAR)

        if (ptr == NULL)
            call malloc (ptr, nval * SZB_CHAR, TY_CHAR)
        nstat = read (fd, Memc[buf], nval / SZB_CHAR + 1)

        fp = ip_lnote(fd)
        if (mod(fp,2) == 0 && fp != 1)
            call bytmov (Memc[buf], 2, Memc[buf], 1, nval)
        call achtbc (Memc[buf], Memc[ptr], len)

        call sfree (sp)
end


# IP_AGETU -- Get an array of <type> from the file.  The data pointer is
# allocated if necessary and contains the data on output.

procedure ip_agetu (fd, ptr, len)

int     fd					#i file descriptor
pointer ptr					#i data pointer
int     len					#i length of array

begin
	call ip_agets (fd, ptr, len)
	call achtsu (Mems[ptr], Mems[ptr], len)
end


# IP_AGET[silrd] -- Get an array of <type> from the file.  The data pointer is
# allocated if necessary and contains the data on output.


procedure ip_agets (fd, ptr, len)

int     fd					#i file descriptor
pointer ptr					#i data pointer
int     len					#i length of array

int     nstat
int     read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_SHORT)
            nstat = read (fd, Mems[ptr], len * SZ_SHORT)
end


procedure ip_ageti (fd, ptr, len)

int     fd					#i file descriptor
pointer ptr					#i data pointer
int     len					#i length of array

int     nstat
int     read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_INT)
            nstat = read (fd, Memi[ptr], len * SZ_INT32)
	    if (SZ_INT != SZ_INT32)
	        call iupk32 (Memi[ptr], Memi[ptr], len)
end


procedure ip_agetl (fd, ptr, len)

int     fd					#i file descriptor
pointer ptr					#i data pointer
int     len					#i length of array

int     nstat
int     read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_LONG)
            nstat = read (fd, Meml[ptr], len * SZ_INT32)
	    if (SZ_INT != SZ_INT32)
	        call iupk32 (Meml[ptr], Meml[ptr], len)
end


procedure ip_agetr (fd, ptr, len)

int     fd					#i file descriptor
pointer ptr					#i data pointer
int     len					#i length of array

int     nstat
int     read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_REAL)
            nstat = read (fd, Memr[ptr], len * SZ_REAL)
	call ieevupkr (Memr[ptr], Memr[ptr], len)
end


procedure ip_agetd (fd, ptr, len)

int     fd					#i file descriptor
pointer ptr					#i data pointer
int     len					#i length of array

int     nstat
int     read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_DOUBLE)
            nstat = read (fd, Memd[ptr], len * SZ_DOUBLE)
	call ieevupkd (Memd[ptr], Memd[ptr], len)
end



# IP_AGETN -- Get an array of native floats from the file.  The data pointer is
# allocated if necessary and contains the data on output.

procedure ip_agetn (fd, ptr, len)

int     fd					#i file descriptor
pointer ptr					#i data pointer
int     len					#i length of array

int     nstat
int     read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_REAL)
        nstat = read (fd, Memr[ptr], len * SZ_REAL)
end


# IP_AGETN8 -- Get an array of native doubles from the file.  The data pointer
# is allocated if necessary and contains the data on output.

procedure ip_agetn8 (fd, ptr, len)

int     fd					#i file descriptor
pointer ptr					#i data pointer
int     len					#i length of array

int     nstat
int     read()

begin
        if (ptr == NULL)
            call malloc (ptr, len, TY_DOUBLE)
        nstat = read (fd, Memd[ptr], len * SZ_DOUBLE)
end


# -----------------------------------------------------------------
# ------------------ UTILITY FILE I/O FUNCTIONS -------------------
# -----------------------------------------------------------------


define	BLKSIZE		1024

# IP_LINE -- Return the offset of the start of the given line number.

int procedure ip_line (fd, line)

int	fd					#i input file descriptor
int	line					#i line number to search

pointer	sp, cbuf, buf
int	nl, offset, i, nread, fsize

int	read(), fstati()

define	done_ 	99
define	err_ 	98

begin
	if (line == 1) {
	    return (1)
	} else {
	    call smark (sp)
	    call salloc (buf, BLKSIZE, TY_CHAR)
	    call salloc (cbuf, BLKSIZE, TY_CHAR)

	    # Rewind file descriptor
	    call ip_lseek (fd, BOF)
	    nl = 1
	    offset = 1
	    
	    nread = BLKSIZE / SZB_CHAR
	    fsize = fstati (fd, F_FILESIZE)
	    while (read (fd, Memc[buf], nread) != EOF) {
		# Convert it to spp chars.
	        call ip_lskip (fd, nread)
        	call chrupk (Memc[buf], 1, Memc[cbuf], 1, BLKSIZE)
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
	    call ip_lseek (fd, BOF)
	    return (ERR)

done_	    if (DEBUG) { call eprintf("ip_line: '%s'\n"); call pargi(offset) }
	    call sfree (sp)
	    call ip_lseek (fd, offset)
	    return (offset)
	}
end


# IP_LOCATE -- Return the offset of the start of the given pattern.

int procedure ip_locate (fd, offset, pattern)

int     fd                                      #i input file descriptor
int	offset					#i offset to begin search
char	pattern[ARB]                            #i pattern to locate

pointer	sp, cbuf, buf
int     fsize, nread, patlen, cur_offset, loc

int     fstati(), read(), strsearch(), strlen()

define	done_	99

begin
        # Rewind file descriptor
        call ip_lseek (fd, offset)
	cur_offset = offset

	call smark (sp)
	call salloc (buf, BLKSIZE, TY_CHAR)
	call salloc (cbuf, BLKSIZE, TY_CHAR)

	if (DEBUG) { call eprintf("ip_loc: offset %d\n"); call pargi(offset)}

	nread = BLKSIZE / SZB_CHAR
	fsize = fstati (fd, F_FILESIZE)
	patlen = strlen (pattern)
        while (read (fd, Memc[buf], nread) != EOF) {
            # Convert it to spp chars.
	    call ip_lskip (fd, nread)
            call chrupk (Memc[buf], 1, Memc[cbuf], 1, BLKSIZE)
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
	call ip_lseek (fd, BOF)
        return (ERR)

done_	if (DEBUG) { call eprintf("ip_loc: %d\n"); call pargi(cur_offset)}
	call sfree (sp)
	call ip_lseek (fd, offset)
	return (cur_offset)
end


# IP_LSEEK -- Set the file position as a byte offset.

procedure ip_lseek (fd, offset)

int     fd					#i file descriptor
int     offset					#i requested offset

long    cur_offset, where, fsize
int	fstati()
common  /fiocom/ cur_offset

begin
	if (offset == BOF || offset == ERR) {
            cur_offset = 1
            call seek (fd, BOF)
	} else {
	    fsize = fstati (fd, F_FILESIZE) * SZB_CHAR
            cur_offset = min (fsize, offset)
	    where = min (fsize, (offset/SZB_CHAR+mod(offset,2)))
            call seek (fd, where)
	}
end


# IP_LNOTE -- Note the file position as a byte offset.

int procedure ip_lnote (fd)

int     fd					#i file descriptor (unused)

long    cur_offset
common  /fiocom/ cur_offset

begin
	return (cur_offset)
end


# IP_LSKIP -- Bump the file position by a byte offset.

procedure ip_lskip (fd, skip)

int     fd                                      #i file descriptor
int     skip

long    cur_offset
common  /fiocom/ cur_offset

begin
        call ip_lseek (fd, cur_offset+skip)
end
