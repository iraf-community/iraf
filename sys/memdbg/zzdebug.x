# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# ZZDEBUG.X -- Debug MEMIO.

task	memchk	= t_memchk,
	stack	= t_stack,
	realloc	= t_realloc


# MEMCHK -- Scan the mem.log output produced by the debug version of MEMIO
# (this must be sorted first) and check for memory which is allocated but
# never freed.

procedure t_memchk()

int	fd, ip
bool	passall, mark
int	addr, retaddr, seqno, action, class
int	old_addr, old_seqno, old_action
char	lbuf[SZ_LINE], old_lbuf[SZ_LINE]
char	descr[SZ_LINE], old_descr[SZ_LINE]
char	tokbuf[SZ_FNAME], fname[SZ_FNAME]

bool	clgetb()
int	open(), getline(), nscan(), gctol()
define	print_ 91

begin
	call clgstr ("fname", fname, SZ_FNAME)
	fd = open (fname, READ_ONLY, TEXT_FILE)

	passall = clgetb ("passall")
	old_addr = 0
	old_action = 0

	while (getline (fd, lbuf) != EOF) {
	    # Scan next line.
	    call sscan (lbuf)
	    call gargwrd (tokbuf, SZ_FNAME)
		ip = 1;  ip = gctol (tokbuf, ip, addr, 16)
	    call gargi (seqno)
	    call gargwrd (tokbuf, SZ_FNAME)
		ip = 1;  ip = gctol (tokbuf, ip, retaddr, 16)
	    call gargwrd (tokbuf, SZ_FNAME)
		action = tokbuf[1]
	    call gargi (class)
	    call gargstr (descr, SZ_LINE)

	    if (nscan() < 4) {
		if (passall)
		    call putline (STDOUT, lbuf)
		next
	    }

	    if (addr != old_addr) {
		# Starting a log for a new buffer address.
		if (old_lbuf[1] != EOS) {
		    if (IS_ALPHA(old_action) && old_action != 'F') {
			ip = 1
			while (old_lbuf[ip] != '\n' && old_lbuf[ip] != EOS)
			    ip = ip + 1
			old_lbuf[ip] = EOS
			call printf ("%s %70t####\n")
			    call pargstr (old_lbuf)

		    } else if (passall)
			call putline (STDOUT, old_lbuf)
		}

	    } else {
		# Verify operation on a particular buffer address.

		if (old_lbuf[1] != EOS && passall)
		    call putline (STDOUT, old_lbuf)

		mark = false
		if (IS_ALPHA(action) && class == 1) 
		    switch (old_action) {
		    case 'A', 'R':
			if (action != 'R' && action != 'F')
			    mark = true
		    case 'F':
			if (action != 'A')
			    mark = true
		    }

		if (mark) {
		    ip = 1
		    while (lbuf[ip] != '\n' && lbuf[ip] != EOS)
			ip = ip + 1
		    lbuf[ip] = EOS
		    call printf ("%s %70t####\n")
			call pargstr (lbuf)
		    lbuf[1] = EOS
		}
	    }

	    old_addr = addr
	    old_seqno = seqno
	    old_action = action
	    call strcpy (descr, old_descr, SZ_LINE)
	    call strcpy (lbuf, old_lbuf, SZ_LINE)
	}

	if (old_lbuf[1] != EOS && passall)
	    call putline (STDOUT, old_lbuf)
end


# STACK -- Test the SALLOC routine, which allocates storage on the stack.

procedure t_stack

int	bufsize
pointer	sp, junk
int	clglpi()

begin
	call smark (sp)

	while (clglpi ("buffer_size", bufsize) != EOF) {
	    call salloc (junk, bufsize, TY_CHAR)
	    call printf ("buffer pointer=%d, size=%d\n")
		call pargi (junk)
		call pargi (bufsize)
	    call flush (STDOUT)
	}

	call sfree (sp)
end


# REALLOC -- Test the REALLOC procedure, used to change the size of a buffer.
# Work with two buffers, so that memory can be fragmented, forcing buffers
# to move.

procedure t_realloc()

pointer	a, b
int	sza, new_sza, szb, new_szb
int	clgeti()

begin
	call malloc (a, SZ_LINE, TY_CHAR)
	call strcpy ("abcdefghijk", Memc[a], ARB)
	sza = SZ_LINE
	call malloc (b, SZ_LINE, TY_CHAR)
	call strcpy ("0123456789", Memc[b], ARB)
	szb = SZ_LINE

	call eprintf ("a is at %d, size %d: %s\n")
	    call pargi (a)
	    call pargi (sza)
	    call pargstr (Memc[a])
	call eprintf ("b is at %d, size %d: %s\n")
	    call pargi (b)
	    call pargi (szb)
	    call pargstr (Memc[b])
	call eprintf ("-------------------------------\n")

	repeat {
	    new_sza = clgeti ("a_bufsize")
	    if (new_sza == 0)
		return
	    call x_realloc (a, new_sza, TY_CHAR)
	    new_szb = clgeti ("b_bufsize")
	    if (new_szb == 0)
		return
	    call x_realloc (b, new_szb, TY_CHAR)

	    call eprintf ("a buf %d, size %d --> %d: %s\n")
		call pargi (a)
		call pargi (sza)
		call pargi (new_sza)
		call pargstr (Memc[a])
	    call eprintf ("b buf %d, size %d --> %d: %s\n")
		call pargi (b)
		call pargi (szb)
		call pargi (new_szb)
		call pargstr (Memc[b])

	    sza = new_sza
	    szb = new_szb
	}

	call mfree (a, TY_CHAR)
	call mfree (b, TY_CHAR)
end
