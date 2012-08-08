# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# Debug MEMIO.

task	stack=t_stack, realloc=t_realloc


# Test the SALLOC routine, which allocates storage on the stack.

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


# Test the REALLOC procedure, used to change the size of a buffer.
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
