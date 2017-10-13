# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>


# MFREE -- Free a previously allocated buffer.  If the buffer has already been
# returned (NULL pointer), ignore the request.  Once the buffer has been
# returned, the old pointer value is of not useful (and invalid), so set it
# to NULL.

procedure mfree (ptr, dtype)

pointer	ptr
int	dtype

pointer	bp
int	fwa, gc_type, status, lwl
char	emsg[SZ_LINE]

int	mgtfwa(), coerce(), mgc_gettype()
errchk	mgtfwa

include "nmemio.com"

begin
	# Check for NULL or already-freed pointers.  We only invoke an error 
	# rather than sys_panic to allow for recovery.
	if (mdebug > 0 && ptr == NULL) {
	    call merror ("Attempt to free NULL pointer")
	    return 
	}
	if (mcollect > 0) {
	    gc_type = mgc_gettype (ptr)
	    if ((gc_type != NULL && gc_type != dtype) && in_task > 0) {
	        call merror ("Attempt to free pointer of wrong type")
		dtype = gc_type
	    }
	}

	if (ptr != NULL) {
	    fwa = mgtfwa (ptr, dtype)

	    bp = coerce (ptr, dtype, TY_INT)
	    if (mwatch  > 0) {

	        # Check the lower sentinal value.  Any serious underflow 
		# would have corrupted the fwa and been detected above in
		# mgtfwa(), we really only use this to check for 0/1 indexing
		# problems that write before the start od the data.
	        if (Memi[bp-1] != lsentinal) {
	    	    call aclrc (emsg, SZ_LINE)
		    call sprintf (emsg, SZ_LINE, 
			"Pointer underflow: addr=0x%x  nelem=%d  type=%s\n")
			    call pargi (ptr)
			    call pargi (Memi[bp-2])
			    call ptype (dtype)
		    if (mreport > 0)
			call eprintf (emsg)
		    call merror (emsg)
		}

	        # Check the upper sentinal value.  Note that the setinal is
		# aligned to the INT boundary so depending on the type we
		# might still allow a slight overrun.
	        lwl = Memi[bp-4]
	        if (Memi[lwl] != usentinal) {
	    	    call aclrc (emsg, SZ_LINE)
		    call sprintf (emsg, SZ_LINE, 
			"Pointer overflow: addr=0x%x  nelem=%d  type=%s\n")
			    call pargi (ptr)
			    call pargi (Memi[bp-2])
			    call ptype (dtype)
		    if (mreport > 0)
			call eprintf (emsg)
		    call merror (emsg)
		}
	    }

	    call zmfree (fwa, status)
	    if (status == ERR)
		call sys_panic (SYS_MCORRUPTED, "Memory has been corrupted")

	    # Negate the pointer so we can detect another attempt to free it.
	    if (mcollect > 0 && in_task > 0)
	        call mgc_update (ptr)
	    if (mcollect >= 0)
	        nfree = nfree + 1
	    ptr   = NULL
	}
end


# PTYPE -- Convert a pointer type code t its string equivalent.

procedure ptype (dtype)

int     dtype

begin
	switch (dtype) {
	case TY_BOOL:       call pargstr ("TY_BOOL")
	case TY_CHAR:       call pargstr ("TY_CHAR")
	case TY_SHORT:      call pargstr ("TY_SHORT")
	case TY_INT:        call pargstr ("TY_INT")
	case TY_LONG:       call pargstr ("TY_LONG")
	case TY_REAL:       call pargstr ("TY_REAL")
	case TY_DOUBLE:     call pargstr ("TY_DOUBLE")
	case TY_COMPLEX:    call pargstr ("TY_COMPLEX")
	case TY_STRUCT:     call pargstr ("TY_STRUCT")
	case TY_POINTER:    call pargstr ("TY_POINTER")
	}
end
