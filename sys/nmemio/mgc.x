# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>


#  MGC Interface - Simple memory garbage collector interface.  Our strategy
#  here is simply to store the pointer and its type (so we can dereference to
#  a host pointer).  As pointers are allocated they are saved here, and when
#  freed the pointer value is set to NULL to indicate it is invalid and that
#  slot is available for later reuse.
#	When a task completes, we run through the buffer looking for un-freed
#  pointers and manually reclaim the space.  This is not especially clever but
#  we are only used (presumably by developers) when requested so normal use
#  of MEMIO should see no added overhead.
#
#               mgc_init  ()
#              mgc_close  ()
#               mgc_save  (ptr, dtype)
#             mgc_update  (ptr)
#   index = mgc_getindex  (ptr)
#     type = mgc_gettype  (ptr)
#            mgc_collect  ()


define	SZ_GC_BUFFER		10240

# A zero-indexed structure saving the (ptr,type) pairs.
define	GC_PTR			Memi[$1+($2 * 2)]
define	GC_TYPE			Memi[$1+($2 * 2 + 1)]


# MGC_INIT -- Initialize the MGC interface.

procedure mgc_init ()

include "nmemio.com"

begin
       	if (mcollect > 0)
            call calloc (mgc, SZ_GC_BUFFER, TY_STRUCT)
	else
	    mgc = NULL
end


# MGC_CLOSE -- Close the MGC buffer.

procedure mgc_close ()

include "nmemio.com"

begin
        if (mcollect > 0 && mgc != NULL) {
            call mfree (mgc, TY_STRUCT)
	    mgc = NULL
	}
end


# MGC_SAVE -- Save a pointer in the GC buffer.

procedure mgc_save (ptr, dtype)

pointer	ptr					#i pointer to save
int	dtype					#i pointer type

int	i

include "nmemio.com"

begin
	if (mcollect <= 0 || mgc == NULL)
	    return

	for (i=0; i < SZ_GC_BUFFER - 1; i=i+1) {
	    if (GC_PTR(mgc,i) == NULL) {
		# Either already freed slot, or the first free one
		GC_PTR(mgc,i) = ptr
		GC_TYPE(mgc,i) = dtype
		if (bmax < i) {
		    bmax = i
		}
		if (mdebug > 0) {
		    call eprintf ("save %d: ptr 0x%x\n")
		        call pargi (i); call pargi (GC_PTR(mgc,i))
		}
		return
	    }
	}

	# If we get this far we've exhausted the GC buffer.  Print a warning
	# if reporting and just ignore it since the chances this would be 
	# a leaked pointer are rather small.
	if (mreport > 0)
	    call eprintf ("Warning:  GC buffer overflow\n")
end


# MGC_UPDATE -- Update the status of the pointer in the GC buffer.

procedure mgc_update (ptr)

pointer	ptr					#i pointer to save

int	i

include "nmemio.com"

begin
	if (mgc == NULL || in_task == 0)
	    return

	if (in_task > 0 && mdebug > 0) {
	  call eprintf ("update  0x%x  collect = %d\n")
	    call pargi (ptr)
	    call pargi (mcollect)
	}

	do i = 0, bmax {
	    if (GC_PTR(mgc,i) == ptr) {
		if (in_task > 0 && mdebug > 0) {
	  	    call eprintf ("update  %d:  0x%x  %d\n")
	    	    call pargi (i); call pargi (GC_PTR(mgc,i)); call pargi (ptr)
		}
	        GC_PTR(mgc,i) = NULL
		return
	    }
	}
end


# MGC_GETINDEX -- Given a pointer, return its GC index.

int procedure mgc_getindex (ptr)

pointer	ptr					#i pointer to save

int	i

include "nmemio.com"

begin
	if (mcollect <= 0 || mgc == NULL)
	    return

	do i = 0, bmax {
	    if (abs (GC_PTR(mgc,i)) == ptr)
		return (i)
	}

	return (NULL)
end


# MGC_GETTYPE -- Given a pointer, return its type.

int procedure mgc_gettype (ptr)

pointer	ptr					#i pointer to save

int	i

include "nmemio.com"

begin
	if (mcollect <= 0 || mgc == NULL)
	    return

	do i = 0, bmax {
	    if (abs (GC_PTR(mgc,i)) == ptr)
		return (GC_TYPE(mgc,i))
	}

	return (NULL)
end


# MGC_COLLECT -- Do the final garbage collection.

procedure mgc_collect ()

int	i, nchars
pointer	bp

int	sizeof ()
pointer	coerce ()

include "nmemio.com"

begin
	if (mcollect <= 0 || mgc == NULL)
	    return
	mcollect = -1

	do i = 0, bmax {
	    if (GC_PTR(mgc,i) != NULL) {
		if (mdebug > 0) {
		    call eprintf ("collect %d: recovering ptr 0x%x\n")
		        call pargi (i); call pargi (GC_PTR(mgc,i))
		}

                bp = coerce (GC_PTR(mgc,i), GC_TYPE(mgc,i), TY_INT)

		nleaked = nleaked + 1
		nchars = Memi[bp - 2] * sizeof (GC_TYPE(mgc,i))
		leaked = leaked + (nchars * SZB_CHAR)

		call mfree (GC_PTR(mgc,i), GC_TYPE(mgc,i))

	    }
	}
end
