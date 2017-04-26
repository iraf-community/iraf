# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include	<config.h>


define	MDEBUG		false

# MEM_FINI --  Close out the MEMIO use in this task.  We perform the memory
# garbage collection and report usage statistics if requested.

procedure mem_fini (task)

char 	task[ARB]			# task name

int	sv_report

include	"nmemio.com"

begin
	# Do garbage collection.
	call mgc_collect()
            
	# Turn off reporting so the print statements below don't add
	# to the reported values.
	sv_report = mreport
	mreport   = 0
	mdebug    = 0
	in_task   = 0

	if (MDEBUG) {
	    call eprintf ("\nTask '%s':\n")
                call pargstr (task)
	    call eprintf ("     mwatch:\t%d\n") ; call pargi (mwatch)
	    call eprintf ("     mclear:\t%d\n") ; call pargi (mclear)
	    call eprintf ("   mcollect:\t%d\n") ; call pargi (mcollect)
	    call eprintf ("    mreport:\t%d\n") ; call pargi (mreport)
	}

	# Report memory usage.
	if (sv_report > 0) {
            call eprintf ("\nTask '%s':\n")
                call pargstr (task)
	    call eprintf ("       Memory:\t%9d used   (%9d max )\n")
		call pargl (mem_used)
		call pargi (max_alloc)
	    call eprintf ("     Pointers:\t%9d alloc  (%9d free)\n")
		call pargi (nalloc)
		call pargi (nfree)
	    call eprintf ("       Leaked:\t%9d bytes  (%9d ptrs)\n\n")
		call pargl (leaked)
		call pargl (nleaked)
	}

	# Free the GC buffer.
	call mgc_close ()
end
