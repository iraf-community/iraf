# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

.help begmem, fixmem
.nf ___________________________________________________________________________
BEGMEM, FIXMEM -- Try to get/release physical memory for a process.  The
actual amount of physical memory available (in chars) is returned.  On a
machine with virtual memory, these routines adjust the working set size.

On any machine, BEGMEM may be used to determine the amount of available
physical memory, to tune algorithms for optimum performance.  BEGMEM returns
as its function value the actual working set size of the process after
the adjustment (or the current working set size if "best_size" is zero).
On some systems this represents a soft limit on the actual amount of memory
which can be used; it is a guarantee that at least that much space is
available.  Some systems will allow the actual working set to dynamically
exceed this value at runtime if the process pages heavily.  The hard limit
on the working set of a process is given by the "max_size" parameter.

Note that the working set must include space not only for a task specific
data buffers, but also for all other process data buffers and for the text
(instruction space) of the code being executed.  There is no easy way to
determine this, hence the application is expected to estimate it.  A typical
value for the base text+data size required to execute a program is 150Kb.
.endhelp ______________________________________________________________________


# BEGMEM -- Attempt to the adjust the amount of physical memory allocated
# to a process.  Save the old memory size in OLD_SIZE, so that memory may
# later be restored with FIXMEM.  The new working set size is returned as
# the function value and the hard limit on the working set size is returned
# in MAX_SIZE.  In general, the process can be expected to page, possibly
# heavily, or swap out if the working set size is exceeded.  All sizes are
# returned in SPP chars.  If BEST_SIZE is zero the working set size is not
# changed, i.e., the current working set parameters are returned.

int procedure begmem (best_size, old_size, max_size)

int	best_size		# desired working set size
int	old_size		# former working set size
int	max_size		# max physical memory available to this process

int	new_size

begin
	call zawset (best_size * SZB_CHAR, new_size, old_size, max_size)
	new_size = new_size / SZB_CHAR
	old_size = old_size / SZB_CHAR
	max_size = max_size / SZB_CHAR

	return (new_size)
end


# FIXMEM -- Restore the original working set size.

procedure fixmem (old_size)

int	old_size
int	j1, j2, j3

begin
	call zawset (old_size * SZB_CHAR, j1, j2, j3)
end
