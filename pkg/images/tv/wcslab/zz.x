include <gset.h>
include <math.h>


# Define the offset array.
define OFFSET Memr[$1+$2-1]

procedure wl_label (wd)

pointer wd                  # I: the WCSLAB descriptor

int	i
pointer	sp, offset_ptr

begin
	# Get some memory.
	call smark (sp)
	call salloc (offset_ptr, N_SIDES, TY_REAL)
	do i = 1, N_SIDES
	    OFFSET(offset_ptr,i) = 0.

 	call sfree (sp)
end
