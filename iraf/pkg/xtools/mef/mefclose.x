include <pkg/mef.h>

# MEF_CLOSE -- Closes mef file descriptor and free up mef memory
# descriptor.

procedure mef_close(mef)

pointer mef  	#I Mef descriptor

begin
	call close(MEF_FD(mef))

	if (MEF_HDRP(mef) != NULL)
	   call mfree(MEF_HDRP(mef), TY_CHAR)

	call mfree (mef, TY_STRUCT)
end
