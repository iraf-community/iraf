include <smw.h>


# SMW_NEWCOPY -- Make a new copy of an SMW structure.

pointer procedure smw_newcopy (smw)

pointer smw             #I SMW pointer to copy
pointer smwcopy         #O SMW copy

int     i
pointer mw_newcopy()

begin
	smwcopy = SMW_MW(smw,0)
        call smw_open (smwcopy, smw, NULL)

        if (SMW_APS(smwcopy) != NULL)
            call amovi (Memi[SMW_APS(smw)], Memi[SMW_APS(smwcopy)], i)

        do i = 0, SMW_NMW(smw)-1
            SMW_MW(smwcopy,i) = mw_newcopy (SMW_MW(smw,i))

        return (smwcopy)
end
