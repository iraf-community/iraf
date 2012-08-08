include "fitsio.h"

procedure fsnkey(nseq,keywrd,keyout,status)

# Make a keyword name by concatinating the root name and a
# sequence number

char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nseq            # i keyword sequence no.
char    keyout[SZ_FKEYWORD]     # o output keyword
%       character fkeyou*8
int     status          # o error status

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

call ftnkey(nseq,fkeywr,fkeyou,status)

call f77upk(fkeyou,keyout,SZ_FKEYWORD)
end
