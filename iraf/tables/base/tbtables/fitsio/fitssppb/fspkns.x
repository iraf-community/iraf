include "fitsio.h"

procedure fspkns(ounit,keywrd,nstart,nkey,strval,comm,status)

# write an array of character string values to header records

int     ounit           # i output file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nkey            # i number of keywords
char    strval[SZ_FSTRVAL,ARB]     # i string value
%       character fstrva*70
char    comm[SZ_FCOMMENT]       # i keyword comment
%       character fcomm*48
int     status          # o error status
int	i
int	n1

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)
call f77pak(comm  ,fcomm ,SZ_FCOMMENT)
# only support a single comment string for all the keywords in the SPP version
%       fcomm(48:48)='&'

n1=nstart
do i=1,nkey   {
 call f77pak(strval(1,i),fstrva,SZ_FSTRVAL)
 call ftpkys(ounit,fkeywr,n1,1,fstrva,fcomm,status)
 n1=n1+1
  }

end
