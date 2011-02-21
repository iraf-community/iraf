include "fitsio.h"

procedure fsgkns(iunit,keywrd,nstart,nmax,strval,nfound,status)

# read an array of character string values from  header records

int     iunit           # i input file pointer
char    keywrd[SZ_FKEYWORD]     # i keyword name
%       character fkeywr*8
int     nstart          # i first sequence number
int     nmax            # i max. number of keyword
char    strval[SZ_FSTRVAL,ARB]     # o string value
%       character*70 fstrva
%       character*48 comm
%       character*8 keynam

int     nfound          # o no. of keywords found
int     status          # o error status
int	i
int	j

begin

call f77pak(keywrd,fkeywr,SZ_FKEYWORD)

nfound=0
j=nstart

do i=1,nmax   {
  call ftkeyn(fkeywr,j,keynam,status)
  if (status > 0)
     go to 10

  call ftgkys(iunit,keynam,fstrva,comm,status)

  if (status <= 0) {
     nfound=i
     call f77upk(fstrva,strval(1,i),SZ_FSTRVAL)

  } else if (status == 202) {
#     ignore keyword not found error
      status=0
  }
  j=j+1
 }

10
   j=0
end
