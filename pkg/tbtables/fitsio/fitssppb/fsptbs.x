include "fitsio.h"

procedure fsptbs(iunit,frow,fchar,nchars,svalue,status)

# write a consecutive string of characters to an ascii or binary
# table. This will span multiple rows of the table if NCHARS+FCHAR is
# greater than the length of a row.

int     iunit           # i input file pointer
int     frow            # i first row
int     fchar           # i first character
int     nchars          # i number of characters
char    svalue[ARB]     # i string value
%       character fsvalu*256
int     status          # o error status
int	readfirst
int	writefirst
int	ntodo
int	itodo

begin

# since the string may be arbitrarily long, write it in pieces
readfirst=1
writefirst=fchar
ntodo=nchars
itodo=min(256,ntodo)

while (itodo > 0) {
  call f77pak(svalue[readfirst],fsvalu,itodo)
  call ftptbs(iunit,frow,writefirst,itodo,fsvalu,status)
  writefirst=writefirst+itodo
  readfirst=readfirst+itodo
  ntodo=ntodo-itodo
  itodo=min(256,ntodo)
 }

end
