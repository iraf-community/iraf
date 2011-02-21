include "fitsio.h"

procedure fsgtbs(iunit,frow,fchar,nchars,svalue,status)

# read a consecutive string of characters from an ascii or binary
# table. This will span multiple rows of the table if NCHARS+FCHAR is
# greater than the length of a row.

int     iunit           # i input file pointer
int     frow            # i first row
int     fchar           # i first character
int     nchars          # i number of characters
char    svalue[ARB]     # o string value
%       character fsvalu*256
int     status          # o error status
int	readfirst
int	writefirst
int	ntodo
int	itodo

begin

# since the string may be arbitrarily long, read it in pieces
readfirst=fchar
writefirst=1
ntodo=nchars
itodo=min(256,ntodo)

while (itodo > 0) {
  call ftgtbs(iunit,frow,readfirst,itodo,fsvalu,status)
  call fsupk(fsvalu,svalue[writefirst],itodo)
  writefirst=writefirst+itodo
  readfirst=readfirst+itodo
  ntodo=ntodo-itodo
  itodo=min(256,ntodo)
 }

end
