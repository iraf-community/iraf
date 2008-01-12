include "fitsio.h"

procedure fsgcfs(iunit,colnum,frow,felem,nelem,array,dim1,
          flgval,anynul,status)

# read an array of string values from a specified column of the table.
# Any undefined pixels will be have the corresponding value of FLGVAL
# set equal to .true., and ANYNUL will be set equal to .true. if
# any pixels are undefined.

int     iunit           # i input file pointer
int     colnum          # i column number
int     frow            # i first row
int     felem           # i first element in row
int     nelem           # i number of elements
char    array[dim1,ARB]      # o array of values
%       character farray*256
int	dim1		# i size of 1st dimension of 2D character string array
bool    flgval[ARB]     # o is corresponding value undefined?
bool    anynul          # o any null values?
int     status          # o error status
int	i
int	elem
bool    null

begin

anynul=false
elem=felem
do i=1,nelem    {
  call ftgcvs(iunit,colnum,frow,elem,1,farray,flgval(i),null,status)
  if (null)
     anynul=true

  call f77upk(farray,array(1,i),dim1)
  elem=elem+1
 }
end
