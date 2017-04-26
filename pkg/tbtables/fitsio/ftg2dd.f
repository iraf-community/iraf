C--------------------------------------------------------------------------
        subroutine ftg2dd(ounit,group,nulval,dim1,nx,ny,
     &                    array,anyflg,status)

C       Read a 2-d image of r*8 values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  d  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       nx      i  size of the image in the x direction
C       ny      i  size of the image in the y direction
C       array   d  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,nx,ny,status
        double precision array(dim1,*),nulval
        logical anyflg,ltemp
        integer fpixel,row

        anyflg=.false.
        fpixel=1
        do 10 row = 1,ny
                call ftgpvd(ounit,group,fpixel,nx,nulval,
     &              array(1,row),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue

        end
