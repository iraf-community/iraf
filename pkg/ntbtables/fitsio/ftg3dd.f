C--------------------------------------------------------------------------
        subroutine ftg3dd(ounit,group,nulval,dim1,dim2,nx,ny,nz,
     &                    array,anyflg,status)

C       Read a 3-d cube of byte values from the primary array.
C       Data conversion and scaling will be performed if necessary
C       (e.g, if the datatype of the FITS array is not the same
C       as the array being read).

C       ounit   i  Fortran output unit number
C       group   i  number of the data group, if any
C       nulval  d  undefined pixels will be set to this value (unless = 0)
C       dim1    i  actual first dimension of ARRAY
C       dim2    i  actual second dimension of ARRAY
C       nx      i  size of the cube in the x direction
C       ny      i  size of the cube in the y direction
C       nz      i  size of the cube in the z direction
C       array   d  the array of values to be read
C       anyflg  l  set to true if any of the image pixels were undefined
C       status  i  returned error stataus

C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,group,dim1,dim2,nx,ny,nz,status
        double precision array(dim1,dim2,*),nulval
        logical anyflg,ltemp
        integer fpixel,row,band

        anyflg=.false.
        fpixel=1
        do 20 band=1,nz
        do 10 row = 1,ny
                call ftgpvd(ounit,group,fpixel,nx,nulval,
     &              array(1,row,band),ltemp,status)
                if (ltemp)anyflg=.true.
                fpixel=fpixel+nx
10      continue
20      continue
        end
