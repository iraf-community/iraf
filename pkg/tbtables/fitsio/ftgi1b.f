C----------------------------------------------------------------------
        subroutine ftgi1b(ounit,nvals,incre,chbuff,status)

C       Read an array of Integer*1 bytes from the input FITS file.

        integer nvals,incre,ounit,status,i,offset
        character*1 chbuff(nvals)

C       ounit   i  fortran unit number
C       nvals   i  number of pixels in the i2vals array
C       incre   i  byte increment between values
C       chbuff  c*1 array of input byte values
C       status  i  output error status

        if (incre .le. 1)then
                call ftgcbf(ounit,0,nvals,chbuff,status)
        else
C               offset is the number of bytes to move between each value
                offset=incre-1
                call ftgcbf(ounit,0,1,chbuff,status)
                do 10 i=2,nvals
                        call ftmoff(ounit,offset,.false.,status)
                        call ftgcbf(ounit,0,1,chbuff(i),status)
10              continue
        end if
        end
