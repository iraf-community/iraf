C----------------------------------------------------------------------
        subroutine ftuscm(input,np,scaled,scale,zero,output)

C       unscale the array of complex numbers, prior to writing to the FITS file

C       input  d  array of complex numbers (pairs of real/imaginay numbers)
C       np     i  total number of values to scale (no. of pairs times 2)
C       scaled l  is the data scaled?
C       scale  d  scale factor
C       zero   d  offset
C       output d  output array

        integer np,i,j
        logical scaled
        double precision input(np),output(np)
        double precision scale,zero

        j=1
        if (scaled)then
            do 10 i=1,np/2
                output(j)=(input(j)-zero)/scale
                j=j+1
C               the imaginary part of the number is not offset!!
                output(j)=input(j)/scale
                j=j+1
10          continue
        else
            do 20 i=1,np
                output(i)=input(i)
20          continue
        end if
        end
