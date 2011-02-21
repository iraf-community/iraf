C--------------------------------------------------------------------------
        subroutine ftdsum(string,complm,sum)

C       decode the 32 bit checksum

C       If complm=.true., then the complement of the sum will be decoded.

C       This Fortran algorithm is based on the C algorithm developed by Rob
C       Seaman at NOAO that was presented at the 1994 ADASS conference, to be 
C       published in the Astronomical Society of the Pacific Conference Series.
C     
C       sum     d  checksum value 
C       complm  l  encode the complement of the sum?
C       string  c  output ASCII encoded check sum
C       sum     d  checksum value 
C
C       written by Wm Pence, HEASARC/GSFC, May, 1995

        double precision sum,all32,word32,factor(4)
        character*16 string,tmpstr
        integer offset,i,j,k,temp,hibits
        logical complm

C       all32 equals a 32 bit unsigned integer with all bits set
C       word32 is equal to 2**32
        parameter (all32=4.294967295D+09)
        parameter (word32=4.294967296D+09)

C       ASCII 0 is the offset value
        parameter (offset=48)

        data factor/16777216.,65536.,256.,1./

        sum=0

C       shift the characters 1 place to the left, since the FITS character
C       string value starts in column 12, which is not word aligned
        tmpstr(1:15)=string(2:16)
        tmpstr(16:16)=string(1:1)

C       convert characters from machine's native character coding sequence
C       to ASCII codes.   This only affects IBM mainframe computers 
C       that do not use ASCII for the internal character representation.
        call ftc2as(tmpstr,16)

C       substract the offset from each byte and interpret each 4 character
C       string as a 4-byte unsigned integer; sum the 4 integers
        k=0
        do 10 i=1,4
          do 20 j=1,4
            k=k+1
            temp=ichar(tmpstr(k:k))-offset
            sum=sum+temp*factor(j)
20        continue
10      continue

C       fold any overflow bits beyond 32 back into the word
30      hibits=sum/word32
        if (hibits .gt. 0)then
                sum=sum-(hibits*word32)+hibits
                go to 30
         end if

        if (complm)then
C           complement the 32-bit unsigned integer equivalent (flip every bit)
            sum=all32-sum
        end if
        end
