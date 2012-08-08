C--------------------------------------------------------------------------
        subroutine ftesum(sum,complm,string)

C       encode the 32 bit checksum by converting every 
C       2 bits of each byte into an ASCII character (32 bit word encoded 
C       as 16 character string).   Only ASCII letters and digits are used
C       to encode the values (no ASCII punctuation characters).

C       If complm=.true., then the complement of the sum will be encoded.

C       This Fortran algorithm is based on the C algorithm developed by Rob
C       Seaman at NOAO that was presented at the 1994 ADASS conference, to be 
C       published in the Astronomical Society of the Pacific Conference Series.
C     
C       sum     d  checksum value 
C       complm  l  encode the complement of the sum?
C       string  c  output ASCII encoded check sum
C
C       written by Wm Pence, HEASARC/GSFC, Sept, 1994

        double precision sum,tmpsum,all32
        character*16 string,tmpstr
        integer offset,exclud(13),nbyte(4),ch(4),i,j,k
        integer quot,remain,check,nc
        logical complm

C       all32 equals a 32 bit unsigned integer with all bits set
        parameter (all32=4.294967295D+09)

C       ASCII 0 is the offset value
        parameter (offset=48)

C       this is the list of ASCII punctutation characters to be excluded
        data exclud/58,59,60,61,62,63,64,91,92,93,94,95,96/

        if (complm)then
C           complement the 32-bit unsigned integer equivalent (flip every bit)
            tmpsum=all32-sum
        else
            tmpsum=sum
        end if
                           
C       separate each 8-bit byte into separate integers
        nbyte(1)=tmpsum/16777216.
        tmpsum=tmpsum-nbyte(1)*16777216.
        nbyte(2)=tmpsum/65536.
        tmpsum=tmpsum-nbyte(2)*65536.
        nbyte(3)=tmpsum/256.
        nbyte(4)=tmpsum-nbyte(3)*256.

C       encode each 8-bit integer as 4-characters 
        do 100 i=1,4
                quot=nbyte(i)/4+offset
                remain=nbyte(i) - (nbyte(i)/4*4)
                ch(1)=quot+remain
                ch(2)=quot
                ch(3)=quot
                ch(4)=quot

C               avoid ASCII punctuation characters by incrementing and
C               decrementing adjacent characters thus preserving checksum value
10              check=0
                    do 30 k=1,13
                        do 20 j=1,4,2
                           if (ch(j)   .eq. exclud(k) .or.
     &                         ch(j+1) .eq. exclud(k))then
                               ch(j)=ch(j)+1
                               ch(j+1)=ch(j+1)-1
                               check=1
                           end if
20                      continue
30                  continue

C               keep repeating, until all punctuation character are removed
                if (check .ne. 0)go to 10

C               convert the byte values to the equivalent ASCII characters
                do 40 j=0,3
                    nc=4*j+i
                    tmpstr(nc:nc)=char(ch(j+1))
40              continue
100     continue

C       shift the characters 1 place to the right, since the FITS character
C       string value starts in column 12, which is not word aligned
        string(2:16)=tmpstr(1:15)
        string(1:1)=tmpstr(16:16)

C       convert characters from ASCII codes to machine's native character
C       coding sequence.  (The string gets converted back to ASCII when it
C       is written to the FITS file). This only affects IBM mainframe computers 
C       that do not use ASCII for the internal character representation.
        call ftas2c(string,16)
        end
