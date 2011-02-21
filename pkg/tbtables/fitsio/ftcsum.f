C--------------------------------------------------------------------------
        subroutine ftcsum(iunit,nrec,sum,status)

C       Calculate a 32-bit 1's complement checksum of the FITS 2880-byte blocks.
C       This Fortran algorithm is based on the C algorithm developed by Rob
C       Seaman at NOAO that was presented at the 1994 ADASS conference, to be 
C       published in the Astronomical Society of the Pacific Conference Series.

C       This uses a 32-bit 1's complement checksum in which the overflow bits
C       are permuted back into the sum and therefore all bit positions are
C       sampled evenly.  In this Fortran version of the original C algorithm,
C       a double precision value (which has at least 48 bits of precision)
C       is used to accumulate the checksum because standard Fortran does not 
C       support an unsigned integer datatype.

C       iunit   i  fortran unit number
C       nrec    i  number of FITS 2880-byte blocks to be summed
C       sum     d  check sum value (initialize to zero before first call)
C       status  i  output error status
C
C       written by Wm Pence, HEASARC/GSFC, Sept, 1994

        integer iunit,nrec,status,i,j,hibits,i4vals(720)
        double precision sum,word32
        parameter (word32=4.294967296D+09)
C       word32 is equal to 2**32

        if (status .gt. 0)return

C       Sum the specified number of FITS 2880-byte records.  This assumes that
C       the FITSIO file pointer points to the start of the records to be summed.
        do 30 j=1,nrec

C           read the record as 720 pixel I*4 vector (do byte swapping if needed)
            call ftgi4b(iunit,720,0,i4vals,status)
            do 10 i=1,720
                if (i4vals(i) .ge. 0)then
                        sum=sum+i4vals(i)
                else
C                       sign bit is set, so add the equalvalent unsigned value
                        sum=sum+(word32+i4vals(i))
                end if
10          continue

C           fold any overflow bits beyond 32 back into the word
20          hibits=sum/word32
            if (hibits .gt. 0)then
                sum=sum-(hibits*word32)+hibits
                go to 20
            end if
30      continue
        end
