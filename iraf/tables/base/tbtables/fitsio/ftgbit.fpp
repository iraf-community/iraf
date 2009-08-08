C----------------------------------------------------------------------
        subroutine ftgbit(buffer,log8)

C       decode the individual bits within the byte into an array of 
C       logical values.  The corresponding logical value is set to 
C       true if the bit is set to 1.

C       buffer  i  input integer containing the byte to be decoded
C       log8    l  output array of logical data values corresponding 
C                  to the bits in the input buffer
C
C       written by Wm Pence, HEASARC/GSFC, May 1992

        integer buffer,tbuff
        logical log8(8)

        log8(1)=.false.
        log8(2)=.false.
        log8(3)=.false.
        log8(4)=.false.
        log8(5)=.false.
        log8(6)=.false.
        log8(7)=.false.
        log8(8)=.false.

C       test for special case: no bits are set
        if (buffer .eq. 0)return

C       This algorithm tests to see if each bit is set by testing
C       the numerical value of the byte, starting with the most significant
C       bit.  If the bit is set, then it is reset to zero before testing
C       the next most significant bit, and so on.

        tbuff=buffer

C       now decode the least significant byte
        if (tbuff .gt. 127)then
                log8(1)=.true.
                tbuff=tbuff-128
        end if
        if (tbuff .gt. 63)then
                log8(2)=.true.
                tbuff=tbuff-64
        end if
        if (tbuff .gt. 31)then
                log8(3)=.true.
                tbuff=tbuff-32
        end if
        if (tbuff .gt. 15)then
                log8(4)=.true.
                tbuff=tbuff-16
        end if
        if (tbuff .gt. 7)then
                log8(5)=.true.
                tbuff=tbuff-8
        end if
        if (tbuff .gt. 3)then
                log8(6)=.true.
                tbuff=tbuff-4
        end if
        if (tbuff .gt. 1)then
                log8(7)=.true.
                tbuff=tbuff-2
        end if
        if (tbuff .eq. 1)then
                log8(8)=.true.
        end if
        end            
