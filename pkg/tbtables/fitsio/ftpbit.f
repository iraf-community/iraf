C----------------------------------------------------------------------
        subroutine ftpbit(setbit,wrbit,buffer)

C       encode the individual bits within the byte as specified by
C       the input logical array. The corresponding bit is set to 
C       1 if the logical array element is true.  Only the bits
C       between begbit and endbit, inclusive, are set or reset;
C       the remaining bits, if any, remain unchanged.

C       setbit  l  input array of logical data values corresponding 
C                  to the bits to be set in the output buffer
C                  TRUE means corresponding bit is to be set.
C       wrbit   l  input array of logical values indicating which
C                  bits in the byte are to be modified.  If FALSE,
C                  then the corresponding bit should remain unchanged.
C       buffer  i  output integer containing the encoded byte
C
C       written by Wm Pence, HEASARC/GSFC, May 1992

        integer buffer,tbuff,outbit
        logical setbit(8),wrbit(8)

        outbit=0
        tbuff=buffer

C       test each of the 8 bits, starting with the most significant
        if (tbuff .gt. 127)then
C           the bit is currently set in the word
            if (wrbit(1) .and. (.not.setbit(1)))then
C                only in this case do we reset the bit
            else
C               in all other cases we want the bit to be set
                outbit=outbit+128
            end if
            tbuff=tbuff-128
        else
C           bit is currently not set; set it only if requested to
            if (wrbit(1) .and. setbit(1))outbit=outbit+128
        end if

        if (tbuff .gt. 63)then
            if (wrbit(2) .and. (.not.setbit(2)))then
            else
                outbit=outbit+64
            end if
            tbuff=tbuff-64
        else
            if (wrbit(2) .and. setbit(2))outbit=outbit+64
        end if

        if (tbuff .gt. 31)then
            if (wrbit(3) .and. (.not.setbit(3)))then
            else
                outbit=outbit+32
            end if
            tbuff=tbuff-32
        else
            if (wrbit(3) .and. setbit(3))outbit=outbit+32
        end if

        if (tbuff .gt. 15)then
            if (wrbit(4) .and. (.not.setbit(4)))then
            else
                outbit=outbit+16
            end if
            tbuff=tbuff-16
        else
            if (wrbit(4) .and. setbit(4))outbit=outbit+16
        end if

        if (tbuff .gt. 7)then
            if (wrbit(5) .and. (.not.setbit(5)))then
            else
                outbit=outbit+8
            end if
            tbuff=tbuff-8
        else
            if (wrbit(5) .and. setbit(5))outbit=outbit+8
        end if

        if (tbuff .gt. 3)then
            if (wrbit(6) .and. (.not.setbit(6)))then
            else
                outbit=outbit+4
            end if
            tbuff=tbuff-4
        else
            if (wrbit(6) .and. setbit(6))outbit=outbit+4
        end if

        if (tbuff .gt. 1)then
            if (wrbit(7) .and. (.not.setbit(7)))then
            else
                outbit=outbit+2
            end if
            tbuff=tbuff-2
        else
            if (wrbit(7) .and. setbit(7))outbit=outbit+2
        end if

        if (tbuff .eq. 1)then
            if (wrbit(8) .and. (.not.setbit(8)))then
            else
                outbit=outbit+1
            end if
        else
            if (wrbit(8) .and. setbit(8))outbit=outbit+1
        end if

        buffer=outbit
        end            
