C----------------------------------------------------------------------
        logical function fttdnn(value)

C       test if a R*8 value has a IEEE Not-a-Number value
C       A NaN has all the exponent bits=1, and the fractional part
C       not=0. 
C       Exponent field is in bits 20-30 in the most significant 4-byte word
C       Mantissa field is in bits 0-19 of most sig. word and entire 2nd word
C
C       written by Wm Pence, HEASARC/GSFC, May 1992
C       modified Aug 1994 to handle all IEEE special values.

        integer value(2)

C       COMMON BLOCK DEFINITIONS:--------------------------------------------
        integer nb,ne
        parameter (nb = 20)
        parameter (ne = 200)
        integer bufnum,chdu,hdutyp,maxhdu,hdstrt,hdend,nxthdr,dtstrt
        integer nxtfld
        logical wrmode
        common/ft0001/bufnum(199),chdu(nb),hdutyp(nb),maxhdu(nb),
     &  wrmode(nb),hdstrt(nb,ne),hdend(nb),nxthdr(nb),dtstrt(nb),nxtfld
        integer compid
        common/ftcpid/compid
C       END OF COMMON BLOCK DEFINITIONS-----------------------------------

        integer word1,word2
C       COMPID specifies what type of floating point word structure
C       is used on this machine, and determines how to test for NaNs.

C       COMPID value:
C           1        generic machine: simply test for NaNs with all bits set
C           2        like a decstation or alpha OSF/1, or IBM PC
C           3        SUN workstation, or IBM mainframe
C          -2305843009213693952   Cray (64-bit) machine

        fttdnn=.false.
        if (compid .eq. 1)then
C           on the VAX we can assume that all NaNs will be set to all bits on
C           (which is equivalent to an integer with a value of -1) because
C           this is what the IEEE to VAX conversion MACRO program returns
            if (value(1) .eq. -1 .and. value(2) .eq. -1)fttdnn=.true.
        else if (compid .gt. 1)then
            if (compid .ge. 3)then
C               this is for SUN-like machines, or IBM main frames
                word1=value(1)
                word2=value(2)
            else
C               this is for DECstation and IBM PCs.  The 2 32 bit integer words
C               are reversed from what you get on the SUN.
                word1=value(2)
                word2=value(1)
            end if

C           efficiently search the number space for NaNs and underflows
            if (word2 .eq. -1)then
                if ((word1 .ge. -1048577 .and. word1 .le. -1)
     &           .or. (word1 .ge. 2146435071))then
                      fttdnn=.true.
                else if ((word1 .lt. -2146435072) .or.
     &          (word1 .ge. 0 .and. word1 .lt. 1048576))then
                      value(1)=0
                      value(2)=0
                end if
             else if (word2 .eq. 0)then
                if ((word1 .gt. -1048577 .and. word1 .le. -1)
     &           .or. (word1 .gt. 2146435071))then
                      fttdnn=.true.
                else if ((word1 .le. -2146435072) .or.
     &          (word1 .ge. 0 .and. word1 .le. 1048576))then
                      value(1)=0
                      value(2)=0
                end if
             else
                if ((word1 .gt. -1048577 .and. word1 .le. -1)
     &           .or. (word1 .gt. 2146435071))then
                      fttdnn=.true.
                else if ((word1 .lt. -2146435072) .or.
     &          (word1 .ge. 0 .and. word1 .lt. 1048576))then
                      value(1)=0
                      value(2)=0
                end if
             end if
        else
C           branch for the Cray:  COMPID stores the negative integer
C           which corresponds to the 3 most sig digits set to 1.   If these
C           3 bits are set in a floating point number, then it represents
C           a reserved value (i.e., a NaN)
            if (value(1).lt. 0 .and. value(1) .ge. compid)fttdnn=.true.        
        end if
        end
