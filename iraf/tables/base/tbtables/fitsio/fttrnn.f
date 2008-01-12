C----------------------------------------------------------------------
        logical function fttrnn(value)

C       test if a R*4 value has a IEEE Not-a-Number (NaN) value
C       A NaN has all the exponent bits=1, and the fractional part not=0.  
C       The exponent field occupies bits 23-30,  (least significant bit = 0)
C       The mantissa field occupies bits 0-22

C       This routine also sets any underflow values to zero.

C       written by Wm Pence, HEASARC/GSFC, May 1992
C       modified Aug 1994 to handle all IEEE special values.

        integer value

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

C       COMPID specifies what type of floating point word structure
C       is used on this machine, and determines how to test for NaNs.

C       COMPID value:
C           1   VAX or generic machine: simply test for NaNs with all bits set
C           2   like a decstation or alpha OSF/1, or IBM PC
C           3   SUN workstation, or IBM mainframe
C          -2305843009213693952   Cray (64-bit) machine

        fttrnn=.false.
        if (compid .eq. 1)then
C           on the VAX we can assume that all NaNs will be set to all bits on
C           (which is equivalent to an integer with a value of -1) because
C           this is what the IEEE to VAX conversion MACRO program returns
            if (value .eq. -1)fttrnn=.true.
        else if (compid .gt. 1)then
C           the following test works on all other machines (except Cray)
C           the sign bit may be either 1 or 0 so have to test both possibilites.
C           Note: overflows and infinities are also flagged as NaNs.
            if (value .ge. 2139095039 .or. (value .lt. 0 .and. 
     1             value .ge. -8388609))then
                   fttrnn=.true.
            else if ((value .gt. 0 .and. value .le. 8388608) .or.
     1             value .le. -2139095040)then
C                  set underflows and denormalized values to zero
                   value=0
            end if            
        else
C           branch for the Cray:  COMPID stores the negative integer
C           which corresponds to the 3 most sig digits set to 1.   If these
C           3 bits are set in a floating point number, then it represents
C           a reserved value (i.e., a NaN)
            if (value .lt. 0 .and. value .ge. compid)fttrnn=.true. 
        end if
        end
