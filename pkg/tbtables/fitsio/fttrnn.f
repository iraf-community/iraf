C----------------------------------------------------------------------
        logical function fttrnn(value)

C       test if a R*4 value has a IEEE Not-a-Number (NaN) value
C       A NaN has all the exponent bits=1, and the fractional part not=0.  
C       The exponent field occupies bits 23-30,  (least significant bit = 0)
C       The mantissa field occupies bits 0-22

C       This routine also sets any underflow values to zero.

C       written by Wm Pence, HEASARC/GSFC, May 1992
C       modified Aug 1994 to handle all IEEE special values.

        real*4 value

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

        real*4  rtiny
        parameter (rtiny = 1.17549435E-38)  ! Smallest normalized IEEE float

C       COMPID specifies what type of floating point word structure
C       is used on this machine, and determines how to test for NaNs.

C       COMPID value:
C           1   VAX or generic machine: simply test for NaNs with all bits set
C           2   like a decstation or alpha OSF/1, or IBM PC
C           3   SUN workstation, or IBM mainframe
C          -2305843009213693952   Cray (64-bit) machine

        fttrnn=.false.
        if (value .ne. value) then
            fttrnn=.true.
        end if

        if (abs(value) .gt. 0 .and. abs(value) .lt. rtiny) then
            value = 0.0
        end if

        return
        end
