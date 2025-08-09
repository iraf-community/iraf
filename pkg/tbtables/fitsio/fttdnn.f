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

C        integer value(2)
        real*8 value

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
C           1        generic machine: simply test for NaNs with all bits set
C           2        like a decstation or alpha OSF/1, or IBM PC
C           3        SUN workstation, or IBM mainframe
C          -2305843009213693952   Cray (64-bit) machine

        real*8  dtiny
        parameter (dtiny = 2.2250738585072014D-308)

        fttdnn=.false.
        if (value .ne. value) then
            fttdnn=.true.
        end if

        if (abs(value) .gt. 0 .and. abs(value) .lt. dtiny) then
            value = 0.0
        end if

	return
        end
