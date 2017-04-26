C------------------------------------------------------------------------------
        subroutine ftarch(compid)

C       This routine looks at how integers and reals are internally
C       stored, to figure out what kind of machine it is running on.

C       compid = 1  -  VAX or Alpha VMS system
C                2  -  Decstation or Alpha OSF/1, or IBM PC
C                3  -  SUN workstation 
C                4  -  IBM mainframe

        integer compid
        real rword
        integer*2 iword(2)
        equivalence (rword, iword)

C       set rword to some arbitrary value
        rword=1.1111111111

C       Then look at the equivalent integer, to distinquish the machine type.
C       The machine type is needed when testing for NaNs.

        if (iword(1) .eq. 16270)then
C               looks like a SUN workstation (uses IEEE word format)
                compid=3
        else if (iword(1) .eq. 14564)then
C               looks like a Decstation, alpha OSF/1, or IBM PC (byte swapped)
                compid=2
        else if (iword(1) .eq. 16526)then
C               looks like a VAX or ALPHA VMS system
                compid=1
        else if (iword(1) .eq. 16657)then
C               an IBM main frame (the test for NaNs is the same as on SUNs)
                compid=4
        else
C               unknown machine
                compid=0
                return
        end if
        end
