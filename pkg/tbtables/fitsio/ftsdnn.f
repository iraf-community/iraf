C----------------------------------------------------------------------
        subroutine ftsdnn(value)

C       set a 64-bit pattern equal to an IEEE Not-a-Number value
C       A NaN has all the exponent bits=1, and the fractional part 
C       not=0.  
C
C       written by Wm Pence, HEASARC/GSFC, February 1991

        integer value(2)

C       there are many NaN values;  choose a simple one in which all bits=1
        value(1)=-1
        value(2)=-1
        end
