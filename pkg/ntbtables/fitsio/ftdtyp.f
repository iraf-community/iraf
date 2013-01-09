C----------------------------------------------------------------------
        subroutine ftdtyp(value,dtype,status)

C       determine datatype of a FITS value field
C       This assumes value field conforms to FITS standards and may not
C          detect all invalid formats.
C       value   c  input value field from FITS header record only,
C                  (usually the value field is in columns 11-30 of record)
C                  The value string is left justified.
C       dtype   c  output type (C,L,I,F) for Character string, Logical,
C                    Integer, Floating point, respectively
C
C       written by Wm Pence, HEASARC/GSFC, February 1991

        character*(*)value,dtype
        integer status

        if (status .gt. 0)return

        dtype=' '

        if (value(1:1) .eq. '''')then
C               character string
                dtype='C'
        else if (value(1:1).eq.'T' .or. value(1:1).eq.'F')then
C               logical
                dtype='L'
        else if (index(value,'.') .gt. 0)then
C               floating point
                dtype='F'
        else
C               assume it must be an integer, since it isn't anything else
                dtype='I'
        end if
        end
