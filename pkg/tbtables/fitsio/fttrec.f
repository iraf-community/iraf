C----------------------------------------------------------------------
        subroutine fttrec(string,status)

C       test the remaining characters in a header record to insure that
C       it contains only pri-ntable ASCII characters,
C       i.e., with ASCII codes greater than or equal to 32 (a blank)
C       Note: this will not detect the delete character (ASCII 127)
C       because of the difficulties in also supporting this check
C       on IBM mainframes, where the collating sequence is entirely
C       different.

C       string  c*72 keyword name
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)

C       optimized in 7/93 to compare "ichar(string(i:i)) .lt. space"
C       rather than                       "(string(i:i)) .lt. ' ' "
C       This is much faster on SUNs and DECstations,
C       and decreases the time needed to write a keywor (ftprec) by 10%.
C       This change made no difference on a VAX

        integer space
C       The following line won't compile with the Lahey compiler on a PC
C        parameter(space = ichar(' '))
        character string*(*)
        integer status,i
        character pos*2

        if (status .gt. 0)return
        space=ichar(' ')

        do 20 i=1,72
            if (ichar(string(i:i)) .lt. space)then
C                 illegal character found
                  status=207
                  write(pos,1000)i
1000              format(i2)
        call ftpmsg('Character #'//pos//' in this keyword value or '//
     &  'comment string is illegal:')
        call ftpmsg(string)
                  return
            end if
20      continue
        end
