C----------------------------------------------------------------------
        subroutine fttkey(keynam,status)

C       test that keyword name contains only legal characters:
C         uppercase letters, numbers, hyphen, underscore, or space
C         (but no embedded spaces)

C       keynam  c*8  keyword name
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)

        character keynam*(*)
        integer status,i
        character*1 c1,pos
        logical spaces

        if (status .gt. 0)return

        spaces=.false.
        do 20 i=1,8
            c1=keynam(i:i)
            if ((c1 .ge. 'A' .and. c1 .le. 'Z') .or. 
     &          (c1 .ge. '0' .and. c1 .le. '9') .or.
     &           c1 .eq. '-' .or. c1 .eq. '_')then
                 if (spaces)then
C                   error: name contains embedded space
                    status=207
                    call ftpmsg('Keyword name contains embedded '//
     &              'space(s): '//keynam(1:8))
                    return
                 end if
            else if (c1 .eq. ' ')then
                 spaces=.true.
            else 
C                illegal character found
                 status=207
                 write(pos,1000)i
1000             format(i1)
                 call ftpmsg('Character '//pos//' in this keyword name'
     &           //' is illegal: "'//keynam(1:8)//'"')
C                explicitly test for the 2 most common cases:
                 if (ichar(c1) .eq. 0)then
                   call ftpmsg('(This is an ASCII NUL (0) character).')
                 else if (ichar(c1) .eq. 9)then
                   call ftpmsg('(This is an ASCII TAB (9) character).')
                 end if
                 return
            end if
20      continue
        end
