C--------------------------------------------------------------------------
        subroutine ftcrep(comm,comm1,repeat)
        
C       check if the first comment string is to be repeated for all keywords
C       (if the last non-blank character is '&', then it is to be repeated)

C       comm    c  input comment string
C       OUTPUT PARAMETERS:
C       comm1   c  output comment string, = COMM minus the last '&' character
C       repeat  l  true if the last character of COMM was the '&" character
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        character*(*) comm,comm1
        logical repeat
        integer i,j

        repeat=.false.
        j=len(comm)
        do 10 i=j,1,-1
                if (comm(i:i) .ne. ' ')then
                        if (comm(i:i) .eq. '&')then
                                comm1=comm(1:i-1)
                                repeat=.true.
                        end if
                        return
                end if
10      continue
        end
