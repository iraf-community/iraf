C--------------------------------------------------------------------------
        subroutine ftkeyn(keywrd,nseq,keyout,status)

C       Make a keyword name by concatinating the root name and a 
C       sequence number

C       keywrd  c  root keyword name
C       nseq    i  sequence number 
C       OUTPUT PARAMETERS:
C       keyout  c  output concatinated keyword name
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, February 1991

        character*(*) keywrd
        integer nseq,status,nspace,i
        character keyout*8,value*20

        keyout=keywrd

C       find end of keyword string
        nspace=1
        do 10 i=1,8
                if (keyout(i:i) .eq. ' ')go to 15
                nspace=nspace+1
10      continue
15      continue

C       append sequence number to keyword root only if there is room
        if (nseq .lt. 0)then
C               illegal value
                go to 900
        else if (nseq .lt. 10 .and. nspace .le. 8)then
                write(keyout(nspace:nspace),1001,err=900)nseq
        else if (nseq .lt. 100 .and. nspace .le. 7)then
                write(keyout(nspace:nspace+1),1002,err=900)nseq
        else if (nseq .lt. 1000 .and. nspace .le. 6)then
                write(keyout(nspace:nspace+2),1003,err=900)nseq
        else if (nseq .lt. 10000 .and. nspace .le. 5)then
                write(keyout(nspace:nspace+3),1004,err=900)nseq
        else if (nseq .lt. 100000 .and. nspace .le. 4)then
                write(keyout(nspace:nspace+4),1005,err=900)nseq
        else if (nseq .lt. 1000000 .and. nspace .le. 3)then
                write(keyout(nspace:nspace+5),1006,err=900)nseq
        else if (nseq .lt. 10000000 .and. nspace .le. 2)then
                write(keyout(nspace:nspace+6),1007,err=900)nseq
        else
C               number too big to fit in keyword
                go to 900
        end if

1001    format(i1)
1002    format(i2)
1003    format(i3)
1004    format(i4)
1005    format(i5)
1006    format(i6)
1007    format(i7)

        return
C       come here if error concatinating the seq. no. to the root string
900     continue

        if (status .gt. 0)return
        status=206
        write(value,1008)nseq
1008    format(i20)
        call ftpmsg('Could not concatinate the integer '//value//
     & ' to the root keyword named: '//keyout)
        end
