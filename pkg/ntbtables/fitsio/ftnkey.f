C--------------------------------------------------------------------------
        subroutine ftnkey(nseq,keywrd,keyout,status)

C       Make a keyword name by concatinating a sequence number and
C       the root name. (Sequence number is prepended to the name)

C       nseq    i  sequence number 
C       keywrd  c  root keyword name
C       OUTPUT PARAMETERS:
C       keyout  c  output concatinated keyword name
C       status  i  output error status (0 = ok)
C       
C       written by Wm Pence, HEASARC/GSFC, Aug 1994

        character*(*) keywrd
        integer nseq,status,nspace,i
        character keyout*8,value*20,work*8
        
        work=keywrd

C       find end of keyword string
        nspace=0
        do 10 i=8,1,-1
                if (work(i:i) .ne. ' ')go to 15
                nspace=nspace+1
10      continue
15      continue

C       prepend sequence number to keyword root only if there is room
        if (nseq .lt. 0)then
C               illegal value
                go to 900
        else if (nseq .lt. 10 .and. nspace .ge. 1)then
                write(keyout,1001,err=900)nseq,work(1:7)
        else if (nseq .lt. 100 .and. nspace .ge. 2)then
                write(keyout,1002,err=900)nseq,work(1:6)
        else if (nseq .lt. 1000 .and. nspace .ge. 3)then
                write(keyout,1003,err=900)nseq,work(1:5)
        else if (nseq .lt. 10000 .and. nspace .ge. 4)then
                write(keyout,1004,err=900)nseq,work(1:4)
        else if (nseq .lt. 100000 .and. nspace .ge. 5)then
                write(keyout,1005,err=900)nseq,work(1:3)
        else if (nseq .lt. 1000000 .and. nspace .ge. 6)then
                write(keyout,1006,err=900)nseq,work(1:2)
        else if (nseq .lt. 10000000 .and. nspace .ge. 7)then
                write(keyout,1007,err=900)nseq,work(1:1)
        else
C               number too big to fit in keyword
                go to 900
        end if

1001    format(i1,a7)
1002    format(i2,a6)
1003    format(i3,a5)
1004    format(i4,a4)
1005    format(i5,a3)
1006    format(i6,a2)
1007    format(i7,a1)

        return
C       come here if error concatinating the seq. no. to the root string
900     continue

        if (status .gt. 0)return
        status=206
        write(value,1008)nseq
1008    format(i20)
        call ftpmsg('Could not concatinate the integer '//value//
     & ' and the root keyword named: '//work)
        end
