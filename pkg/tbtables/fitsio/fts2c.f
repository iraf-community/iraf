C----------------------------------------------------------------------
        subroutine fts2c(in,cval,lenval,status)
C       convert an input string to a left justified quoted string
C               The minimum length FITS string is 8 characters, so
C               pad the quoted string with spaces if necessary.
C       cval = returned quoted string
C       lenval = length of the cval string, including the 2 quote characters
        character*(*) in,cval
        integer length,i,j,i1,i2,lenval,status

        if (status .gt. 0)return

        i1=1
        i2=1
C       test for blank input string
        if (in .eq. ' ')then
                cval='''        '''
                lenval=10
                return
        end if

        length=len(in)
C       find first and last non-blank characters

C       modified 29 Nov 1994 to treat leading spaces as significant
C        do 5 i=1,length
C                i1=i
C                if (in(i:i) .ne. ' ')go to 10
C5       continue
C10      continue

        do 15 i=length,1,-1
                i2=i
                if (in(i:i) .ne. ' ')go to 20
15      continue
20      continue

        cval=''''//in(i1:i2)

C       test if there are any single quotes in the string;  if so, replace
C       them with two successive single quotes
        lenval=i2-i1+2
        do 30 i=lenval,2,-1
                if (cval(i:i) .eq. '''')then
C                  shift all the characters over 1 space
                   do 40 j=len(cval),i+1,-1
                      cval(j:j)=cval(j-1:j-1)
40                 continue
                   i2=i2+1
                end if
30      continue

C       find location of closing quote
        lenval=max(10,i2-i1+3)  
        lenval=min(lenval,len(cval))
        cval(lenval:lenval)=''''
        end
