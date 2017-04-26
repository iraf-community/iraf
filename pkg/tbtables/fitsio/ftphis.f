C--------------------------------------------------------------------------
        subroutine ftphis(ounit,histry,status)

C       write a HISTORY record to the FITS header
C
C       ounit   i  fortran output unit number
C       histry  c  input history string
C       OUTPUT PARAMETERS:
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991

        integer ounit,status,strlen,actlen,i,nkeys,c1,c2
        character*(*) histry
        character*80  rec

        if (status .gt. 0)return
                
C       find the length of the string, and write it out 70 characters at a time
        nkeys=1
        strlen=len(histry)
        actlen=strlen
        do 10 i=strlen,1,-1
                if (histry(i:i) .ne. ' ')then
                        actlen=i
                        go to 20
                end if
10      continue

20      c1=1
        c2=min(actlen,70)
        nkeys=(actlen-1)/70+1
        do 30 i=1,nkeys
                rec='HISTORY   '//histry(c1:c2)
                call ftprec(ounit,rec,status)
                c1=c1+70
                c2=min(actlen,c2+70)
30      continue
        end
