C--------------------------------------------------------------------------
        subroutine ftpkys(ounit,keywrd,strval,comm,status)

C       write a character string value to a header record 
C
C       ounit   i  fortran output unit number
C       keywrd  c  keyword name    ( 8 characters, cols.  1- 8)
C       strval  c  keyword value 
C       comm    c  keyword comment (47 characters, cols. 34-80)
C       OUTPUT PARAMETERS
C       status  i  output error status (0 = ok)
C
C       written by Wm Pence, HEASARC/GSFC, June 1991
C       modified 6/93 to handle long string values by continuing the
C       string onto subsequent comment keywords (with a blank keyword name)

C       Modified again in 9/94 to remove support for long string values;
C       Now, one must call ftpkls to write a long string values.

        character*(*) keywrd,comm,strval
        integer ounit,status,lenval,ncomm,nvalue
        character strtmp*68,value*70,keynam*8,cmnt*48
        
        if (status .gt. 0)return

        strtmp=strval
        keynam=keywrd
        cmnt=comm

C       convert string to quoted character string (max length = 70 characters)
        call fts2c(strtmp,value,lenval,status)

        if (lenval .gt. 70)then
C           truncate the string to 70 characters (if the input string contained
C           apostrophies, then it could get expanded to more than 70 characters)
            value(70:70)=''''
            lenval=70
C           N.B. there could be a problem here if character 69 is also a '.
C           Then the closing quote would be considered a literal appostrophy.
        end if

C       find amount of space left for comment string
C       (assume 10 char. for 'keyword = ', and 3 between value and comment)
C       which leaves 67 spaces for the value string + comment string
        nvalue=max(20,lenval)
        ncomm=67-nvalue

C       write the keyword record
        if (ncomm .gt. 0)then
C         there is space for a comment
          call ftprec(ounit,
     &    keynam//'= '//value(1:nvalue)//' / '//cmnt(1:ncomm),status)
        else
C         no room for a comment
          call ftprec(ounit,
     &    keynam//'= '//value(1:nvalue)//'   ',status)
        end if  
        end
