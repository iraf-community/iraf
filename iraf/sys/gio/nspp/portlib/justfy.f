      subroutine justfy (ichar,len,newlen)
      dimension       ichar(1)
      in = 0
      call ncgchr (1h ,1,1,iblank)
      do 102 i=1,len
         call ncgchr (ichar,len,i,jchar)
         if (in .ne. 0) go to 101
         if (jchar .eq. iblank) go to 102
  101    in = in+1
         call ncpchr (ichar,len,in,jchar)
  102 continue
      newlen = in
      return
      end
