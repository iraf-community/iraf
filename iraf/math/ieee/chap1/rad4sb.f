c
c-----------------------------------------------------------------------
c subroutine:  rad4sb
c used by subroutine radix4. never directly accessed by user.
c-----------------------------------------------------------------------
c
       subroutine rad4sb(ntype)
c
c      input: ntype = type of butterfly invoked
c      output: parameters used by subroutine radix4
c
       dimension ix(2996)
       common /xx/ix
       common ntypl,kkp,index,ixc
       if(ntype.eq.ntypl) go to 7
       ix(ixc)=0
       ix(ixc+1)=ntype
       ixc=ixc+2
       if(ntype.ne.4) go to 4
       indexp=(index-1)*9
       ix(ixc)=kkp+1
       ix(ixc+1)=indexp+1
       ixc=ixc+2
       go to 6
4      ix(ixc)=kkp+1
       ixc=ixc+1
6      ntypl=ntype
       return
7      if(ntype.ne.4) go to 8
       indexp=(index-1)*9
       ix(ixc)=kkp+1
       ix(ixc+1)=indexp+1
       ixc=ixc+2
       return
8      ix(ixc)=kkp+1
       ixc=ixc+1
       return
       end
