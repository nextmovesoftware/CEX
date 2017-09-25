      SUBROUTINE PARSE(string,nargs,argptr,maxargs)
c======================================================================
c  Parse string into individual arguments, each separated by 1 or more
c  spaces.  Returns number of arguments 'nargs', and pointers to the 
c  first and last character of argument i in argptr(1,i) and 
c  argptr(2,i), respectively.
c----------------------------------------------------------------------
      character*(*) string
      integer       argptr(2,maxargs)
      integer       nargs

      lstring = lenstr(string)
      nargs   = 0
      iptr    = 0
      do while(iptr.lt.lstring)
         iptr = iptr+1
         if (string(iptr:iptr).ne.' ') then
            jptr  = index(string(iptr:lstring),' ')
            if (jptr.gt.0) then
               jptr = jptr+iptr-2
            else
               jptr = lstring
            end if
            nargs = nargs+1
            argptr(1,nargs) = iptr
            argptr(2,nargs) = jptr
            iptr = jptr
         end if
      end do
      return
      end
