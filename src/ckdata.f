
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine ckdata(routin,expect,found)

      character*(*) routin, expect, found

c...Check a parameter name read by fixin or sitein.
c     routin is the name of the calling routine.
c     expect is the name that should have been read.
c     found is the name that was read from the data file.

c...Local variables
      integer i, loc, i_same
      character par*10, tempchar *10, string*80


c...Extract junk from the parameter name

      loc = index(found,'(')
      if (loc .eq. 0) then
        loc = index(found,'|')
        if (loc .eq. 0) then
            loc = index(found,',')
          endif
      endif
      if (loc .eq. 0) then
        par = found( :6)
      else
        par = found( :loc-1)
      endif

c...Convert to lower case if needed (MACHINE DEPENDANT)
      do 100 i = 1, 6
         if (par(i:i) .ge. 'A' .and. par(i:i) .le. 'Z') then
           par(i:i) = char( ichar(par(i:i)) + 32 )
         endif
100   continue

      i_same = 1;
      
      do 200 i = 1, len(expect)
         if (par(i:i) .ne. expect(i:i)) then
           i_same = 0;
         endif
200   continue

c.  Trim the blanks before or after the variable
c...Test name for expected name
c...if (expect .ne. trim(par)) then
      if (i_same .eq. 0 ) then
      if (routin .eq. 'fixin') then
          call message('   There is an error in your fix.100 file.')
      else if (routin .eq. 'sitein') then
          call message('   There is an error in your <site>.100 file.')
      endif
        string = '   The data for _' // par // '_ was read when the ' //
     +           'data for _' // expect // '_ was expected.'
      write(*,"('par len ',i2)") len_trim(par)
      write(*,"('exp len ',i2)") len(expect)
      call message(string)
c...Comments out these since g77 not recongize the trim function
c...  string = 'trim result _' // trim(par) // '_ '
c...  call message(string)
c...      tempchar = trim(par)
c...      string = 'trim _' // tempchar // TRIM('wait  ') // '_ end'
c...   call message(string)
      
        STOP
      endif

      return
      end
