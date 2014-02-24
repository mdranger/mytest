

!		Copyright 1993 Colorado State University
!			All Rights Reserved
!  12/29/2009  Add one input parameter to decide the output format
! of the data
!


      subroutine wrtbin(jfd_case, ensem_case, time, outflag)
      real time
      integer jfd_case, ensem_case, outflag

      include 'outval.inc'

!**** Local variables
      integer ierr

!
!**** Write all output values to the binary file
      if ( outflag .eq. 1) then
          write(unit=1,iostat=ierr) time,vals1,vals2,vals3,vals4, vals5
      else
          write(unit=1,iostat=ierr) jfd_case, ensem_case,
     - time,vals1,vals2,vals3
      endif

!**** Check ierr for an error on writing
      if (ierr .ne. 0) then
        call message('   ***Error on writing to binary file.')
      call message('      Is the disk flooded?')
        STOP 'Execution error.'
      endif

      return
      end
