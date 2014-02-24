

c		Copyright 1993 Colorado State University
c			All Rights Reserved



      subroutine wrtstatus(jfd_case, ensem_case, time)
      real time
      integer jfd_case, ensem_case

      include 'outval.inc'

c**** Local variables
      integer ierr
      logical file_exist

c      open(unit=51,file='status_end.bin',
c     $   form='UNFORMATTED',status='UNKNOWN')

c**** Write all output values to the binary file
      write(unit=51,iostat=ierr) jfd_case, ensem_case, 
     - time,vals1,vals2,vals3

c**** Check ierr for an error on writing
      if (ierr .ne. 0) then
        call message('   ***Error on writing to status file.')
	call message('      Is the disk flooded?')
        STOP 'Execution error.'
      endif

c      print *, "Ending status: "
c      print *, time, jfd_case, ensem_case, vals2
      return
      end
