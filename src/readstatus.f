

c		Copyright 1993 Colorado State University
c			All Rights Reserved



      subroutine readstatus (jfd_case, ensem_case)
      integer jfd_case, ensem_case

      include 'outval.inc'
c      include 'plot2.inc'
c**** Local variables
      integer ierr, id1, id2
      real ttime
      logical goahead

c          inquire(file="status_start.bin",exist=goahead)
  
c      if (goahead) then 
c      open(unit=19,file='status_start.bin',
c     $   form='UNFORMATTED',status='OLD')
c2345
c      endif

c****  search binary file until found or EOF
10     read(unit=19,end=20) id1, id2, ttime, vals1, vals2, vals3
        if (jfd_case .eq. id1 .and. ensem_case .eq. id2) then 
               goto 30
         endif
         goto 10
20     continue
      
30    print *, "======================== "
c      print *, "Previous Status: jfd, ensem_case"
c      print *, id1, id2, ttime, vals2
      close(19)


      return
      end
