!     
! File:   floodin.f
! Author: zli
!
! Created on March 08, 2010, 1:07 PM
! read in the data in flod.100 file
!...floodin.F

      subroutine floodin(tomatch,curflood)

      character*5 tomatch
      character*5 curflood

!...Read in the new flooding type

      include 'wetland.inc'
      include 'chrvar.inc'

!...Local variables
      integer   i, FLOODLNS
      real      temp
      character fromdat*5, name*20, string*80

!...Number of lines to read for each flooding type
!...only 2 at this moment,
!...surfw surface water and time of water existing
      parameter (FLOODLNS =  2)

      character*100 floodfile

      floodfile = ' '

      floodfile(1:pathlen) = libpath
!...change the pathlen + 9 to pathlen + 10
      floodfile(pathlen+1:pathlen+10) = '\flood.100'
      open(unit=11,file=floodfile,status='OLD')

!      open(unit=11, file='flood.100',status='OLD')
      rewind(11)
20    continue

      read(11, 100, end=200) fromdat
      if (tomatch .ne. fromdat) then
        do 25 i = 1, FLOODLNS
          read(11, *) temp, name
25      continue
        goto 20
      else
        read(11, *) surfwater, name
        call ckdata('schedl','surfw',name)
        read(11, *) floodtime, name
        call ckdata('schedl','timew',name)
        close(11)
        curflood = tomatch
      endif

!      print *, 'read ', curflood, surfwater, floodtime
      return

100   format(a5)

200   continue
      call message(' Error reading in values from the flood.100 file.')
      string = '   Looking for type: ' // tomatch
      call message(string)
      STOP

      end

