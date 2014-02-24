c               Copyright 1993 Colorado State University
c                       All Rights Reserved

C...CULTIN.F

      subroutine cultin(tomatch,curcult)

      character*5 tomatch
      character*5 curcult

c...Read in the new cult type
c
      include 'parcp.inc'
      include 'chrvar.inc'


c...Local variables
      integer i, CULTLNS
      real temp
      character fromdat*5, name*20, string*80

c...Number of lines to read for each cult type
      parameter (CULTLNS = 11)

      character*100 fixfile

      fixfile = ' '

      fixfile(1:pathlen) = libpath
      fixfile(pathlen+1:pathlen+9) = '\cult.100'

c      write(*,*) "cult position"
c      write(*,*) fixfile

      open(unit=11,file=fixfile,status='OLD')

c      open(unit=11, file='cult.100',status='OLD')
      rewind(11)
20    continue
      read(11, 100, end=200) fromdat
      if (tomatch .ne. fromdat) then 
        do 25 i = 1, CULTLNS
          read(11, *) temp, name
25      continue
        goto 20
      else
        read(11, *) cultra(1), name 
        call ckdata('schedl','cultra',name)
        read(11, *) cultra(2), name 
        call ckdata('schedl','cultra',name)
        read(11, *) cultra(3), name 
        call ckdata('schedl','cultra',name)
        read(11, *) cultra(4), name 
        call ckdata('schedl','cultra',name)
        read(11, *) cultra(5), name 
        call ckdata('schedl','cultra',name)
        read(11, *) cultra(6), name 
        call ckdata('schedl','cultra',name)
        read(11, *) cultra(7), name 
        call ckdata('schedl','cultra',name)
        read(11, *) clteff(1), name 
        call ckdata('schedl','clteff',name)
        read(11, *) clteff(2), name 
        call ckdata('schedl','clteff',name)
        read(11, *) clteff(3), name 
        call ckdata('schedl','clteff',name)
        read(11, *) clteff(4), name 
        call ckdata('schedl','clteff',name)
        close(11)
        curcult = tomatch 
      endif


      return

100   format(a5)

200   continue
      call message('   Error reading in values from the cult.100 file.')
      string = '   Looking for type: ' // tomatch
      call message(string)
      STOP

      end
