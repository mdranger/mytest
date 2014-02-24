
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine fertin(tomatch,curfert,savedfert)

      character*5 tomatch
      character*5 curfert
      real      savedfert

c...Read in the new fert type
c
      include 'fertil.inc'
      include 'const.inc'
      include 'chrvar.inc'

c...Local variables
      integer   i, FERTLNS
      real temp
      character fromdat*5, name*100, string*80


c...Number of lines to read for each fert type
      parameter (FERTLNS = 4)

      character*100 fixfile

      fixfile = ' '

      fixfile(1:pathlen) = libpath
      fixfile(pathlen+1:pathlen+9) = '\fert.100'

c      write(*,*) fixfile

      open(unit=11,file=fixfile,status='OLD')

c      open(unit=11, file='fert.100',status='OLD')
      rewind(11)
20    continue
c      read(11, 100, end=200) fromdat, name
      read(11, 100, end=200) fromdat
!...      print *, fromdat
      if (tomatch .ne. fromdat) then
        do 25 i = 1, FERTLNS
          read(11, *) temp, name
!...          print *, "temp", temp
25      continue
        goto 20
      else
        read(11, *) feramt(N), name 
        call ckdata('schedl','feramt',name)
        read(11, *) feramt(P), name 
        call ckdata('schedl','feramt',name)
        read(11, *) feramt(S), name 
        call ckdata('schedl','feramt',name)
        read(11, *) aufert, name 
        call ckdata('schedl','aufert',name)
        savedfert = aufert
        close(11)
        curfert = tomatch 
      endif


      return

100   format(a5)

200   continue
      call message('   Error reading in values from the fert.100 file.')
      string = '   Looking for fert type: ' // tomatch
      call message(string)
      STOP

      end
