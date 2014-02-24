
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


C...FIREIN.F

      subroutine firein(tomatch,curfire)

      character*5 tomatch
      character*5 curfire

c...Read in the new fire type
c
      include 'parcp.inc'
      include 'const.inc'
      include 'chrvar.inc'

c...Local variables
      integer   i, FIRELNS
      real      temp
      character fromdat*5, name*20, string*80


c...Number of lines to read for each fire type
      parameter (FIRELNS = 9)

      character*100 fixfile

      fixfile = ' '

      fixfile(1:pathlen) = libpath
      fixfile(pathlen+1:pathlen+9) = '\fire.100'

c      write(*,*) fixfile

      open(unit=11,file=fixfile,status='OLD')

c      open(unit=11, file='fire.100',status='OLD')
      rewind(11)
20    continue
      read(11, 100, end=200) fromdat
      if (tomatch .ne. fromdat) then 
        do 25 i = 1, FIRELNS 
          read(11, *) temp, name
25      continue
        goto 20
      else
        read(11, *) flfrem, name 
        call ckdata('schedl','flfrem',name)
        read(11, *) fdfrem(1), name 
        call ckdata('schedl','fdfrem',name)
        read(11, *) fdfrem(2), name 
        call ckdata('schedl','fdfrem',name)
        read(11, *) fret(N), name 
        call ckdata('schedl','fret',name)
        read(11, *) fret(P), name 
        call ckdata('schedl','fret',name)
        read(11, *) fret(S), name 
        call ckdata('schedl','fret',name)
        read(11, *) frtsh, name 
        call ckdata('schedl','frtsh',name)
        read(11, *) fnue(ABOVE), name 
        call ckdata('schedl','fnue',name)
        read(11, *) fnue(BELOW), name 
        call ckdata('schedl','fnue',name)
        close(11)
        curfire = tomatch 
      endif


      return

100   format(a5)

200   continue
      call message('   Error reading in values from the fire.100 file.')
      string = '   Looking for type: ' // tomatch
      call message(string)
      STOP

      end
