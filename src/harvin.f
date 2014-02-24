
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


C...HARVIN.F

      subroutine harvin(tomatch,curharv)

      character*5 tomatch
      character*5 curharv

c...Read in the new harvest type

      include 'parcp.inc'
      include 'param.inc'
      include 'chrvar.inc'

c...Local variables
      integer   i, HARVLNS
      real      temp
      character fromdat*5, name*20, string*80


c...Number of lines to read for each harv type
      parameter (HARVLNS = 6)

      character*100 fixfile

      fixfile = ' '

      fixfile(1:pathlen) = libpath
      fixfile(pathlen+1:pathlen+9) = '\harv.100'

c      write(*,*) fixfile

      open(unit=11,file=fixfile,status='OLD')

c      open(unit=11, file='harv.100',status='OLD')
      rewind(11)
20    continue
      read(11, 100, end=200) fromdat
      if (tomatch .ne. fromdat) then 
        do 25 i = 1, HARVLNS
          read(11, *) temp, name
25      continue
        goto 20
      else
        read(11, *) aglrem, name
        call ckdata('schedl','aglrem',name)
        read(11, *) bglrem, name 
        call ckdata('schedl','bglrem',name)
        read(11, *) temp, name
        flghrv = int(temp)
        call ckdata('schedl','flghrv',name)
        read(11, *) rmvstr, name 
        call ckdata('schedl','rmvstr',name)
        read(11, *) remwsd, name 
        call ckdata('schedl','remwsd',name)
        read(11, *) hibg, name 
        call ckdata('schedl','hibg',name)
        close(11)
        curharv = tomatch 
      endif


      return

100   format(a5)

200   continue
      call message('   Error reading in values from the harv.100 file.')
      string = '   Looking for harvest type: ' // tomatch
      call message(string)
      STOP

      end
