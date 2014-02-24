c               Copyright 1993 Colorado State University
c                       All Rights Reserved


C...IRRGIN.F

      subroutine irrgin(tomatch,curirri)

      character*5 tomatch
      character*5 curirri

c...Read in the new irrigation type

      include 'parcp.inc'
      include 'chrvar.inc'

c...Local variables
      integer   i, IRRILNS 
      real      temp
      character fromdat*5, name*20, string*80


c...Number of lines to read for each type
      parameter (IRRILNS =  4)

      character*100 fixfile

      fixfile = ' '

      fixfile(1:pathlen) = libpath
      fixfile(pathlen+1:pathlen+9) = '\irri.100'

c     write(*,*) fixfile

      open(unit=11,file=fixfile,status='OLD')

c      open(unit=11, file='irri.100',status='OLD')
      rewind(11)
20    continue
      read(11, 100, end=200) fromdat
      if (tomatch .ne. fromdat) then 
        do 25 i = 1, IRRILNS
          read(11, *) temp, name
25      continue
        goto 20
      else
        read(11, *) temp, name
        auirri = int(temp)
        call ckdata('schedl','auirri',name)
        read(11, *) fawhc, name 
        call ckdata('schedl','fawhc',name)
        read(11, *) irraut, name 
        call ckdata('schedl','irraut',name)
        read(11, *) irramt, name 
        call ckdata('schedl','irramt',name)
        close(11)
        curirri = tomatch 
      endif


      return

100   format(a5)

200   continue
      call message('   Error reading in values from the irri.100 file.')
      string = '   Looking for type: ' // tomatch
      call message(string)
      STOP

      end
