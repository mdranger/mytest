
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


c**** Variables used internally which must be shared among routines.
c
c---- Change p1co2 to p1co2(2) to represent surface and soil layers.
c---- Omit ps1s2 -- no longer used.
c---- 				vek  08/91
c
c---- cemicb is the slope of the regression line for C/E of som1
c----        (computed in setrce, called from prelim)
c---- cercrp(mx/mn, nparts, nelem)
c---- fps1s3 is the fraction of som1 decomposing to som3
c---- fps2s3 is the fraction of som2 decomposing to som3
c---- lhzci(pool,iso) is the lower horizon carbon used as an input 
c                     to the surface soil in an erosion event
c---- lhze(pool,iel) is the lower horizon N,P,S used as an input 
c                     to the surface soil in an erosion event
c---- orglch is the fraction of organics that leach from soil som1 when
c----        there is sufficient water flow.  Computed in predec.
c---- rnew??(iel,1) is the C/E ratio for new material created when a
c----		    lignin component decomposes to som1.
c---- rnew??(iel,2) is the C/E ratio for new material created when a
c----		    lignin component decomposes to som2.
c----		The last 2 letters of the name specifies the component:
c----			rnewas has values for aboveground structural
c----			rnewbs has values for belowground structural
c----			rneww1 has values for wood1
c----			rneww2 has values for wood2
c----			rneww3 has values for wood3
c---- wc is the water capacity for the top layer afiel(1)-awilt(1)
c-----livroot(layer): C in live crop roots in each layer (crop + pasture)
c-----livcroot(layer): C in live coarse roots in each layer (tree)
c-----livfroot(layer): C in live fine roots in each layer (tree)
c-----rtfr(10): root fraction of each layer (relevant to the top layer?)
c     according to Jobbagy and Jackson, 1998
c....by zp, add 4 variables are added to estimate the wood decomposition
c....rate
c....decp_times record the time of woodec function call
c....decp_wood1c (2c, 3c) will accumulate for each time and divided by
c....decp_times at last to output
c....remove the variable "run_num"
      common/comput/cemicb(3),cercrp(2,2,3),defacm(12),eftext,
     -	fps1s3,fps2s3,lhzci(3,2),lhze(3,3),orglch,p1co2(2),
     -  s1co2(10), s1s3(10), s2s3(10),
     -  rnewas(3,2),rnewbs(3,2),rneww1(3,2),rneww2(3,2),rneww3(3,2),
     -  wc, livroot(10), livcroot(10),livfroot(10),rtfr(10),deadrt(10),
     -  deadcrt(10),cum_temp(12), decp_wood1c, decp_wood2c, decp_wood3c
     -   ,decp_times

      real cemicb,cercrp,defacm,eftext
     -	,fps1s3,fps2s3,lhzci,lhze,orglch,p1co2
     -  ,s1co2, s1s3,s2s3
     -  ,rnewas,rnewbs,rneww1,rneww2,rneww3,wc
     -  ,livroot,livcroot,livfroot,rtfr,deadrt
     -  ,deadcrt, cum_temp, decp_wood1c, decp_wood2c, decp_wood3c
     
      integer  decp_times
      save /comput/
