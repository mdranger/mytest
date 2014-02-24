c               created by zhengpeng li
c Calcualte the GPP and return the monthly sum of GPP
c... From Wenping Yuan's documents
c... The formula of GPP_C:
c... if ta > t_max then ta=t_max;
c... index_tmin =w_tmin + (1-w_tmin)*(ta-t_min)*(ta-t_max)/((ta-t_min)*(ta-t_max)-(ta-topt)**2);
c... if ta < t_min then index_tmin = 0;
c... index_vpd =w_vpd + (1-w_vpd)/(1+a_vpd*exp(b_vpd*vpd));
c... index_par = par;
c... index_phe =min(index_tmin,index_vpd);
c... gpp_c=(ndvi - 0.2)* index_par * max_lue* index_phe;

c... Approx       Approximate 95%
c... Parameter      Estimate    Std Error      Confidence Limits     Label
c...      t_min           -2.9205       0.6108     -4.1178     -1.7232
c...      w_tmin           0.5000            0      0.5000      0.5000
c...      t_max           50.0000            0     50.0000     50.0000
c...      topt            22.5295       0.6292     21.2961     23.7628
c...      w_vpd            0.5000            0      0.5000      0.5000
c...      a_vpd           0.00100            0     0.00100     0.00100
c...      b_vpd           0.00200            0     0.00200     0.00200
c...      max_lue          0.5456      0.00299      0.5397      0.5514
c...      Bound0          75.4924      22.0172     32.3406       118.6    w_vpd < 0.5
c...      Bound8           3.1780       0.2012      2.7837      3.5722    t_max < 50
c...      Bound12         93.6352       5.7367     82.3917       104.9    w_tmin < 0.5
c...      Bound4          96657.8      23634.2     50337.0      142979    0.002 < b_vpd
c...      Bound5          22998.4       8662.3      6021.1     39975.6    0.001 < a_vpd


      real function gpp_cal(gtime)
c      real time

      include 'comput.inc'
      include 'const.inc'
      include 'param.inc'
      include 'parfs.inc'
      include 'parfx.inc'
      include 'plot1.inc'
      include 'plot3.inc'
      include 'potent.inc'
      include 'wth.inc'
      include 'zztim.inc'
      
      real ta, index_tmin, index_vpd, index_par, index_phe
      real t_min, w_tmin, t_max, topt, w_vpd, a_vpd, b_vpd, max_lue
c.... Set up the parameter values      
      t_min = -2.9205
      w_tmin = 0.5
      t_max = 50.0
      topt = 22.5295
      w_vpd = 0.50
      a_vpd = 0.001
      b_vpd = 0.002
      max_lue = 0.5456
c...Compute the Gross Primary production using input files
c...Read in the data file for a location 
c     ta, vpd can be found in flux net data
c     NDVI is seperate

c.....read in temperature and 
      open(unit=11,file='gpp_input.txt',status='OLD')
        read(11, *) year, month, jday, ta, vpd
        call ckdata('cropin','ppdf',name)
30    continue      
      gpp_cal = 0
      
      if (ta > t_max) then
          ta=t_max      
      endif
      index_tmin =w_tmin + (1.0-w_tmin)*(ta-t_min)*(ta-t_max)/
     $     ((ta-t_min)*(ta-t_max)-(ta-topt)**2)

      if (ta < t_min) then
          index_tmin = 0     
      endif
      index_vpd =w_vpd + (1-w_vpd)/(1+a_vpd*exp(b_vpd*vpd))
      index_par = par
      index_phe = min(index_tmin,index_vpd)
      gpp_c= 2.14 *(ndvi - 0.2)* index_par * max_lue* index_phe
      
      close(11)
      return 
      end