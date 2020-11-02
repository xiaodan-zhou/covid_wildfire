C############# Copyright 2000, Phil Everson, Swarthmore College. ##############
C#####################################################################
C# Minor changes for R port Copyright (C) 2004-2005, Roger D. Peng <rpeng@jhsph.edu>
C#####################################################################
C# This program is free software; you can redistribute it and/or modify
C# it under the terms of the GNU General Public License as published by
C# the Free Software Foundation; either version 2 of the License, or
C# (at your option) any later version.
C#
C# This program is distributed in the hope that it will be useful,
C# but WITHOUT ANY WARRANTY; without even the implied warranty of
C# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C# GNU General Public License for more details.
C#
C# You should have received a copy of the GNU General Public License
C# along with this program; if not, write to the Free Software
C# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

C This file contains subroutines called by the Splus function "tlnise"
C (version mv1). (See Everson & Morris (2000) JRSS-B for details).
C
C standardize:  Rotates Y and V so that mean(V*)=I.
C ^^^^^^^^^^^
C
C MODE FINDER:
C ^^^^^^^^^^^
C postmode:     Returns the mode of f(B0|Y).
C ^^^^^^^^
C
C calclik:      Returns the log-likelihood and related quantities.
C ^^^^^^^
C
C estep/mstep:  Perform E and M steps for the EM algorithm used by
C ^^^^^^^^^^^    postmode.
C
C CONSTRAINED WISHARTS:
C ^^^^^^^^^^^^^^^^^^^^
C rscwish:      Returns N Standard Constrained Wishart matrices with
C ^^^^^^^       df degrees of freedom, and constrained by the vector d.
C               (see Everson & Morris (2000) JCGS for details).
C
C augment:      Augments a (t-1)x(t-1) acceptable candidate matrix (Tmat)
C ^^^^^^^       into a txt candidate matrix (possibly acceptable).
C
C checkcon:     Checks a condition on a txt candidate matrix to see if
C ^^^^^^^^      it is acceptable. If it is, an updated inverse is computed.
C
C load:		Loads an acceptable CWish matrix into the output array Ua.
C ^^^^
C
C POSTERIOR CALCULATIONS:
C ^^^^^^^^^^^^^^^^^^^^^^
C
C lfB0:         Computes conditional posterior moments and weights
C ^^^^           each proportional to a density ratio: f(B0)/f0(B0).
C                (See Everson & Morris (2000) JRSS-B for details).
C
C
C RANDOM NUMBER GENERATORS:
C ^^^^^^^^^^^^^^^^^^^^^^^^
C
C ran2:         (function) Returns a random U(0,1) variate (long-period).
C ^^^^   Taken from Press et. al. (1992) Numerical Recipes in Fortran, 2nd ed..
C
C gasdev:       (function) Returns a Normal(0,1) deviate.
C ^^^^^^ Taken from Press et. al. (1992) Numerical Recipes in Fortran, 2nd ed..
C
C gamdev:       (function) Returns a Gamma(alpha,1) deviate.
C ^^^^^^ Taken from Press et. al. (1992) Numerical Recipes in Fortran, 2nd ed..
C
C rnormal:      Returns n random N(mu, sigma) variates.
C ^^^^^^^
C
C rchisq:       Returns a Chi-square(df) variate.
C ^^^^^^
C
C rcchisq:      Generates a random Constrained Chi-square(df;d) variate.
C ^^^^^^^
C
C QUANTILES:
C ^^^^^^^^^^
C
C pchisq:       Evaluates the Chi-square(df) CDF.
C ^^^^^^
C
C qchisq:       Inverts the Chi-square(df) CDF to return quantiles.
C ^^^^^^
C
C bisect:       Bisection routine for inverting the Chi-square(df) CDF.
C ^^^^^^
C
C gammln:	Evaluates the log-gamma function.
C ^^^^^^ Taken from Press et. al. (1992) Numerical Recipes in Fortran, 2nd ed..
C
C gammp:        Returns the incomplete gamma function P(a,x).
C ^^^^^  Taken from Press et. al. (1992) Numerical Recipes in Fortran, 2nd ed..
C
C gser:         Series representation of  P(a,x).
C ^^^^   Taken from Press et. al. (1992) Numerical Recipes in Fortran, 2nd ed..
C
C gcf:          Continued fraction representation of  P(a,x).
C ^^^    Taken from Press et. al. (1992) Numerical Recipes in Fortran, 2nd ed..
C
C MATRIX ALGEBRA:
C ^^^^^^^^^^^^^^^
C
C ludcmp:       Returns the LU decomposition of a matrix.
C ^^^^^^  Taken from Press et. al. (1992) Numerical Recipes in Fortran, 2nd ed..
C
C lubksb:       Performs backsubstitution with a LU decomposition.
C ^^^^^^  Taken from Press et. al. (1992) Numerical Recipes in Fortran, 2nd ed..
C
C solve:        Returns the inverse of a matrix and its log-determinant.
C ^^^^^
C
C mmult:        Matrix multiplication.
C ^^^^^
C
C mammult:      Pre- and post-multiplies each element of an array by
C ^^^^^^^        a matrix.
C
C rtmat:        Returns the symmetric matrix square root.
C ^^^^^
C
C jacobid:      Returns eigenvalues and eigenvectors of a matrix.
C ^^^^^^^
C
C
      SUBROUTINE standardize(Y,V,Vo,p,J,Yj,Vj,rtVo,indxp)
      INTEGER p,J,indxp(p)
      DOUBLE PRECISION Y(p,J), V(p,p,J), Vo(p,p), Yj(p),
     $   Vj(p,p), rtVo(p,p)
C Returns Y and V as rtVo^-1%*%Y and rtVo^-1%*%V%*%rtVo^-1, where rtVo
C is the symmetric pxp matrix square root of V (^-1 indicates inverse).
      INTEGER jay,i,k
      DOUBLE PRECISION d,dum
C reset Vo as its matrix square root:
      call rtmat(Vo, p, Yj, rtVo, Vj)
C copy Vo to rtVo:
      do 200 i=1,p
       do 100 k=1,p
        rtVo(i,k)=Vo(i,k)
 100    continue
 200   continue
       call ludcmp(Vo, p,p,indxp, d)
C Vo is now replaced by the LU decomposition of rtVo.
      do 1200 jay=1,J
C load Yj as Y(,jay) and Vj as V(,,jay):
       do 400 i=1,p
        Yj(i)=Y(i,jay)
        do 300 k=1,p
         Vj(i,k)=V(i,k,jay)
 300    continue
 400   continue
C Backsubstitute to set Yj = rtVo^-1%*%Y(,j):
       call lubksb(Vo,p,p,indxp,Yj)
C Backsubstitute to set Vj = rtVo^-1%*%V(,,j):
       do 500 i=1,p
        call lubksb(Vo,p,p,indxp,Vj(1,i))
 500   continue
C replace Vj (now = rtVo^-1%*%V(,j)) by its transpose:
       do 700 i=1,p
        do 600 k=1,i-1
         dum=Vj(i,k)
         Vj(i,k)=Vj(k,i)
         Vj(k,i)=dum
 600    continue
 700   continue
C Backsubstitute to set Vj = rtVo^-1%*%V(,,j)%*%rtVo^-1:
       do 800 i=1,p
        call lubksb(Vo,p,p,indxp,Vj(1,i))
 800   continue
C Vj now contains rtVo^-1%*%Vj%*%rtVo^-1.
C Replace Y(,j) and V(,,j) by Yj and Vj:
       do 1100 i=1,p
        Y(i,jay)=Yj(i)
        do 1000 k=1,p
         V(i,k,jay)=Vj(i,k)
 1000    continue
 1100   continue
 1200  continue
      RETURN
      END


C
C %%%%%%%%%%%%%%%%%%%%%%%%%% MODE FINDER %%%%%%%%%%%%%%%%%%%%%%%%%
C

      SUBROUTINE postmode(A, Y, W, V, rtV0,alpha, p,q,r, k, EPS, MAXIT,
C outputs:
     $        newA,lf,llik,iter,
C workspace:
     $        gamma, resid, Dgam, WDW, luD,
     $        Yi, Wi, tWi, Di, Si, H, Hi,indxp,indxp2,indxpk, indxr)
C
      INTEGER p,q,r,k,MAXIT,iter,indxp(p),indxp2(p),indxpk(p,k),indxr(r)
      DOUBLE PRECISION A(p,p), Y(p,k), W(q,k),V(p,p,k), rtV0(p,p),
     $    alpha, EPS, newA(p,p), lf, gamma(r), resid(p,k), Dgam(r,r),
     $    WDW(r,r), luD(p,p,k), Yi(p), Wi(p,r), tWi(r,p),Di(p,p),
     $    Si(p,p),H(p,p), Hi(p,p)
C
C Returns newA = (Bhat^{-1} - I), where Bhat is the posterior mode
C of B0 = (I+A)^{-1} in the following p-dimensional 2-level Normal
C hierarchical model:
C
C   Level-1:   Yi | thetai  ~  Np(thetai, Vi),  i=1,...,k
C   Level-2:   thetai|mui,A ~ Np(mui, A);  mui = Wi%*%gamma
C
C The Vi's are assumed to have been re-scaled by rtV0^{-1}. The Yi's
C are p-dimensional responses from k groups, and a q-dimensional covariate
C wi is available from each group. The level-2 means mui are computed by
C multiplying a pxr covariate matrix Wi by an unknown r-dimensional
C regression coefficient gamma. Wi is the Kronecker product of the pxp
C identity matrix I with the qx1 covariate vector wi (r = qp).
C
C The prior distribution assumed for B0 is
C
C    P(B0) \proto |B0|^{alpha-p-1}, 0 < B0 < I.
C
C The program also returns lf, the posterior density of B0 at the mode.
C oldA is a starting value for A. TOL and MAXIT govern convergence.
      INTEGER i,j
      DOUBLE PRECISION dev,maxdev,llik,ldwdw,sldd,ssres,ldd0
C
      maxdev=1.
      iter=0
C replace rtV0 by its LU decomposition; set indxp (leave it!).
      call ludcmp(rtV0,p,p,indxp,dev)
      do while(maxdev.gt.EPS.and.iter.lt.MAXIT)
       iter=iter+1
C Set Di = I + A:
       do 200 i=1,p
        do 100 j=1,p
         Di(i,j)=A(i,j)
 100    continue
        Di(i,i)=Di(i,i)+1.
 200   continue
C Compute log(det(I+A)):
       call ludcmp(Di,p,p,indxp2,ldd0)
       ldd0=0.
       do 300 i=1,p
        ldd0=ldd0+dlog(abs(Di(i,i)))
 300   continue
C Compute the log-likelihood of A, and related quantities:
       call calclik(A,Y,W,V,rtV0,p,q,r,k,llik,gamma,resid,Dgam,WDW,
     $  luD,ldwdw,sldd,ssres,Yi,Wi,tWi,Di,indxp,indxp2,indxpk,indxr)
C
C Adjust the log-likelihood to account for the prior parameter alpha,
C  and obtain lf, the log-posterior density evaluated at B0=(I+A)^{-1}|Y:
       lf = llik - (alpha-1.*p-1.)*ldd0/2.
C
C Compute expected sufficient statistic H, given A:
       call estep(A,resid,WDW,luD,W,V,rtV0,p,q,r,k,
     $  H, Hi, Di, Si, Wi, tWi,indxp,indxp2,indxpk,indxr)
C
C Maximize the complete data posterior density given H:
       call mstep(H, alpha, p, k, newA, Yi, Hi, Di, Si)
C
C Check for convergence:
       maxdev=0.
       do 500 i=1,p
        do 400 j=1,p
         dev=abs(newA(i,j)-A(i,j))
         if(dev.gt.maxdev) then
          maxdev=dev
         endif
         A(i,j)=newA(i,j)
 400    continue
 500   continue
      end do
      RETURN
      END

      SUBROUTINE calclik(A, Y, W, V, rtV0, p, q, r, k,
C outputs:
     $  llik, gamma, resid, Dgam, WDW, luD, ldwdw, sldd, ssres,
C work space:
     $  Yi, Wi, tWi, Di, indxp,indxp2,indxpk,indxr)
C
      INTEGER p,q,r,k,indxp(p), indxp2(p),indxpk(p,k),indxr(r)
      DOUBLE PRECISION A(p,p),Y(p,k),W(q,k),V(p,p,k),rtV0(p,p),
     $  llik,gamma(r), resid(p,k), Dgam(r,r), WDW(r,r), luD(p,p,k),
     $  ldwdw,sldd, ssres, Yi(p),  Wi(p,r), tWi(r,p), Di(p,p)
C Computes llik: the log-likelihood of A (or of B0 = (I+A)^{-1}).
C
C Sets gamma and Dgam as the posterior mean and covariance matrix of
C  the level-2 regression coefficient gamma, assuming A is the level-2
C  covariance matrix.
C Sets resid as the raw residuals Yi - Wi%*%gamma.
C Sets WDW as the LU decomposition of Dgam^{-1}.
C Sets luD as the LU decompositions of (Vi+A), i=1,...,k.
C
C Sets ldwdw = log(det(sum(Wi'(Vi+A)^{-1}Wi))).
C Sets sldd = sum(log(det(Vi+A))).
C Sets ssresid = sum(Yi'%*%(Vi+A)^{-1}%*%Yi).
C
C rtV0 is input as its LU decomposition; indxp goes with it.
      INTEGER i,j,l,t
C initialize sldd, WDW and gamma to 0's. Set Dgam as identity:
      sldd=0.
      do 200 j=1,r
       gamma(j)=0.
       do 100 l=1,r
        WDW(j,l)=0.
        Dgam(j,l)=0.
 100   continue
       Dgam(j,j)=1.
 200  continue
C First i=1,k loop:
      do 1300 i=1,k
C
C Set Yi, Wi=tWi=0, and Di = Vi + A:
       do 400 j=1,p
        Yi(j)=Y(j,i)
        do 300 l=1,p
         Di(j,l)=V(j,l,i)+A(j,l)
 300    continue
        do 350 l=1,r
         Wi(j,l)=0.
         tWi(l,j)=0.
 350    continue
 400   continue
C Set Wi = I kronecker wi  (p*1 x p*q)
       do 500 j=1,p
        do 450 l=1,q
         Wi(j,q*(j-1)+l)=W(l,i)
 450    continue
 500   continue
C replace Wi by rtV0^{-1}%*%Wi:
       do 550 j=1,r
        call lubksb(rtV0,p,p,indxp,Wi(1,j))
 550   continue
C Set tWi as the transpose of wi ( = rtV0^{-1} kronecker wi):
       do 575 j=1,p
        do 560 l=1,r
         tWi(l,j)=Wi(j,l)
 560    continue
 575   continue
C solve (Vi+A)%*%X = Wi for X (returned as Wi):
       call solve(Di, Wi, p, p, r, r, indxp2)
C Di is now the LU decomposition of (Vi+A); Wi is now (Vi+A)^{-1}%*%Wi.
C
C Add logs of diagonal elements of Di to update sum(log(det(Vi+A))):
       do 600 j=1,p
        sldd=sldd+dlog(abs(Di(j,j)))
C store indxp2 in indxpk(1,i):
        indxpk(j,i)=indxp2(j)
 600   continue
C Multiply elements to update WDW = sum(Wi'(Vi+A)^{-1}Wi)
C  and WDY = sum(Wi'(Vi+A)^{-1}Yi) (loaded in gamma):
       do 1000 j=1,r
        do 700 t=1,p
         gamma(j)=gamma(j)+Yi(t)*Wi(t,j)
 700    continue
        do 900 l=1,r
         do 800 t=1,p
          WDW(j,l)=WDW(j,l)+tWi(j,t)*Wi(t,l)
 800     continue
 900    continue
 1000  continue
C Load Di into luD:
       do 1200 j=1,p
        do 1100 l=1,p
         luD(j,l,i)=Di(j,l)
 1100   continue
 1200  continue
 1300 continue
C
C End of first i=1,k loop.  WDW is now sum(Wi'(Vi+A)^{-1}Wi).
C luD contains the LU decompositions of (Vi+A), i=1,..,k.
C
      call solve(WDW,gamma,r,r,1,1,indxr)
C Now gamma is finished; WDW is replaced by its LU decomposition.
C
C Compute ldwdw as the log-determinant of WDW:
      ldwdw=0.
      do 1400 i=1,r
       ldwdw=ldwdw+dlog(abs(WDW(i,i)))
 1400 continue
C Finish inverting WDW into Dgam:
      do 1500 i=1,r
       call lubksb(WDW,r,r,indxr,Dgam(1,i))
 1500 continue
C
C
C Compute ssresid = sum(Yi'%*%(Vi+A)^{-1}%*%(Yi-Wi%*%gamma).
      ssres=0.
C Second i=1,k loop:
      do 2500 i=1,k
C Load Y(,,i) into Yi and the LU decomposition of (Vi+A) into Di:
       do 1700 j=1,p
        Yi(j)=Y(j,i)
        do 1600 l=1,p
         Di(j,l)=luD(j,l,i)
 1600   continue
        do 1650 l=1,r
         Wi(j,l)=0.
 1650   continue
 1700  continue
C Set Wi = I kronecker wi  (p*1 x p*q)
       do 1750 j=1,p
        do 1725 l=1,q
         Wi(j,q*(j-1)+l)=W(l,i)
 1725   continue
 1750  continue
C replace Wi by rtV0^{-1}%*%Wi:
       do 1775 j=1,r
        call lubksb(rtV0,p,p,indxp,Wi(1,j))
 1775  continue
C Set residi = Yi-Wi%*%gamma:
       do 1900 j=1,p
        resid(j,i)=0.
C First set residij = (Wi%*%gamma)j;
         do 1800 l=1,r
          resid(j,i)=resid(j,i)+Wi(j,l)*gamma(l)
 1800   continue
C Now subtract (Wi%*%gamma)j from Yij
        resid(j,i)=Yi(j)-resid(j,i)
C set indxp2 = indxpk(1,i):
       indxp2(j)=indxpk(j,i)
 1900  continue
C Solve (Vi+A)%*%X = Yi to get X = (Vi+A)^{-1}Yi (returned as Yi):
       call lubksb(Di, p,p,indxp2, Yi)
C Now multiply by resid_i to get sum of squares contribution:
       do 2000 j=1,p
        ssres=ssres+Yi(j)*resid(j,i)
 2000  continue
 2500 continue
C End of second i=1,k loop.
      llik = -0.5*(sldd + ldwdw + ssres)
      RETURN
      END




      SUBROUTINE estep(A, resid,WDW,luD,W,V,rtV0,p,q,r,k,
     $  H, Hi, Di, Si, Wi,tWi,indxp,indxp2,indxpk,indxr)
C
      INTEGER p,q,r,k,indxp(p),indxp2(p),indxpk(p,k),indxr(r)
      DOUBLE PRECISION A(p,p),resid(p,k),WDW(r,r),luD(p,p,k),
     $ V(p,p,k),rtV0(p,p),W(q,k),H(p,p),Hi(p,p),Di(p,p),Si(p,p),
     $ Wi(p,r), tWi(r,p)
C
C Performs the E-step in an EM algorithm to locate the MLE or
C posterior mode of B0 = (I+A)^{-1} in a 2-level Normal HM.
C
C resid is the matrix of residuals and WDW is the LU decomposition
C  of the matrix sum(W[,,i]%*%(Vi+A)^{-1}t(W[,,i])).
C luD is the array of LU decompositions for the Vi+A, i=1,..,k.
C
C Returns H as the pxp expected level-2 sum of squared deviations,
C given that A is the correct level-2 covariance matrix:
C  H = E(sum((Yi-Wi%*%gamma)%*%(Yi-Wi%*%gamma)').
C
      INTEGER i,j,l,t
      DOUBLE PRECISION dum
C Initialize H:
      do 70 j=1,p
       do 50 l=1,p
        H(j,l)=0.
 50    continue
 70   continue
C
C  BIG i=1,...,k loop:
      do 1000 i=1,k
C Load W(,i) into Wi,tWi, luD(,,i) into Di, Hi=A, and Si=0:
       do 200 j=1,p
        do 100 l=1,r
         tWi(l,j)=0.
         Wi(j,l)=0.
 100    continue
C Set indxp2 = indxpk(1,i)
        indxp2(j)=indxpk(j,i)
        do 150 l=1,p
         Si(j,l)=0.
         Di(j,l) = luD(j,l,i)
         Hi(j,l)=A(j,l)
 150    continue
 200   continue
       do 275 j=1,p
        do 250 l=1,q
         Wi(j,q*(j-1)+l) =W(l,i)
 250    continue
C tWi (pq x p) = Kronecker product of pxp identity with W(,i) (qx1)
 275   continue
C replace Wi by rtV0^{-1}%*%Wi:
       do 280 j=1,r
        call lubksb(rtV0,p,p,indxp,Wi(1,j))
 280   continue
C Set tWi as the transpose of wi ( = rtV0^{-1} kronecker wi):
       do 290 j=1,p
        do 285 l=1,r
         tWi(l,j)=Wi(j,l)
 285    continue
 290   continue
C Solve WDW%*%X = tWi; X = Dgam%*%tWi (stored in tWi)
C  WDW is input as the LU decomposition for WDW (with indxr).
       do 300 j=1,p
        call lubksb(WDW, r,r,indxr,tWi(1,j))
 300   continue
C Add to Si the outer product of resid with itself and
C  the product of Wi with Dgam%*%tWi (stored in tWi):
       do 600 j=1,p
        do 500 l=1,p
         Si(j,l) = Si(j,l)+resid(j,i)*resid(l,i)
         do 400 t=1,r
          Si(j,l) = Si(j,l) + Wi(j,t)*tWi(t,l)
 400     continue
 500    continue
 600   continue
C Now Si = ( Y(,i)-W(,,i)%*%gamma*)^2 + W(,,i)%*%Dgam%*%t(W(,,i)) ).
C Pre- multiply Si by (I-Bi) = A(Vi+A)^{-1}; store as Hi.
C
C First solve (I+A)X = A for X = (I+A)^{-1}A = A(I+A)^{-1}
C (store as Hi). Recall that Di contains the LU decomposition
C of (Vi+A) and Hi = A.
       do 700 j=1,p
        call lubksb(Di, p,p,indxp2,Hi(1,j))
C corrected error (indxp, not indxp2) 7/29/2000.
 700   continue
C Now post-multiply Si by (I+A)^{-1}A (stored in Hi); store in Di:
       call mmult(Si,Hi ,p,p,p, Di,p,p,p)
C Replace Di by its transpose, A%*%(Vi+A)^{-1}Si;
       do 770 j=1,p
        do 750 l=1,j
         dum=Di(j,l)
         Di(j,l)=Di(l,j)
         Di(l,j)=dum
 750    continue
 770   continue
       call mmult(Di,Hi, p,p,p, Si,p,p,p)
C Si is now A(Vi+A)^{-1}Si(Vi+A)^{-1}A (see comment after 600)
C
C Multiply Vi by (I+A)^{-1}A (stored as Hi); store in Di:
       do 830 j=1,p
        do 800 l=1,p
         Di(j,l)=0.
         do 790 t=1,p
          Di(j,l)=Di(j,l)+V(j,t,i)*Hi(t,l)
 790     continue
 800    continue
 830   continue
C Finally, add the results to H:
       do 900 j=1,p
        do 850 l=1,p
         H(j,l) = H(j,l) + (Si(j,l) +Di(j,l)+Si(l,j)+Di(l,j))/2.
C modified to ensure symmetry of H 7/31/2000.
 850    continue
 900   continue
 1000 continue
      RETURN
      END


      SUBROUTINE mstep(H, alpha, p, k, newA, Yi, Hi, Di, Si)
C
      INTEGER p,k
      DOUBLE PRECISION H(p,p), alpha, newA(p,p),
     $ Yi(p), Hi(p,p), Di(p,p), Si(p,p)
C Performs the M-step in an EM algorithm to locate the MLE or
C posterior mode of B0 = (I+A)^{-1} in a 2-level Normal HM.
C
C I'm using storage variable names Yi, Hi, Di and Si from the postmode
C and estep routines - the names are not descriptive.
C
      INTEGER i,j,l
      DOUBLE PRECISION x
      if(int(alpha).eq.p+1) then
C Find the conditional MLE of A given H:
       do 200 i=1,p
        do 100 j=1,p
         newA(i,j)=H(i,j)/k
 100    continue
 200   continue
      else
C Find newA = modeB0^{-1}-I where modeB0 is the posterior
C  mode for B0, assuming p(B0) = |B0|^{alpha-p-1} (a priori).
       x=alpha-1.*p-1.
       do 400 i=1,p
        do 300 j=1,p
C Set Di as the matrix to be squared: H - kI (used later, too).
         Di(i,j)=H(i,j)
 300    continue
        Di(i,i)=H(i,i)-1.*k
 400   continue
C Set Si as the square of Di:
       do 700 i=1,p
        do 600 j=1,p
         Si(i,j)=0.
         do 500 l=1,p
          Si(i,j)=Si(i,j)+Di(i,l)*Di(l,j)
 500     continue
 600    continue
 700   continue
C Set newA as the matrix to be square-rooted.
       do 900 i=1,p
        do 800 j=1,p
         newA(i,j) = 4.*(1.*k+x)*H(i,j) + Si(i,j)
 800    continue
 900   continue
C Replace newA by its symmetric matrix square root:
       call rtmat(newA, p, Yi, Hi, Si)
       do 1500 i=1,p
        do 1400 j=1,p
         newA(i,j) = (newA(i,j) + Di(i,j))/(2.*(1.*k+x))
 1400   continue
 1500  continue
       endif
      RETURN
      END

C
C
C
C %%%%%%%%%%%%%%%%%%%% CONSTRAINED WISHART SIMULATION %%%%%%%%%%%%%%%%%%%%
C
C
      SUBROUTINE rscwish(Ua, N, p, df, d, pd, chitab, seed, Tmat,
     $		Deltinv, H11, H12, Zt, Zts, tempp, mcnt, drvcnt,
     $		orvcnt, rejcnt)
C Returns N pxp Constrained Wishart(df, I; diag(d)) matrices in Ua.
C		(see Everson & Morris (2000) JCGS for details).
      INTEGER N, p, seed, step, cnt, mcnt, drvcnt, orvcnt, accept
      INTEGER rejcnt(p)
      DOUBLE PRECISION Ua(p,p,N), df, d(p), pd(p), chitab(999)
      DOUBLE PRECISION Tmat(p,p), Ut, Zt(p), Zts(p), dt, pdt, Xts
      DOUBLE PRECISION Uts, Deltinv(p,p), H11(p,p), H12(p), tempp(p)
      DOUBLE PRECISION lambda
      lambda=0.
      mcnt=0
      drvcnt=0
      orvcnt=0
C Keep track of how many matrices are begun (=mcnt) and how many diagonal
C random variables (=drvcnt) and off-diagonal random variables (=orvcnt)
C are generated.
      cnt=0
C Keep track of how many acceptable matrices have been generated (=cnt).
      do while(cnt.lt.N)
       mcnt = mcnt + 1
       step=1
       pdt=pd(step)
       drvcnt = drvcnt + 1
       call rcchisq(Ut, df, pdt, chitab, seed)
       Tmat(1,1)=dsqrt(Ut)
       accept=1
       Deltinv(1,1) = 1./(d(1)-Ut)
       do while(step.lt.p.and.accept.eq.1)
        drvcnt = drvcnt + 1
        orvcnt = orvcnt + step
        step=step+1
        pdt=pd(step)
        call augment(Tmat, step, p, df, pdt, chitab, Zt, Zts, Xts,
     $		Ut, Uts, seed)
        dt=d(step)
        if(dt.gt.0) then
         call checkcon(Tmat, step, p, dt, Zt, Ut, accept, Deltinv,
     $		H11, H12, lambda, tempp)
        endif
       end do
       if(accept.eq.1) then
C An acceptable CWish matrix has been generated. Update cnt and load
C U = Tmat%*%t(Tmat) into the next open position in Ua (pxpxN).
        cnt = cnt + 1
        call load(Ua, Tmat, p, N, cnt)
       else
	rejcnt(step)=rejcnt(step)+1
       endif
      end do
      return
      end




      SUBROUTINE augment(Tmat, step, p, df, pdt, chitab, Zt, Zts,
     $			Xts, Ut, Uts, seed)
C Augments a (t-1)x(t-1) Tmat to a txt (step = t), by simulating a
C Constrained Chi-square(df; dt) and partitioning it with Betas to fill
C in the t-th row of Tmat (t=2,..,p).
C
C p:		physical dimension of Tmat
C pd:		P(U < dt), for U ~ Chi-square(df)
C chitab:	table of 999 evenly-spaced Chi-square(df) quantiles
C seed:		seed for random number generator
C
      INTEGER step, seed, p, i, j, k
      DOUBLE PRECISION Tmat(p,p), df, pdt, chitab(999), Zt(p), Zts(p)
      DOUBLE PRECISION Xts, Ut, Uts, z1, z2, ZZ, u1, u2, pi, ran2
      pi=3.14159265
      call rcchisq(Ut, df, pdt, chitab, seed)
      k=int((step-1)/2.)
C k is the largest integer less than or equal to (t-1)/2 (step=t)
      ZZ=0.0
      do 100 i=1,k
       call rnorm(z1, z2, seed)
       Zts(2*i-1)=z1
       Zts(2*i)=z2
       ZZ = ZZ + z1**2 + z2**2
 100  continue
      if(step-1.gt.2*k) then
C if t-1 is odd, get one more N(0,1) to fill out Zts:
       u1=ran2(seed)
       u2=ran2(seed)
       z1 = dcos(2.*pi*u2)*dsqrt((-2.)*dlog(u1))
       Zts(step-1)=z1
       ZZ = ZZ + z1**2
      endif
C Call the bisection routine to generate a Chi-square(df-step+1) rv Xts.
      call rchisq(Xts, df-step+1, seed)
C Zts[1:(t-1)] ~ iid N(0,1); Xts ~ Chi-square(df-t+1)
      Uts = ZZ + Xts
      do 300 j=1,step-1
       Zt(j) = Zts(j)*dsqrt(Ut/Uts)
       Tmat(step,j) = Zt(j)
 300  continue
      Tmat(step,step)=dsqrt(Xts*Ut/Uts)
      return
      end







      SUBROUTINE checkcon(Tmat,step,p,dt,Zt,Ut,accept,Deltinv,
     $ H11,H12,lambda, tempp)
C Checks to see if the t-th step (step=t) of the construction of Tmat
C is acceptable. If not acceptable (i.e., if det(D-TT') is negative
C after step t) then "accept" is returned as 0. Otherwise, "accept"
C is returned as 1, and "Deltinv", which is input as the (t-1)x(t-1)
C inverse of D-TT', is updated to contain the txt inverse of D-TT'.
C
      INTEGER step, p, i, j, accept
      DOUBLE PRECISION Tmat(p,p), dt, Zt(p), Ut, Deltinv(p,p)
      DOUBLE PRECISION H11(p,p), H12(p), H22, lambda
      DOUBLE PRECISION tempp(p),  x(1,1)
C
      call mmult(Tmat, Zt, p, p, 1, tempp, step-1, step-1, 1)
      call mmult(Deltinv, tempp, p,p,1, H12, step-1, step-1, 1)
      call mmult(tempp, H12, 1, p, 1, x, 1, step-1, 1)
      lambda = dt - Ut - x(1,1)
C
      if(lambda.lt.0.) then
       accept = 0
C lambda < 0 at any step t means we have to start over with t=1.
      else
C Update Deltinv from (t-1)x(t-1) to txt. Leave accept=1.
       if(step.lt.p) then
C Does not update Deltinv if t=p (step=t=p). That would indicate
C that the matrix is finished and acceptable.
        H22 = 1./lambda
        do 200 i=1,step-1
         H12(i) = H12(i)/lambda
         do 100 j=1,step-1
          H11(i,j) = Deltinv(i,j) + H12(i)*H12(j)/H22
 100     continue
 200    continue
        do 400 i=1,step-1
         Deltinv(step,i) = H12(i)
         Deltinv(i,step) = H12(i)
         do 300 j=1,step-1
          Deltinv(i,j)=H11(i,j)
	  Deltinv(j,i)=H11(i,j)
 300     continue
 400    continue
        Deltinv(step,step)=H22
       endif
      endif
C
      return
      end

      SUBROUTINE load(Ua, Tmat, p, N, cnt)
C Computes Tmat%*%t(Tmat) and loads into Ua(,,cnt)
      INTEGER i, j, k, cnt, p, N
      DOUBLE PRECISION Ua(p,p,N), Tmat(p,p)
C
      Ua(1,1,cnt)=Tmat(1,1)**2
      do 300 i=2,p
       Ua(i,i,cnt)=Tmat(i,i)**2
       do 200 j = 1,i-1
        Ua(i,i,cnt) = Ua(i,i,cnt) + Tmat(i,j)**2
        Ua(i,j,cnt) = 0.0
        do 100 k=1,j
         Ua(i,j,cnt)=Ua(i,j,cnt)+Tmat(i,k)*Tmat(j,k)
 100    continue
        Ua(j,i,cnt)=Ua(i,j,cnt)
 200   continue
 300  continue
      return
      end

C
C %%%%%%%%%%%%%%%%%%% POSTERIOR CALCULATIONS %%%%%%%%%%%%%%%%%%%%%
C


      SUBROUTINE lfB0(B0a,Y,V,w,rtV0,N,p,q,r,J,df,Siginv,prior,adj,
C 14(Uses tlnise.mv1.src variable names)
C outputs:
     $ lfv,lf0v,lrv,gamhat,Dgamhat,thetahat,Vthetahat,meanA,sumwt,
C 23workspace:
     $ gamma, Dgam, GG, theta, Vtheta, resid, B0,Bj, A, WDW, luD,
C 34
     $ Yj, Vj, Wj, tWj, Dj, indxp, indxp2, indxpJ, indxr)
C
      INTEGER p,q,r,J,indxp(p), indxp2(p),indxpJ(p,J),indxr(r)
      DOUBLE PRECISION B0a(p,p,N), Y(p,J), V(p,p,J), w(q,J),
     $  rtV0(p,p), df, Siginv(p,p),prior,adj,
C
     $  lfv(N),lf0v(N),lrv(N),gamhat(r),Dgamhat(r,r),
     $  thetahat(p,J), Vthetahat(p,p,J),meanA(p,p),sumwt,
C
     $  gamma(r),Dgam(r,r),GG(r,r),theta(p),Vtheta(p,p,J),
     $  resid(p,J),B0(p,p),Bj(p,p),A(p,p),WDW(r,r),luD(p,p,J),
     $  Yj(p),Vj(p,p),Wj(p,r),tWj(r,p),Dj(p,p)
C Returns the log posterior density of B0, assuming a prior
C distribution p(B0)dB0 propto |B0|^{(prior-p-1)/2}dB0, 0 < B0 < I.
C Y and V are assumed to have been rotated by rtV0^-1.
C
      INTEGER iter,i,jay,l
      DOUBLE PRECISION ldet, llik, ssres, sldd, ldwdw, trBS, wt
      do 60 i=1,r
       gamhat(i)=0.
       do 50 k=1,r
        GG(i,k)=0.
        Dgamhat(i,k)=0.
 50    continue
 60   continue
      do 90 jay=1,J
       do 80 i=1,p
        thetahat(i,jay)=0.
        do 70 k=1,p
         meanA(i,k)=0.
         Vthetahat(i,k,jay)=0.
         Vtheta(i,k,jay)=0.
 70     continue
 80    continue
 90   continue
C Replace rtV0 by its LU decomposition, with indxp (leave it!):
      call ludcmp(rtV0, p, p, indxp, ldet)
      do 2000 iter=1,N
C set B0 = Di = B0a(,,iter):
       do 200 i=1,p
        do 100 jay=1,p
         B0(i,jay)=B0a(i,jay,iter)
 100    continue
 200   continue
C Set Dj = B0%*%Siginv:
       call mmult(B0, Siginv, p,p,p, Dj,p,p,p)
C set A = B0^{-1}:
       call inverse(B0,p,p,A,indxp2,ldet)
C B0 is replaced by its LU decomposition;
C  A = B0^{-1}; ldet=log(det(B0))
       trBS=0.
C Set A = B0^{-1}-I, trBS = tr(B%*%Siginv):
       do 300 i=1,p
        A(i,i)=A(i,i)-1.
        trBS = trBs + Dj(i,i)
 300   continue
C Compute lf0(B0) and lf(B0):
       lf0v(iter)= ((df-1.*p-1.)*ldet - trBS)/2.
       call calclik(A, Y, w, V,rtV0,p,q,r,J,llik,gamma,resid,
     $  Dgam, WDW, luD, ldwdw, sldd, ssres, Yj, Wj,tWj,Dj,
     $  indxp, indxp2, indxpJ, indxr)
       lfv(iter)=llik + (prior-1.*p -1.)*ldet/2.
C Compute wt = exp(lf-lf0+adj):
       lrv(iter)=lfv(iter)-lf0v(iter)+adj
       wt = exp(lrv(iter))
       sumwt=sumwt+wt
C
C Add weighted contributions to meanA:
C
       do 350 i=1,p
        do 325 k=1,p
         meanA(i,k)=meanA(i,k)+wt*A(i,k)
 325    continue
 350   continue
C
C Add weighted contributions to gamhat, Dgamhat, and GG:
C
       do 500 i=1,r
        gamhat(i)=gamhat(i)+wt*gamma(i)
        do 400 jay=1,r
         Dgamhat(i,jay)=Dgamhat(i,jay)+wt*Dgam(i,jay)
         GG(i,jay)=GG(i,jay)+wt*gamma(i)*gamma(jay)
 400    continue
 500   continue
C
C Add weighted contributions to thetahat, Vthetahat and Vtheta:
C
       do 1800 jay=1,J
C set indxp2 and Dj as the LU decomposition of Vj+A:
        do 700 i=1,p
         theta(i)=0.
         indxp2(i)=indxpJ(i,jay)
         do 600 k=1,p
          Dj(i,k)=luD(i,k,jay)
C set B0 as V(,,j):
          B0(i,k)=V(i,k,jay)
 600     continue
         do 650 k=1,r
C reset Wj, tWj:
          Wj(i,k)=0.
          tWj(k,i)=0.
 650     continue
 700    continue
C compute (Vj+A)^-1 %*%Vj = t(Bj) (store as B0):
        do 800 i=1,p
         call lubksb(Dj, p,p, indxp2, B0(1,i))
 800    continue
C Set Yj = Wjgamma, Bj, Dj = I - Bj:
        do 1000 i=1,p
         Yj(i) = Y(i,jay)-resid(i,jay)
         do 900 k=1,p
          Bj(i,k)=B0(k,i)
          Dj(i,k)=-B0(k,i)
 900     continue
 1000   continue
        do 1005 i=1,p
         Dj(i,i)=Dj(i,i)+1.
 1005   continue
C Set Wj:
        do 1020 i=1,p
         do 1010 k=1,q
          Wj(i,q*(i-1)+k)=W(k,jay)
 1010    continue
 1020   continue
C replace Wj by rtV0^{-1}%*%Wj:
        do 1030 i=1,r
         call lubksb(rtV0,p,p,indxp,Wj(1,i))
 1030   continue
C transpose the product Wj%*%Dgam into tWj:
        do 1060 i=1,p
         do 1050 k=1,r
          do 1040 l=1,r
           tWj(k,i)=tWj(k,i)+Wj(i,l)*Dgam(l,k)
 1040     continue
 1050    continue
 1060   continue
C set A = BjWjDgamWj'Bj' (B0 still holds Bj'):
        call mmult(Wj,tWj,p,r,p,A,p,r,p)
        call mmult(Bj,A,p,p,p,Vj,p,p,p)
        call mmult(Vj,B0,p,p,p,A,p,p,p)
C Set theta = (I-Bj)Yj + BjWjgamma, Vj = (I-Bj)Vj + (A):
        do 1300 i=1,p
         theta(i)=0.
         do 1200 k=1,p
          Vj(i,k)=0.
          theta(i)=theta(i)+Bj(i,k)*Yj(k) + Dj(i,k)*Y(k,jay)
          do 1100 l=1,p
           Vj(i,k)=Vj(i,k)+Dj(i,l)*V(l,k,jay)
 1100     continue
          Vj(i,k)=Vj(i,k)+A(i,k)
 1200    continue
 1300   continue
        do 1500 i=1,p
         thetahat(i,jay)=thetahat(i,jay)+wt*theta(i)
         do 1400 k=1,p
          Vthetahat(i,k,jay)=Vthetahat(i,k,jay)+wt*Vj(i,k)
          Vtheta(i,k,jay) = Vtheta(i,k,jay)+wt*theta(i)*theta(k)
 1400    continue
 1500   continue
 1800  continue
 2000 continue
C
C rescale estimates by sumwt:
C
      do 2100 i=1,r
       gamhat(i)=gamhat(i)/sumwt
 2100 continue
      do 2300 i=1,r
       do 2200 k=1,r
        Dgamhat(i,k)=(Dgamhat(i,k)+GG(i,k))/sumwt-gamhat(i)*gamhat(k)
 2200  continue
 2300 continue
      do 2350 i=1,p
       do 2325 k=1,p
        meanA(i,k)=meanA(i,k)/sumwt
 2325  continue
 2350 continue
      do 3000 jay=1,J
       do 2400 i=1,p
        thetahat(i,jay)=thetahat(i,jay)/sumwt
 2400  continue
       do 2600 i=1,p
        do 2500 k=1,p
         Vthetahat(i,k,jay)=(Vthetahat(i,k,jay)+Vtheta(i,k,jay))/sumwt
         Vthetahat(i,k,jay)=Vthetahat(i,k,jay)-thetahat(i,jay)*
     $    thetahat(k,jay)
 2500   continue
 2600  continue
 3000 continue
      RETURN
      END
C
C
C %%%%%%%%%%%%%%%%%%%% RANDOM NUMBER GENERATION %%%%%%%%%%%%%%%%%%%%
C
C
      DOUBLE PRECISION FUNCTION ran2(idum)
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
C
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      DOUBLE PRECISION AM, EPS, RNMX
C
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1,
     $ IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,
     $ IR2=3791,NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
C `Long period (> 2 x 10^18) random number generator of L'Ecuyer with
C Bays-Durham shuffle and added safeguards. Returns a uniform random
C deviate between 0.0 and 1.0 (exclusive of the endpoint values). Call
C with idum a negative integer to initialize; thereafter, do not alter
C idum between successive deviates in sequence. '
C
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/ , iv/NTAB*0/, iy/0/

      if(idum.le.0) then
C initialize
       idum=-idum
C prevent idum=0
       idum2=idum
       do 100 j=NTAB+8,1,-1
C Load shuffle table (after 8 warm-ups)
        k=idum/IQ1
        idum=IA1*(idum-k*IQ1)-k*IR1
        if(idum.lt.0) then
         idum=idum+IM1
        endif
        if(j.le.NTAB) then
         iv(j)=idum
        endif
 100   continue
       iy=iv(1)
      endif
C Start here when not initializing.
C Compute idum=mod(IA1*idum,IM1) without overflows by
C Schrage's method
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if(idum.lt.0) then
       idum=idum+IM1
      endif
      k=idum2/IQ1
C Compute idum2=mode(IA2*idum2,IM2) likewise.
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if(idum2.lt.0) then
       idum2=idum2+IM2
      endif
      j=1+iy/NDIV
C j will be in the range 1:NTAB
C Here idum is shuffled, idum and idum2 are combined to
C generate output.
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1) then
       iy=iy+IMM1
      endif
      ran2=min(AM*iy,RNMX)
C No endpoint values returned.
      return
      END


      DOUBLE PRECISION FUNCTION gasdev(idum)
C
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
C Modified 1/24/2000 by P. Everson for use with ran2.
      INTEGER idum
C Returns a normally distributed deviate with 0 mean and unit variance,
C Using ran2(idum) as the source of uniform deviates.
C
      INTEGER iset
      DOUBLE PRECISION fac, gset, rsq, v1, v2, ran2
      SAVE iset, gset
      DATA iset/0/
      if(iset.eq.0) then
C We don't have an extra deviate handy, so pick two U(-1,1) numbers:
C
 1     v1=2.*ran2(idum)-1.
       v2=2.*ran2(idum)-1.
C See is (v1,v2) is in the unit circle:
       rsq=v1**2+v2**2
C If not, try again:
       if(rsq.ge.1.or.rsq.eq.0.) goto 1
C If so, make Box-Muller transformation to get two Normal deviates.
       fac=dsqrt(-2.*dlog(rsq)/rsq)
       gset=v1*fac
       gasdev=v2*fac
       iset=1
      else
C We have an extra deviate handy, so return it and unset the flag.
       gasdev=gset
       iset=0
      endif
      return
      END




      DOUBLE PRECISION FUNCTION gamdev(ia, idum)
C
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
C Modified 1/24/2000 by P. Everson for use with ran2.
      INTEGER ia, idum
C Returns a deviate distributed as a Gamma distribution of INTEGER order
C ia=int(da), i.e., a waiting time to the iath event in a Poisson
C process of unit mean. Uses ran2(idum) as the source Uniform deviates.
C
      INTEGER j
      DOUBLE PRECISION am, e, s, v1, v2, x, y, ran2
      if(ia.lt.6) then
C Use direct method, adding Exponentials.
       x=1.
       do 11 j=1,ia
        x=x*ran2(idum)
 11    continue
       x=-dlog(x)
      else
C Use rejection method.
 1     v1=2.*ran2(idum)-1.
       v2=2.*ran2(idum)-1.
       if(v1**2+v2**2.gt.1.) goto 1
C (v1,v2) is a random (Uniform) point in the unit circle.
       y=v2/v1
C y is the tangent of a random (Uniform) angle from -pi to pi.
       am=ia-1
       s=dsqrt(2.*am+1.)
       x=s*y+am
C Now decide whether to reject x:
       if(x.lt.0.) goto 1
       e=(1.+y**2)*exp(am*dlog(x/am)-s*y)
C e is the ratio of the prob. fn. to comparison fn.
       if(ran2(idum).gt.e) goto 1
C reject on basis of a second Uniform deviate.
      endif
      gamdev=x
      return
      END


      SUBROUTINE rnormal(X, n, mu, sigma ,seed)
      INTEGER n, seed
      DOUBLE PRECISION X(n), mu(n),sigma(n), Zi
C Generates n N(mu, sigma^2) random variables in X.
C Uses ran2(seed) for U(0,1) deviates.
      INTEGER i
      DOUBLE PRECISION gasdev
      do 100 i=1,n
       Zi=gasdev(seed)
       X(i)=sigma(i)*Zi + mu(i)
 100  continue
      return
      end


      SUBROUTINE rnorm(Z1,Z2,seed)
C Generates two independent N(0,1) random variables Z1 and Z2.
      double precision U1,U2,Z1,Z2,pi,ran2
      integer seed
      pi=3.14159265
      U1=ran2(seed)
      U2=ran2(seed)
      z1=dcos(2.*pi*u2)*dsqrt((-2.)*dlog(U1))
      Z2=dsin(2.*pi*U2)*dsqrt((-2.)*dlog(U1))
      return
      end

      SUBROUTINE rchisq(x, df, seed)
      INTEGER seed
C Returns one draws from the (unconstrained) Chi-sqare(df) distribution.
C df is double precision for consistency, but is assumed to be an integer.
C *** Calls gamdev ***
C
      INTEGER ia
      DOUBLE PRECISION x,z, df, gamdev, gasdev
      ia=int(df/2.)
C gamdev can only handle integer alpha.
      if(ia.gt.0) then
       x=gamdev(ia, seed)
C x ~ Gamma(ia, 1)
      endif
      if(2*ia.lt.int(df)) then
C Generate z ~ N(0,1) and add z^2/2 to x. z^2/2 ~ Gamma(1/2, 1).
       z=gasdev(seed)
       x=x+z**2/2.
C x ~ Gamma(df/2, 1)
      endif
      x=x*2.
C x ~ Gamma(df/2, 1/2);  i.e., x ~ Chi-square(df).
      return
      END




      SUBROUTINE rcchisq(x, df, pd, chitab, seed)
C Returns one draw from the constrained Chi-square distribution:
C a Chi-square(df) draw that is constrained to be less than d,
C where pd is the probability that a Chi-square(df) is < d.
C "chitab" is a table of Chi-square(df) quantiles.
C
      INTEGER seed
      DOUBLE PRECISION x, df, pd, chitab(999),  p, ran2
      p = pd*ran2(seed)
      call qchisq(p, df, chitab, x)
      return
      end

C
C %%%%%%%%%%%%%%%%%%%% QUANTILES %%%%%%%%%%%%%%%%%%%%
C
C

      SUBROUTINE qchisq(p, df, chitab, q)
C Returns the pth quantile of the Chi-square(df) distribution as q.
C chitab is a table of equally spaced Chi-square(df) quantiles.
      INTEGER  ITMAX, cp, i
      DOUBLE PRECISION p, df, chitab(999), q, EPS, l, u, fl, fu, sd,gammp
      PARAMETER (ITMAX=1000, EPS=3.e-7)
      if(p.gt.0.9990) then
       l = chitab(999)
       fl= 0.9990
       sd = dsqrt(df*2.)
       u = l+sd
       call pchisq(u, df, fu)
       do while(fu.lt.p)
        l = u
        fl = fu
        u = u+sd
        call pchisq(u, df, fu)
       end do
      else
       if(p.lt.0.001) then
        l = EPS
        fl = EPS
        u=chitab(1)
        fu=0.0010
       else
	cp = int(1000.*p)
        fl = cp/1000.
	l = chitab(cp)
	fu = (cp+1)/1000.
	u = chitab(cp+1)
       endif
      endif
C now l and u bracket the desired quantile q, with l < q < u
C  and fl < p < fu.
C
      q=(l+u)/2.
      i=0
      do while(u-l.gt.EPS*q.and.i.lt.ITMAX)
       i = i+1
       call bisect(p, df, l, q, u,fl, fu)
      end do
      return
      end


      SUBROUTINE pchisq(xx,df,p)
C Call gammp to return p, the CDF of the Chi-square(df) distribution at x.
      DOUBLE PRECISION df, gammp, x, xx, p, a
      a = df/2.
      x = xx/2.
      p = gammp(a, x)
      return
      end





      SUBROUTINE bisect(p,df, l, mid, u, fl, fu)
C Bisects a lower and upper bound on the Chi-square(df) CDF
C to return a narrower bound.
      DOUBLE PRECISION df, p, a, x, l, u, fl, fu, mid, fm, gammp
      a = df/2.
      mid = (l+u)/2.
      x = mid/2.
      fm = gammp(a, x)
      if(fm.lt.p) then
       l = mid
       fl = fm
      else
       u = mid
       fu = fm
      endif
      return
      end




      DOUBLE PRECISION FUNCTION gammln(xx)
C Returns the value of Lanczos' approximation to ln(gamma(xx)), for n > 0.
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
C
      INTEGER j
      DOUBLE PRECISION xx, ser, stp, tmp, x, y, cof(6)
      SAVE cof, stp
      DATA cof, stp/76.18009172947146d0, -86.50532032941677d0,
     $ 24.01409824083091d0, -1.231739572450155d0,
     $ 0.1208650973866179d-2, -.5395239384953d-5,
     $ 2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*dlog(tmp)-tmp
      ser=1.000000000190015d0
      do 10 j=1,6
       y=y+1.d0
       ser=ser+cof(j)/y
 10   continue
      gammln=tmp+dlog(stp*ser/x)
      RETURN
      END


      DOUBLE PRECISION FUNCTION gammp(a,x)
C Returns the incomplete gamma function P(a,x).
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
      DOUBLE PRECISION a, x, gammcf, gamser,gln
      if(x.lt.a+1.) then
C use series representation
       call gser(gamser,a,x,gln)
       gammp=gamser
      else
C use continued fraction representation
       call gcf(gammcf,a,x,gln)
       gammp = 1.-gammcf
      endif
      return
      end

      SUBROUTINE gser(gamser,a,x,gln)
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
      INTEGER ITMAX, n
      DOUBLE PRECISION a, gamser, gln, x, EPS, ap, del, sum, gammln
      PARAMETER (ITMAX=100, EPS=3.e-7)
      gln=gammln(a)
      ap=a
      sum=1./a
      del=sum
      do 10 n=1,ITMAX
       ap=ap+1.
       del=del*x/ap
       sum=sum+del
       if(abs(del).lt.abs(sum)*EPS) goto 20
 10   continue
 20   gamser=sum*exp(-x+a*dlog(x)-gln)
      return
      end

      SUBROUTINE gcf(gammcf, a,x,gln)
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
      INTEGER ITMAX, i
      DOUBLE PRECISION a, x, gammcf, gln, EPS, FPMIN,
     $ an,b,c,d,del,h,gammln
      PARAMETER (ITMAX=100,EPS=3.e-7, FPMIN=1.e-30)
      gln=gammln(a)
      b=x+1.-a
      c=1./FPMIN
      d=1./b
      h=d
      do 10 i=1,ITMAX
       an=-i*(i-a)
       b=b+2.
       d=an*d+b
       if(abs(d).lt.FPMIN) d=FPMIN
       c=b+an/c
       if(abs(c).lt.FPMIN) c=FPMIN
       d=1./d
       del=d*c
       h=h*del
       if(abs(del-1.).lt.EPS) goto 20
 10   continue
 20   gammcf=exp(-x+a*dlog(x)-gln)*h
      return
      end

C
C
C
C %%%%%%%%%%%%%%%%%%%% MATRIX MANIPULATIONS %%%%%%%%%%%%%%%%%%%%

      SUBROUTINE ludcmp(a,n,np,indx,d)
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
C Modified by P.Everson 1/23/2000.
C
      INTEGER  n,np,indx(np),NMAX
      DOUBLE PRECISION d, a(np,np), TINY
C NMAX is the largest expected n; TINY is a small number.
      PARAMETER (NMAX=500, TINY=1.0e-20)
C
C Given a matrix a(1:n,1:n), with physical dimensions np x np, this
C subroutine replaces it by the LU decomposition of a rowwise permutation
C of itself. a and n are input. a is output, but rearranged; indx(1:n) is
C an output vector that records the row permutation effected by the
C partial pivoting; d is output as +/- 1 depending on whether the number
C of row interchanges was even or odd, respectively (odd -> d = -1).
C This routine is used in conjunction with lubksb to solve linear
C equations, invert a matrix, or compute a (log) determinant.
C
      INTEGER  i,imax,j,k
      DOUBLE PRECISION aamax, dum, sum, vv(NMAX)
C vv stores the implicit scaling of each row.
      d=1.
C No row interchanges yet.
      sing=0
C Loop over rows to get implicit scaling information.
      do 12 i=1,n
       aamax=0.
       do 11 j=1,n
        if(abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
 11    continue
       vv(i)=1./aamax
C Save the scaling.
 12    continue
       do 19 j=1,n
C This is the loop over columns of Crout's method.
        do 14 i=1,j-1
         sum=a(i,j)
         do 13 k=1,i-1
          sum=sum-a(i,k)*a(k,j)
 13      continue
         a(i,j)=sum
 14     continue
        aamax=0.
C Initialization for the search for the largest pivot element.
        do 16 i=j,n
         sum=a(i,j)
         do 15 k=1,j-1
          sum=sum-a(i,k)*a(k,j)
 15      continue
         a(i,j)=sum
         dum=vv(i)*abs(sum)
C Figure of merit for the pivot. Is better than the best so far?
         if(dum.ge.aamax) then
          imax=i
          aamax=dum
         endif
 16     continue
C Do we need to interchange rows?
        if(j.ne.imax) then
C Yes, do so...
         do 17 k=1,n
          dum=a(imax,k)
          a(imax,k)=a(j,k)
          a(j,k)=dum
 17      continue
C ... and change the parity of d.
         d=-d
C Also interchange the scale factor.
         vv(imax)=vv(j)
        endif
        indx(j)=imax
        if(a(j,j).eq.0.) a(j,j)=TINY
C If the pivot element is 0.0 the matrix is singular. Replace 0
C by a TINY number.
        if(j.ne.n) then
C Now divide by the pivot element.
         dum=1./a(j,j)
         do 18 i=j+1,n
          a(i,j)=a(i,j)*dum
 18      continue
        endif
 19    continue
       return
       end


      SUBROUTINE lubksb(a,n,np,indx,b)
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
C Modified by P.Everson 1/23/2000.
C
      INTEGER n,np,indx(np)
      DOUBLE PRECISION a(np,np), b(n)
C Solves the set of linear equations AX = B. Here a is input, not as
C the matrix A, but rather its LU decomposition, determined by the
C routine ludcmp. b(1:n) is input as the right-hand side vector B,
C and returns with the solution vector X. a, n, np and index are not
C modified by this routine and can be left in place for successive calls
C witH different right-hand sides b.
      INTEGER i, ii, j, ll
      DOUBLE PRECISION sum
      ii=0
C When ii is set to a positive value, it will become the index of the
C first nonvanishing element of b. We now do the forward substitution.
C The only new wrinkle is to unscramble the permutation as we go.
      do 12 i=1,n
       ll=indx(i)
       sum=b(ll)
       b(ll)=b(i)
       if(ii.ne.0) then
        do 11 j=ii,i-1
         sum=sum-a(i,j)*b(j)
 11     continue
       else if(sum.ne.0.) then
C A nonzero element was encountered, so from now on we will have
C to do the sums in the loop above.
        ii=i
       endif
       b(i)=sum
 12   continue
C Now we do the backsubstitution.
      do 14 i=n,1,-1
       sum=b(i)
       do 13 j=i+1,n
        sum=sum-a(i,j)*b(j)
 13    continue
       b(i)=sum/a(i,i)
 14   continue
      return
      end


      SUBROUTINE solve(A, B, n, np, m, mp, indx)
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
C Modified by P.Everson 1/23/2000.
C
      INTEGER n,np, m, mp, indx(np), j
      DOUBLE PRECISION A(np,np), B(np,mp), d
C Call ludcmp and lubksb to solve the equation AX = B. The nxn
C matrix A and nxm matrix B are input, and X is output as B.
C
      call ludcmp(A, n, np, indx, d)
C get LU decomposition of A; now solve the m equations.
      do 100 j=1,m
       call lubksb(A, n, np, indx,B(1,j))
 100  continue
      return
      end



      SUBROUTINE mmult(A,B,p,q,r,C, pp,qq,rr)
C
C Returns the matrix product A%*%B = C. p,q,r are physical dimensions;
C pp,qq,rr are corresponding working dimensions.
C
      INTEGER p,q,r,pp,qq,rr,i,j,k
      DOUBLE PRECISION t, A(p,q), B(q,r), C(p,r)
      do 100 i=1,pp
       do 200 j=1,rr
        t=0.0
        do 300 k=1,qq
         t=t+A(i,k)*B(k,j)
 300    continue
        C(i,j)=t
 200   continue
 100  continue
      RETURN
      END



      SUBROUTINE mammult(M, Mt, A, p, N, U, V, MAM)
C
C Pre- and post-multiplies each pxp element of a pxpxn array (A)
C by a pxp matrix (M) and its transpose (Mt), respectively.
C
      INTEGER p, N, i, j, ind
      DOUBLE PRECISION M(p,p), Mt(p,p), A(p,p,N)
      DOUBLE PRECISION MAM(p,p,N), U(p,p), V(p,p)
C
      do 500 ind=1,N
       do 200 i=1,p
        do 100 j=1,p
         U(i,j)=A(i,j,ind)
 100	 continue
 200	continue
	call mmult(M,U,p,p,p,V,p,p,p)
	call mmult(V,Mt,p,p,p,U,p,p,p)
	do 400 i=1,p
	 do 300 j=1,p
	  MAM(i,j,ind)=U(i,j)
 300	 continue
 400    continue
 500   continue
      RETURN
      END



      SUBROUTINE rtmat(A, p, eig, G, GD)
C Returns A as its pxp symetric matrix square-root.
C Calls Fortran subroutine jacobid.
      INTEGER p
      DOUBLE PRECISION A(p,p), eig(p), G(p,p), GD(p,p)
C
      INTEGER i,j,nrot
      DOUBLE PRECISION dum
      call jacobid(A,p,p,eig,G,nrot)
C eig now contains the eigenvalues of A, and the corresponding
C eigenvectors are now the columns of G.
C Set A as the diagonal matrix of eigenvalue square roots:
      do 200 i=1,p
       do 100 j=1,p
        A(i,j)=0.
 100   continue
       A(i,i)=dsqrt(eig(i))
 200  continue
      call mmult(G,A,p,p,p,GD,p,p,p)
C Replace G by its transpose:
      do 400 i=1,p
       do 300 j=1,i
        dum=G(i,j)
        G(i,j)=G(j,i)
        G(j,i)=dum
 300   continue
 400  continue
      call mmult(GD,G,p,p,p,A,p,p,p)
      RETURN
      END



      SUBROUTINE jacobid(a,n,np,d,v,nrot)
C
C ***revised by PE for double precision for use with Splus***
C
C Computes all eigenvalues and eigenvectors of a real symmetric
C matrix a, which is of size n, stored in a physical np x np array.
C On output, elements of a above the diagonal are destroyed.
C d returns the eigenvalues of a in its first n elements.
C v is a matrix with the same logical and physical dimensions as a,
C whose columns contain, on output, the normalized eigenvectors of a.
C nrot returns the number of jacobi rotations that were required.
C
      INTEGER n,np,nrot,NMAX
      Double Precision a(np,np),d(np),v(np,np)
      PARAMETER (NMAX=500)
      INTEGER i,ip,iq,j
      Double Precision c,g,h,s,sm,t,tau,theta,tresh,b(NMAX),z(NMAX)
      do 12 ip=1,n
        do 11 iq=1,n
          v(ip,iq)=0.
11      continue
        v(ip,ip)=1.
12    continue
      do 13 ip=1,n
        b(ip)=a(ip,ip)
        d(ip)=b(ip)
        z(ip)=0.
13    continue
      nrot=0
      do 24 i=1,50
        sm=0.
        do 15 ip=1,n-1
          do 14 iq=ip+1,n
            sm=sm+abs(a(ip,iq))
14        continue
15      continue
        if(sm.eq.0.)return
        if(i.lt.4)then
          tresh=0.2*sm/n**2
        else
          tresh=0.
        endif
        do 22 ip=1,n-1
          do 21 iq=ip+1,n
            g=100.*abs(a(ip,iq))
            if((i.gt.4).and.(abs(d(ip))+
     *g.eq.abs(d(ip))).and.(abs(d(iq))+g.eq.abs(d(iq))))then
              a(ip,iq)=0.
            else if(abs(a(ip,iq)).gt.tresh)then
              h=d(iq)-d(ip)
              if(abs(h)+g.eq.abs(h))then
                t=a(ip,iq)/h
              else
                theta=0.5*h/a(ip,iq)
                t=1./(abs(theta)+sqrt(1.+theta**2))
                if(theta.lt.0.)t=-t
              endif
              c=1./sqrt(1+t**2)
              s=t*c
              tau=s/(1.+c)
              h=t*a(ip,iq)
              z(ip)=z(ip)-h
              z(iq)=z(iq)+h
              d(ip)=d(ip)-h
              d(iq)=d(iq)+h
              a(ip,iq)=0.
              do 16 j=1,ip-1
                g=a(j,ip)
                h=a(j,iq)
                a(j,ip)=g-s*(h+g*tau)
                a(j,iq)=h+s*(g-h*tau)
16            continue
              do 17 j=ip+1,iq-1
                g=a(ip,j)
                h=a(j,iq)
                a(ip,j)=g-s*(h+g*tau)
                a(j,iq)=h+s*(g-h*tau)
17            continue
              do 18 j=iq+1,n
                g=a(ip,j)
                h=a(iq,j)
                a(ip,j)=g-s*(h+g*tau)
                a(iq,j)=h+s*(g-h*tau)
18            continue
              do 19 j=1,n
                g=v(j,ip)
                h=v(j,iq)
                v(j,ip)=g-s*(h+g*tau)
                v(j,iq)=h+s*(g-h*tau)
19            continue
              nrot=nrot+1
            endif
21        continue
22      continue
        do 23 ip=1,n
          b(ip)=b(ip)+z(ip)
          d(ip)=b(ip)
          z(ip)=0.
23      continue
24    continue
      RETURN
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 8888.






      SUBROUTINE  aaprod(X,Y,p,q,r,n,res)
C
C Multiplies pxq and qxr matrix elements of two arrays
C and loads the result in res.
C
      INTEGER p,q,r,n,i,j,k,l
      DOUBLE PRECISION X(p,q,n), Y(q,r,n), res(p,r,n), t
      Do 100 k=1,n
       Do 110 i=1,p
        Do 120 j=1,r
         t=0.0
         Do 130 l=1,q
          t=t+X(i,l,k)*Y(l,j,k)
  130    continue
         res(i,j,k)=t
  120   continue
  110  continue
  100 continue
      RETURN
      END


      SUBROUTINE amprod(A,B,p,q,r,n,C)
C Post-multiplies each pxq matrix element of an array A by
C a qxr matrix B, and returns the result as C.
C
      INTEGER p,q,r,n,i,j,k,l
      DOUBLE PRECISION A(p,q,n),B(q,r),C(p,r,n),t
      do 100 i = 1,n
       do 200 j = 1,p
        do 300 k = 1,r
         t=0.0
         do 400 l = 1,q
          t=t+A(j,l,i)*B(l,k)
  400    continue
         C(j,k,i)=t
  300   continue
  200  continue
  100 continue
      RETURN
      END


      SUBROUTINE maprod(A,B,p,q,r,n,C)
C Pre-multiplies each qxr matrix element of an array B by
C a pxq matrix A, and returns the result as C.
      INTEGER p,q,r,n,i,j,k,l
      DOUBLE PRECISION A(p,q),B(q,r,n),C(p,r,n),t
      do 100 i = 1,n
       do 200 j = 1,p
        do 300 k = 1,r
         t=0.0
         do 400 l = 1,q
          t=t+A(j,l)*B(l,k,i)
  400    continue
         C(j,k,i)=t
  300   continue
  200  continue
  100 continue
      RETURN
      END


      SUBROUTINE mmmsum(X,Y,Z,p,q,r,s,n,res1,res)
C Multiplies matrices from 3 arrays, sums over the third
C dimension of the array and returns the result as res.
      INTEGER p,q,r,s,n,i,j,k,l
      DOUBLE PRECISION X(p,q,n), Y(q,r,n), Z(r,s,n)
      DOUBLE PRECISION res1(p,r), res(p,s), t
        Do 100 i=1,n
         Do 110 j=1,p
          Do 120 k=1,r
           t=0.0
           Do 130 l=1,q
            t=t+X(j,l,i)*Y(l,k,i)
 130        continue
           res1(j,k)=t
 120      continue
 110     continue
         Do 140 j=1,p
          Do 150 k=1,s
           t=0.0
           Do 160 l=1,r
            t=t+res1(j,l)*Z(l,k,i)
 160       continue
           res(j,k)=res(j,k)+t
 150      continue
 140     continue
 100    continue
      RETURN
      END


      SUBROUTINE inverse(A, n, np, Ainv, indx, ldet)
C Taken from Numerical Recipes in Fortran, 2nd ed., Cambridge University Press.
C Modified by P.Everson 1/23/2000.
      INTEGER n, np, indx(np), i, j
      DOUBLE PRECISION A(np,np), Ainv(np,np), ldet, d
C Returns Ainv as the nxn inverse of A. Returns ldet as the
C  log-determinant of A. Returns A as its LU decomposition.
C
C set Ainv as the identity matrix
      do 200 i=1,n
       do 100 j=1,n
        Ainv(i,j)=0.
 100   continue
       Ainv(i,i)=1.
 200  continue
C LU decompose A:
      call ludcmp(A, n, np, indx, d)
C Compute log-determinant:
      ldet=0.
      do 300 i=1,n
       ldet=ldet+dlog(abs(A(i,i)))
 300  continue
C Now find inverse by columns:
      do 400 j=1,n
       call lubksb(A, n, np, indx, Ainv(1,j))
 400  continue
      RETURN
      END
