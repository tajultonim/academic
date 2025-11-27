program combination
  
integer::n,r,i,factn,factr,factnr,ncr
write(*,*)"this program calculate ncr"
write(*,*)"enter n and r"
read(*,*) n,r
 factn=1
 do i=1,n 
    factn=factn*i
 end do

 factr=1
 do i=1,r 
    factr=factr*i
 end do
 factnr=1
 do i=1,n-r
   factnr=factnr*i 
 end do 
 ncr=factn/(factr*factnr)
 write(*,*)'ncr=',ncr 
end program