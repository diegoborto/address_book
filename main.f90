module modu
implicit none

integer :: n, i, j, more

type person
 character*10 :: namep
 character*20 :: surname
 integer :: age
end type

type(person), allocatable :: list(:)

contains

subroutine swap()
implicit none
type(person) :: tmp

tmp = list(j-1)
list(j-1) = list(j)
list(j) = tmp

return
end subroutine swap

subroutine sort()
implicit none

do i=1,n
 do j = 2, n-(i-1)
   if (list(j-1)%age > list(j)%age) then
     call swap()
   endif
 enddo
enddo

return
end subroutine sort

subroutine printlist()
implicit none
!n = size(list)
write(*,*) 'The people in the list are'
write(*,*) 'Name        Surname         Age'
do i = 1,n
  write(*,*) list(i)
enddo

return
end subroutine printlist

subroutine add()
implicit none
type(person), allocatable :: tmpadd(:)
allocate(tmpadd(n+more))
tmpadd%namep = 'Pinco'
tmpadd%surname = 'Pallo'
tmpadd%age = 99
do i=1,n
  tmpadd(i) = list(i)
enddo
deallocate(list)
allocate(list(n+more))
do i =1, n+more
  list(i) = tmpadd(i)
enddo

write(*,*) 'Add a person'
do i =n+1,n + more
  write(*,*) 'Add the', i ,'person'
  write(*,*) 'Insert the name of the person (max 10 char)'
  read(*,*) list(i)%namep
  write(*,*) 'Insert the surname of the person (max 20 char)'
  read(*,*) list(i)%surname
  write(*,*) 'Insert the age of the person'
  read(*,*) list(i)%age
enddo
n = size(tmpadd)
!write(*,*) n, size(list)
deallocate(tmpadd)
return
end subroutine add

end module
!----------------------------------------------------------
program main
use modu
implicit none
!integer ::  i

!type(person), allocatable :: list(:)

write(*,*) 'How many people do you want to add?'
do 
 read(*,*) n
 if (n > 0 ) exit
enddo

allocate(list(n))

do i=1,n
  write(*,*) 'Person n°', i
  write(*,*) 'Insert the name of the person (max 10 char)'
  read(*,*) list(i)%namep
  write(*,*) 'Insert the surname of the person (max 20 char)'
  read(*,*) list(i)%surname
  write(*,*) 'Insert the age of the person'
  read(*,*) list(i)%age
enddo

call printlist()

call sort()

call printlist()

write(*,*) 'How many people do you want to add?'
read(*,*) more
if (more > 0) then
call add()
endif

call sort()
call printlist()

deallocate(list)
stop
end program main

