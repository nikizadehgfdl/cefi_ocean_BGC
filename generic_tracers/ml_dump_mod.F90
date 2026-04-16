module ml_dump_mod
  implicit none
  private
  public :: ml_dump_init, ml_dump_record, ml_dump_close

  logical :: ml_opened = .false.
  integer :: ml_unit = 99
  character(len=:), allocatable :: ml_filename

contains

subroutine ml_dump_init(fname)
  character(len=*), intent(in) :: fname
  integer :: ierr
  if (.not. ml_opened) then
    ml_filename = trim(fname)
    open(unit=ml_unit, file=ml_filename, status='unknown', action='write', position='append', iostat=ierr)
    if (ierr /= 0) then
       write(*,*) 'ml_dump_mod: ERROR opening file ', trim(fname), ' iostat=', ierr
       return
    end if
    ! write header
    write(ml_unit,'(A)') 'Temp,Sal,dic,po4,sio4,alk,htotallo,htotalhi,htotal,zt,co2star,alpha,pCO2surf,co3_ion,omega_arag,omega_calc'
    ml_opened = .true.
  end if
end subroutine ml_dump_init

subroutine ml_dump_record(temp,salt,dic,po4,sio4,alk,htotallo,htotalhi,htotal,zt,co2star,alpha,pco2surf,co3_ion,omega_arag,omega_calc)
  real, intent(in) :: temp,salt,dic,po4,sio4,alk,htotallo,htotalhi,htotal,zt
  real, intent(in) :: co2star,alpha,pco2surf,co3_ion,omega_arag,omega_calc
  if (.not. ml_opened) then
    call ml_dump_init('co2_training_dump.csv')
  end if
  ! Write all reals in scientific notation with 4 decimal places (width 12)
  write(ml_unit,'(15(E12.4,","),E12.4)') &
       ,temp,salt,dic,po4,sio4,alk,htotallo,htotalhi,htotal,zt,co2star,alpha,pco2surf,co3_ion,omega_arag,omega_calc
end subroutine ml_dump_record

subroutine ml_dump_close()
  if (ml_opened) then
    close(unit=ml_unit)
    ml_opened = .false.
  end if
end subroutine ml_dump_close

end module ml_dump_mod
