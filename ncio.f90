module ncio

    use netcdf
    use caseConverter, only: to_lower

    implicit none

    private
    public :: ncinfo, ncopen, ncclose, ncread


    integer, parameter :: dim_max = 3

    type ncinfo
        private
        integer        :: unit
        character(256) :: file
        character(16)  :: action
        integer        :: record
        integer        :: recstep
        character(64)  :: varname
        integer        :: varid
        integer        :: ndim
        integer        :: shape(dim_max)
    end type ncinfo


    interface ncread
        module procedure &
          & ncread_1    , &
          & ncread_2    , &
          & ncread_3
    end interface ncread


    contains


    subroutine ncopen(ftype, file, action, tstart, tstep, var, ndim, shape)
        type(ncinfo), intent(out) :: ftype
        character(*), intent(in)  :: file
        character(*), intent(in)  :: action
        integer     , intent(in)  :: tstart
        integer     , intent(in)  :: tstep
        character(*), intent(in)  :: var
        integer     , intent(in)  :: ndim
        integer     , intent(in)  :: shape(ndim)

        integer :: stat
        integer :: ndim_true
        integer :: action_int

        if (tstart <= 0) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A,I0)') 'Invalid tstart : ', tstart
            write(0,'(A)')    'Argument "tstart" should be more than 0'
            ERROR STOP
        endif

        if (ndim > dim_max) then
            write(0,'(A)')    '<ERROR STOP>'
            write(0,'(A,I0)') 'Number of dimension out of limit : ', ndim
            write(0,'(A,I0)') 'Dimension limit is ', dim_max
            ERROR STOP
        endif

        if (any(shape(1:ndim)<=0)) then
            write(0,'(A)')                 '<ERROR STOP>'
            !write(0,'(A)',ADVANCE='NO') '"shape" includes 0 or negative value : shape=('
            write(0,'(A,*(I0,:,","),")")') '"shape" includes 0 or negative value : shape=(', shape(1:ndim)
            ERROR STOP
        endif

        if (to_lower(trim(action)) == 'read') then
            action_int = NF90_NOWRITE
        else
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A)') 'NCOPEN() for ' // trim(file)
            write(0,'(A)') 'Action ' // trim(action) // ' is not supported'
            ERROR STOP
        endif

        ftype%file          = trim(file)
        ftype%action        = trim(action)
        ftype%record        = tstart
        ftype%recstep       = tstep
        ftype%varname       = trim(var)
        ftype%ndim          = ndim
        ftype%shape(1:ndim) = shape(1:ndim)
    

        !! OPEN
        stat = NF90_OPEN(PATH=file      , &  !! IN
                       & MODE=action_int, &  !! IN
                       & NCID=ftype%unit  )  !! OUT

        call error_stop(ftype, &  !! IN
                      & stat   )  !! IN


        !! Get Variable ID
        stat = NF90_INQ_VARID(NCID =ftype%unit , &  !! IN
                            & NAME =trim(var)  , &  !! IN
                            & VARID=ftype%varid  )  !! OUT

        call error_stop(ftype, &  !! IN
                      & stat   )  !! IN


        !! Check the Dimension
        stat = NF90_INQUIRE(NCID       =ftype%unit, &  !! IN
                          & NDIMENSIONS=ndim_true   )  !! OUT

        if (ndim /= ndim_true-1) then
            write(0,'(A)') '<ERROR STOP>'
            write(0,'(A,I0,A,I0,A)') 'Number of dimensions of ' // trim(var) // ' is ', ndim_true-1, ' except the time dimension, but ', ndim, ' was specified'
            call ncclose(ftype)
            ERROR STOP
        endif

    end subroutine ncopen


    subroutine ncclose(ftype)
        type(ncinfo), intent(inout) :: ftype
        
        integer :: stat

        stat = NF90_CLOSE(ftype%unit)  !! IN

        ftype%unit    = -999999
        ftype%file    = 'CLOSED'
        ftype%action  = 'CLOSED'
        ftype%record  = -999999
        ftype%recstep = -999999
        ftype%varid   = -999999

    end subroutine ncclose


    subroutine ncread_1(ftype, input_data)
        integer, parameter :: rk=4
        integer, parameter :: dim=1
        type(ncinfo), intent(inout) :: ftype
        real(rk)    , intent(out)   :: input_data(ftype%shape(1))

        real(rk) :: work_reader(ftype%shape(1),1)
        integer  :: start(dim+1)
        integer  :: count(dim+1)
        integer  :: stat
        integer  :: i

        start(1:dim)   = 1
        start(dim+1)   = ftype%record
        count(1:dim+1) = [(ftype%shape(i), i=1,dim), 1]

        stat = NF90_GET_VAR(NCID  =ftype%unit                       , &  !! IN
                          & VARID =ftype%varid                      , &  !! IN
                          & VALUES=work_reader(1:ftype%shape(1),1:1), &  !! OUT
                          & START =start(1:dim+1)                   , &  !! IN
                          & COUNT =count(1:dim+1)                     )  !! IN
        call error_stop(ftype, &  !! IN
                      & stat   )  !! IN

        input_data(1:ftype%shape(1)) = work_reader(1:ftype%shape(1),1)

        ftype%record = ftype%record + ftype%recstep

    end subroutine ncread_1


    subroutine ncread_2(ftype, input_data)
        integer, parameter :: rk=4
        integer, parameter :: dim=2
        type(ncinfo), intent(inout) :: ftype
        real(rk)    , intent(out)   :: input_data(ftype%shape(1),ftype%shape(2))

        real(rk) :: work_reader(ftype%shape(1),ftype%shape(2),1)
        integer  :: start(dim+1)
        integer  :: count(dim+1)
        integer  :: stat
        integer  :: i

        start(1:dim)   = 1
        start(dim+1)   = ftype%record
        count(1:dim+1) = [(ftype%shape(i), i=1,dim), 1]

        stat = NF90_GET_VAR(NCID  =ftype%unit                                        , &  !! IN
                          & VARID =ftype%varid                                       , &  !! IN
                          & VALUES=work_reader(1:ftype%shape(1),1:ftype%shape(2),1:1), &  !! OUT
                          & START =start(1:dim+1)                                    , &  !! IN
                          & COUNT =count(1:dim+1)                                      )  !! IN
        call error_stop(ftype, &  !! IN
                      & stat   )  !! IN

        input_data(1:ftype%shape(1),1:ftype%shape(2)) = work_reader(1:ftype%shape(1),1:ftype%shape(2),1)

        ftype%record = ftype%record + ftype%recstep

    end subroutine ncread_2


    subroutine ncread_3(ftype, input_data)
        integer, parameter :: rk=4
        integer, parameter :: dim=3
        type(ncinfo), intent(inout) :: ftype
        real(rk)    , intent(out)   :: input_data(ftype%shape(1),ftype%shape(2),ftype%shape(3))

        real(rk) :: work_reader(ftype%shape(1),ftype%shape(2),ftype%shape(3),1)
        integer  :: start(dim+1)
        integer  :: count(dim+1)
        integer  :: stat
        integer  :: i

        start(1:dim)   = 1
        start(dim+1)   = ftype%record
        count(1:dim+1) = [(ftype%shape(i), i=1,dim), 1]

        stat = NF90_GET_VAR(NCID  =ftype%unit                                                         , &  !! IN
                          & VARID =ftype%varid                                                        , &  !! IN
                          & VALUES=work_reader(1:ftype%shape(1),1:ftype%shape(2),1:ftype%shape(3),1:1), &  !! OUT
                          & START =start(1:dim+1)                                                     , &  !! IN
                          & COUNT =count(1:dim+1)                                                       )  !! IN
        call error_stop(ftype, &  !! IN
                      & stat   )  !! IN

        input_data(1:ftype%shape(1),1:ftype%shape(2),1:ftype%shape(3)) = work_reader(1:ftype%shape(1),1:ftype%shape(2),1:ftype%shape(3),1)

        ftype%record = ftype%record + ftype%recstep

    end subroutine ncread_3


    subroutine error_stop(ftype, stat)
        type(ncinfo), intent(inout) :: ftype
        integer     , intent(in)    :: stat

        if (stat == NF90_NOERR) then
            return
        endif

        write(0,'(A)')                 '<ERROR STOP>'
        write(0,'(A)')                 trim(NF90_STRERROR(stat))
        write(0,'(A)')                 'FILE : '// trim(ftype%file)
        write(0,'(A,I0)')              'ERROR-CODE : ', stat
        write(0,'(A)')                 'VARIABLE : ' // trim(ftype%varname)
        write(0,'(A,I0)')              'T=', ftype%record
        write(0,'(A,I0)')              'TSTEP=', ftype%recstep
        write(0,'(A,*(I0,:,","),")")') 'SHAPE=(', ftype%shape(1:ftype%ndim)
        call ncclose(ftype)
        ERROR STOP

    end subroutine error_stop


end module ncio

