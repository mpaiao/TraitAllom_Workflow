!---~---
! PROGRAM SplitAuthor
! 
! This program reads the original TRY data base files and splits the data into files for
! authors. It also removes non-regular characters and substitute them with basic ones.
!---~---
program split_author
   implicit none

   !--- Parameters
   integer, parameter      :: str_len      = 1000    ! String length
   integer, parameter      :: report_every = 25000   ! Report progress
   logical, parameter      :: is_debug     = .false. ! Debug code?
   !--- Variable declaration
   character(len=100), dimension(27)  :: header                ! Header names.
   character(len=str_len)             :: input_file            ! Input file.
   character(len=str_len)             :: author_file           ! Author file.
   character(len=str_len)             :: LastName              ! The list below should be
   character(len=str_len)             :: FirstName             !    the default list of
   character(len=str_len)             :: DatasetID             !    entries in the TRY data
   character(len=str_len)             :: Dataset               !    base.
   character(len=str_len)             :: SpeciesName           !
   character(len=str_len)             :: AccSpeciesID          !
   character(len=str_len)             :: AccSpeciesName        !
   character(len=str_len)             :: ObservationID         !
   character(len=str_len)             :: ObsDataID             !
   character(len=str_len)             :: TraitID               !
   character(len=str_len)             :: TraitName             !
   character(len=str_len)             :: DataID                !
   character(len=str_len)             :: DataName              !
   character(len=str_len)             :: OriglName             !
   character(len=str_len)             :: OrigValueStr          !
   character(len=str_len)             :: OrigUnitStr           !
   character(len=str_len)             :: ValueKindName         !
   character(len=str_len)             :: OrigUncertaintyStr    !
   character(len=str_len)             :: UncertaintyName       !
   character(len=str_len)             :: Replicates            !
   character(len=str_len)             :: StdValue              !
   character(len=str_len)             :: UnitName              !
   character(len=str_len)             :: RelUncertaintyPercent !
   character(len=str_len)             :: OrigObsDataID         !
   character(len=str_len)             :: ErrorRisk             !
   character(len=str_len)             :: Reference             !
   character(len=str_len)             :: Comment               !
   character(len=str_len)             :: LastNameNoDotSpace    ! Variables used for building
   character(len=str_len)             :: FirstNameNoDotSpace   !    the file name. We remove
                                                               !    spaces and dots to avoid
                                                               !    OS problems.
   integer                            :: nline                 ! Line count
   integer                            :: i                     ! Counter
   integer                            :: ioerr                 ! I/O error.
   logical                            :: iexist                ! Flag for existing file
   !---~---




   !--- First, make sure that the user provided a file name.
   if (command_argument_count() < 1) then
      stop ' FATAL ERROR. Please provide the input file name as argument!'
   else
      call get_command_argument(1,input_file)
   end if
   !---~---

   !---~---
   !   Read file header. It will be useful for distributing the data across authors
   !---~---
   write(unit=*,fmt='(a,1x,2a)') " Split file ",trim(input_file),"."
   open (unit=10,file=trim(input_file),status='old',action='read',iostat=ioerr)
   read (unit=10,fmt=*) ( header(i),i=1,27)
   !---~---



   !---~---
   !   Read each of the lines until the end.
   !---~---
   ioerr = 0
   nline = 1
   read_loop: do
      !--- Read line.
      nline = nline + 1
      read (unit=10,fmt=*,iostat=ioerr)                                                          &
          LastName,FirstName,DatasetID,Dataset,SpeciesName,AccSpeciesID,AccSpeciesName           &
         ,ObservationID,ObsDataID,TraitID,TraitName,DataID,DataName,OriglName,OrigValueStr       &
         ,OrigUnitStr,ValueKindName,OrigUncertaintyStr,UncertaintyName,Replicates,StdValue       &
         ,UnitName,RelUncertaintyPercent,OrigObsDataID,ErrorRisk,Reference,Comment
      !---~---


      !--- End file reading in case an exception occurred.
      select case (ioerr)
      case (-2,-1)
         !--- End of file reached. Normal exit.
         exit read_loop
         !---~---
      case (0)
         !--- File read normally. Keep going.
         continue
         !---~---
      case default
         !--- Serious error, report line number and quit.
         write(unit=*,fmt='(a,2(1x,i9,a))')                                                      &
            '   - FATAL ERROR (',ioerr,') whilst reading file (line ',nline,').'
         stop "File problem prevented successful run"
         !---~---
      end select
      !---~---



      !--- Remove diacritical markings from all text
      call remove_diacritics(LastName             )
      call remove_diacritics(FirstName            )
      call remove_diacritics(DatasetID            )
      call remove_diacritics(Dataset              )
      call remove_diacritics(SpeciesName          )
      call remove_diacritics(AccSpeciesID         )
      call remove_diacritics(AccSpeciesName       )
      call remove_diacritics(ObservationID        )
      call remove_diacritics(ObsDataID            )
      call remove_diacritics(TraitID              )
      call remove_diacritics(TraitName            )
      call remove_diacritics(DataID               )
      call remove_diacritics(DataName             )
      call remove_diacritics(OriglName            )
      call remove_diacritics(OrigValueStr         )
      call remove_diacritics(OrigUnitStr          )
      call remove_diacritics(ValueKindName        )
      call remove_diacritics(OrigUncertaintyStr   )
      call remove_diacritics(UncertaintyName      )
      call remove_diacritics(Replicates           )
      call remove_diacritics(StdValue             )
      call remove_diacritics(UnitName             )
      call remove_diacritics(RelUncertaintyPercent)
      call remove_diacritics(OrigObsDataID        )
      call remove_diacritics(ErrorRisk            )
      call remove_diacritics(Reference            )
      call remove_diacritics(Comment              )
      !---~---


      !---~---
      !   Remove dots and spaces from the last name (for building file names).
      !---~---
      LastNameNoDotSpace  = LastName
      FirstNameNoDotSpace = FirstName
      call remove_dot_space(LastNameNoDotSpace)
      call remove_dot_space(FirstNameNoDotSpace)
      !---~---



      !--- Update line number.
      if (mod(nline,report_every) == 0) then
         write(unit=*,fmt='(a,1x,i9,a)') " + Read line ",nline,"."
      end if
      !---~---


      !--- Create author file and check if the file exists.
      write(unit=author_file,fmt='(3a)')                                                   &
         trim(LastNameNoDotSpace),trim(FirstNameNoDotSpace),'.csv'
      inquire(file=trim(author_file),exist=iexist)
      !---~---

      !--- Print information (useful for debugging only).
      if (is_debug) then
         write(unit=*,fmt='(a,1x,a)') "LastName              = ",trim(LastName             )
         write(unit=*,fmt='(a,1x,a)') "FirstName             = ",trim(FirstName            )
         write(unit=*,fmt='(a,1x,a)') "DatasetID             = ",trim(DatasetID            )
         write(unit=*,fmt='(a,1x,a)') "Dataset               = ",trim(Dataset              )
         write(unit=*,fmt='(a,1x,a)') "SpeciesName           = ",trim(SpeciesName          )
         write(unit=*,fmt='(a,1x,a)') "AccSpeciesID          = ",trim(AccSpeciesID         )
         write(unit=*,fmt='(a,1x,a)') "AccSpeciesName        = ",trim(AccSpeciesName       )
         write(unit=*,fmt='(a,1x,a)') "ObservationID         = ",trim(ObservationID        )
         write(unit=*,fmt='(a,1x,a)') "ObsDataID             = ",trim(ObsDataID            )
         write(unit=*,fmt='(a,1x,a)') "TraitID               = ",trim(TraitID              )
         write(unit=*,fmt='(a,1x,a)') "TraitName             = ",trim(TraitName            )
         write(unit=*,fmt='(a,1x,a)') "DataID                = ",trim(DataID               )
         write(unit=*,fmt='(a,1x,a)') "DataName              = ",trim(DataName             )
         write(unit=*,fmt='(a,1x,a)') "OriglName             = ",trim(OriglName            )
         write(unit=*,fmt='(a,1x,a)') "OrigValueStr          = ",trim(OrigValueStr         )
         write(unit=*,fmt='(a,1x,a)') "OrigUnitStr           = ",trim(OrigUnitStr          )
         write(unit=*,fmt='(a,1x,a)') "ValueKindName         = ",trim(ValueKindName        )
         write(unit=*,fmt='(a,1x,a)') "OrigUncertaintyStr    = ",trim(OrigUncertaintyStr   )
         write(unit=*,fmt='(a,1x,a)') "UncertaintyName       = ",trim(UncertaintyName      )
         write(unit=*,fmt='(a,1x,a)') "Replicates            = ",trim(Replicates           )
         write(unit=*,fmt='(a,1x,a)') "StdValue              = ",trim(StdValue             )
         write(unit=*,fmt='(a,1x,a)') "UnitName              = ",trim(UnitName             )
         write(unit=*,fmt='(a,1x,a)') "RelUncertaintyPercent = ",trim(RelUncertaintyPercent)
         write(unit=*,fmt='(a,1x,a)') "OrigObsDataID         = ",trim(OrigObsDataID        )
         write(unit=*,fmt='(a,1x,a)') "ErrorRisk             = ",trim(ErrorRisk            )
         write(unit=*,fmt='(a,1x,a)') "Reference             = ",trim(Reference            )
         write(unit=*,fmt='(a,1x,a)') "Comment               = ",trim(Comment              )
         stop 'Check'
      end if
      !---~---



      !--- In case the file doesn't exist, create a file and write header.
      if (iexist) then
         open(unit=11,file=trim(author_file),status='old',action='write',position='append')
      else
         write(unit=*,fmt='(a,1x,2a)') '   - Create file ',trim(author_file),'.'
         open(unit=11,file=trim(author_file),status='new',action='write',position='rewind')
         write(unit=11,fmt="(27('""',a,'""':','))") (trim(header(i)),i=1,27)
      end if
      !---~---


      !--- Write output to the file:
      write (unit=11,fmt="(27('""',a,'""':','))")                                                &
          trim(LastName),trim(FirstName),trim(DatasetID),trim(Dataset),trim(SpeciesName)         &
         ,trim(AccSpeciesID),trim(AccSpeciesName),trim(ObservationID),trim(ObsDataID)            &
         ,trim(TraitID),trim(TraitName),trim(DataID),trim(DataName),trim(OriglName)              &
         ,trim(OrigValueStr),trim(OrigUnitStr),trim(ValueKindName),trim(OrigUncertaintyStr)      &
         ,trim(UncertaintyName),trim(Replicates),trim(StdValue),trim(UnitName)                   &
         ,trim(RelUncertaintyPercent),trim(OrigObsDataID),trim(ErrorRisk),trim(Reference)        &
         ,trim(Comment)
      !---~---

      !--- Safely close the output file.
      close(unit=11,status='keep')
      !---~---
   end do read_loop
   !---~---


   stop ' program ended. success!'
   return
end program split_author
!---~---


!---~---
!   Sub-routine to remove diacritical marks.
!---~---
subroutine remove_diacritics(label)
   implicit none
   !--- Parameters
   integer               , parameter     :: str_len  = 1000 ! String length
                                                            ! Make this the same as the
                                                            !   main program.
   !--- Arguments.
   character(len=*)      , intent(inout) :: label           ! Label to be fixed
   !--- Internal variables
   character(len=str_len)                :: label_orig      ! Copy of the input label
   integer                               :: imax            ! Trimmed label length
   integer                               :: i               ! Counter
   integer                               :: iword           ! Integer equivalent of string
   integer                               :: oa              ! Initial position
   integer                               :: oz              ! Final position.
   !---~---


   !--- Find the length of the label
   label_orig = label
   imax       = len_trim(label_orig)
   !---~---


   !--- Loop through the word characters
   oa = 1
   do i=1,imax
      !--- Find the integer equivalent.
      iword = iachar(label_orig(i:i))
      !---~---

      !---~---
      !   Identify if the character has a diacritical marking. If so, replace it with the base
      !   letter.
      !---~---
      select case(iword)
      case (131)
         oz               = oa + 0
         label(oa:oz) = 'f'
      case (138)
         oz               = oa + 0
         label(oa:oz) = 'S'
      case (140)
         oz               = oa + 1
         label(oa:oz) = 'OE'
      case (142)
         oz               = oa + 0
         label(oa:oz) = 'Z'
      case (154)
         oz               = oa + 0
         label(oa:oz) = 's'
      case (156)
         oz               = oa + 1
         label(oa:oz) = 'oe'
      case (158)
         oz               = oa + 0
         label(oa:oz) = 'z'
      case (159,221)
         oz               = oa + 0
         label(oa:oz) = 'Y'
      case (170)
         oz               = oa + 0
         label(oa:oz) = 'a'
      case (174)
         oz               = oa + 0
         label(oa:oz) = 'R'
      case (176)
         oz               = oa + 0
         label(oa:oz) = 'o'
      case (178)
         oz               = oa + 0
         label(oa:oz) = '2'
      case (179)
         oz               = oa + 0
         label(oa:oz) = '3'
      case (181)
         oz               = oa + 0
         label(oa:oz) = 'u'
      case (185)
         oz               = oa + 0
         label(oa:oz) = '1'
      case (186)
         oz               = oa + 0
         label(oa:oz) = 'o'
      case (188)
         oz               = oa + 2
         label(oa:oz) = '1/4'
      case (189)
         oz               = oa + 2
         label(oa:oz) = '1/2'
      case (190)
         oz               = oa + 2
         label(oa:oz) = '3/4'
      case (192:197)
         oz               = oa + 0
         label(oa:oz) = 'A'
      case (198)
         oz               = oa + 1
         label(oa:oz) = 'AE'
      case (199)
         oz               = oa + 0
         label(oa:oz) = 'C'
      case (200:203)
         oz               = oa + 0
         label(oa:oz) = 'E'
      case (204:207)
         oz               = oa + 0
         label(oa:oz) = 'I'
      case (208,222)
         oz               = oa + 1
         label(oa:oz) = 'TH'
      case (209)
         oz               = oa + 0
         label(oa:oz) = 'N'
      case (210:214,216)
         oz               = oa + 0
         label(oa:oz) = 'O'
      case (215)
         oz               = oa + 0
         label(oa:oz) = 'x'
      case (217:220)
         oz               = oa + 0
         label(oa:oz) = 'U'
      case (223)
         oz               = oa + 1
         label(oa:oz) = 'ss'
      case (224:229)
         oz               = oa + 0
         label(oa:oz) = 'a'
      case (230)
         oz               = oa + 1
         label(oa:oz) = 'ae'
      case (231)
         oz               = oa + 0
         label(oa:oz) = 'c'
      case (232:235)
         oz               = oa + 0
         label(oa:oz) = 'e'
      case (236:239)
         oz               = oa + 0
         label(oa:oz) = 'i'
      case (240,254)
         oz               = oa + 1
         label(oa:oz) = 'th'
      case (241)
         oz               = oa + 0
         label(oa:oz) = 'n'
      case (242:246,248)
         oz               = oa + 0
         label(oa:oz) = 'o'
      case (247)
         oz               = oa + 0
         label(oa:oz) = '/'
      case (249:252)
         oz               = oa + 0
         label(oa:oz) = 'u'
      case (253,255)
         oz               = oa + 0
         label(oa:oz) = 'y'
      case default
         oz               = oa + 0
         label(oa:oz) = label_orig(i:i)
      end select
      !---~---


      !--- Update oa to be one position after oz
      oa = oz + 1
      !---~---
   end do
   !---~---

   return
end subroutine remove_diacritics
!---~---


!---~---
!   Sub-routine to remove diacritical marks.
!---~---
subroutine remove_dot_space(label)
   implicit none
   !--- Parameters
   integer         , parameter     :: str_len  = 1000 ! String length.
                                                      ! Make this the same as the
                                                      !   main program.
   !--- Arguments.
   character(len=*), intent(inout) :: label           ! Label to be fixed
   !--- Internal variables
   character(len=str_len)          :: label_orig      ! Copy of the input label
   integer                         :: imax            ! Trimmed label length
   integer                         :: i               ! Counter
   integer                         :: oa              ! Initial position
   integer                         :: oz              ! Final position.
   !---~---


   !--- Find the length of the label
   label_orig = label
   imax       = len_trim(label_orig)
   label      = ''
   !---~---


   !--- Loop through the word characters
   oa = 1
   do i=1,imax
      !---~---
      !   Identify if the character has a diacritical marking. If so, replace it with the base
      !   letter.
      !---~---
      select case(label_orig(i:i))
      case (' ','.','(',')')
         !--- Skip character.
         oz           = oa -1
         !---~---
      case default
         !--- Copy character.
         oz           = oa + 0
         label(oa:oz) = label_orig(i:i)
         !---~---
      end select
      !---~---


      !--- Update oa to be one position after oz
      oa = oz + 1
      !---~---
   end do
   !---~---

   return
end subroutine remove_dot_space
!---~---



