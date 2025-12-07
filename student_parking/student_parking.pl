:- module(student_parking, [
    student_parking_violation/13
]).

% 1. Student lots (must be in one of these for normal student parking)
student_lot(s8).
student_lot(s10).
student_lot(s).      % generic student lot "S"
student_lot(esps).
student_lot(enps).
student_lot(scps).
student_lot(nps).

% 3. Faculty lots (students only allowed after 5pm)
faculty_lot(e).
faculty_lot(e1).
faculty_lot(e2).
faculty_lot(e3).
faculty_lot(e6).
faculty_lot(e7).

% 4. Locations where stopping/standing/parking is prohibited
prohibited_location(sidewalk).
prohibited_location(driveway).
prohibited_location(crosswalk).
prohibited_location(roadway).
prohibited_location(landscaped).


student_parking_violation(_Lot, _Hour,
                          HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          _CurbColor, _InDisabledSpace,
                          _SpecialZone, _IsLoading,
                          no_valid_student_permit) :-
    HasStudentPermit \== true.

student_parking_violation(Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          _CurbColor, _InDisabledSpace,
                          _SpecialZone, _IsLoading,
                          wrong_lot_for_student) :-
    % Not a student lot AND not a faculty lot (faculty lots have their own rule)
    \+ student_lot(Lot),
    \+ faculty_lot(Lot).


student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          _CurbColor, true,              % InDisabledSpace = true
                          _SpecialZone, _IsLoading,
                          disabled_space_without_permit) :-
    HasDisabledPermit \== true.


student_parking_violation(Lot, Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          _CurbColor, _InDisabledSpace,
                          _SpecialZone, _IsLoading,
                          faculty_lot_before_5pm) :-
    faculty_lot(Lot),
    Hour < 17.


student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          Location, _DFH, _DB, _DR,
                          _CurbColor, _InDisabledSpace,
                          _SpecialZone, _IsLoading,
                          prohibited_location(Location)) :-
    prohibited_location(Location).


student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, DistFireHydrant, _DB, _DR,
                          _CurbColor, _InDisabledSpace,
                          _SpecialZone, _IsLoading,
                          too_close_to_fire_hydrant) :-
    number(DistFireHydrant),
    DistFireHydrant < 15.

student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, DistBuilding, _DR,
                          _CurbColor, _InDisabledSpace,
                          _SpecialZone, _IsLoading,
                          too_close_to_building) :-
    number(DistBuilding),
    DistBuilding < 15.

student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, _DB, DistRamp,
                          _CurbColor, _InDisabledSpace,
                          _SpecialZone, _IsLoading,
                          too_close_to_disabled_ramp) :-
    number(DistRamp),
    DistRamp < 5.

student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          red, _InDisabledSpace,
                          _SpecialZone, _IsLoading,
                          red_curb_no_parking).


student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          blue, _InDisabledSpace,
                          _SpecialZone, _IsLoading,
                          blue_curb_disabled_only) :-
    HasDisabledPermit \== true.


student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          yellow, _InDisabledSpace,
                          _SpecialZone, IsLoading,
                          yellow_curb_loading_only) :-
    IsLoading \== true.


student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          _CurbColor, _InDisabledSpace,
                          no_parking, _IsLoading,
                          no_parking_zone).

student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          _CurbColor, _InDisabledSpace,
                          fire_lane, _IsLoading,
                          fire_lane_zone).

student_parking_violation(_Lot, _Hour,
                          _HasStudentPermit, _HasDisabledPermit,
                          _Location, _DFH, _DB, _DR,
                          _CurbColor, _InDisabledSpace,
                          tow_away_zone, _IsLoading,
                          tow_away_zone).
