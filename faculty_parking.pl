% Faculty/Staff lots
faculty_staff_lot(e7).
faculty_staff_lot(e2).
faculty_staff_lot(e3).
faculty_staff_lot(e1).
faculty_staff_lot(e6).
faculty_staff_lot(e).

% 3-hour Faculty/Staff stalls
three_hour_faculty_staff_stall(f1_stall1).
three_hour_faculty_staff_stall(f1_stall2).

% distance between 2 parking stalls
distance(f1_stall1, f1_stall2, 300).
distance(f1_stall1, f1, 1000).

may_park(Lot, HasValidPermit) :-
    faculty_staff_lot(Lot),
    HasValidPermit = true.

may_park(Stall, HasValidPermit) :-
    three_hour_faculty_staff_stall(Stall),
    HasValidPermit = true.

% re-parking within 500 feet in 24 hours is not allowed
violation(FromSpot, ToSpot, HoursSinceMove, HasValidPermit, reparking_within_500feet_in_24hours) :-
    HoursSinceMove < 24,
    distance(FromSpot, ToSpot, Feet),
    Feet =< 500,
    HasValidPermit = true.
