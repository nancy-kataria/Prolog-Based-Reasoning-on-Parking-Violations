?- may_park_overnight(s8, time(mon, 2), true).
?- may_park_overnight(s8, time(mon, 2), false).
?- overnight_violation(s8, time(mon, 2), false, 5, Reason).
?- may_park_overnight(s10, time(mon, 3), true).
?- overnight_violation(s10, time(mon, 3), true, 5, Reason).
?- overnight_violation(s8, time(mon, 2), true, 80, Reason).
?- overnight_violation(s10, time(mon, 10), true, 10, Reason).