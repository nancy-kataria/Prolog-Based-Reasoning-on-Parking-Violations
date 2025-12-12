:- module(parking_logic, [next_step/2]).

:- ensure_loaded(student_parking).
:- ensure_loaded(faculty_parking).
:- ensure_loaded(overnight_parking).


/*  Public entry point
    next_step(+AnswersDict, -ResponseDict).

    ResponseDict is either:

    _{ type:question,
       key:role,
       text:"Are you a student or faculty/staff?",
       inputType:select,
       options:[student,faculty]
     }

    or

    _{ type:result,
       status:ok,              % or "violation"
       code:no_student_permit, % your internal reason
       label:"Eligible for parking" | "Valid parking" | ...
       message:"You may not park here because ..."
     }
*/

% 
next_step(Answers, Response) :-
    (   % role based questions
        \+ _{role:_} :< Answers
    ->  question_dict(role, Response)

        % goal
    ;   \+ _{goal:_} :< Answers
    ->  question_dict(goal, Response)

        % role specific questions - permit and lot
    ;   Answers.role == student
    ->  student_flow(Answers, Response)
    ;   Answers.role == faculty
    ->  faculty_flow(Answers, Response)
    ).


%  Question metadata as JSON-friendly dicts 

question_dict(Key, Dict) :-
    question(Key, Text, Type, Options),
    Dict = _{ type:question,
              key:Key,
              text:Text,
              inputType:Type,
              options:Options
            }.


question(role,
         "Are you a student or faculty/staff member?",
         select,
         [student, faculty]).

question(goal,
         "What would you like to check?",
         select,
         [check_parking_spot, check_parked_car]).

question(student_permit,
         "Do you have a valid student parking permit?",
         select,
         [true,false]).

question(faculty_permit,
         "Do you have a valid faculty/staff parking permit?",
         select,
         [true,false]).

question(disabled_permit,
         "Do you have a disabled parking permit?",
         select,
         [true,false]).

question(lot,
         "Which parking lot are you parking in?",
         text,
         []).

question(hour,
         "What time are you parking? (24-hour format, e.g. 14)",
         number,
         []).

question(space_type,
         "Where specifically are you parking?",
         select,
         [regular, disabled_space]).

question(curb_color,
         "What is the curb color?",
         select,
         [none, red, blue, yellow, white]).

question(near_object,
         "Are any of these within 15 ft of where you parked? (fire hydrant, building entrance, disabled access ramp)",
         select,
         [none, fire_hydrant, building_entrance, disabled_access_ramp]).


% Student flow 

student_flow(A, Response) :-
    % Need student_permit, lot, hour first
    (   \+ _{student_permit:_} :< A
    ->  question_dict(student_permit, Response)
    ;   \+ _{lot:_} :< A
    ->  question_dict(lot, Response)
    ;   \+ _{hour:_} :< A
    ->  question_dict(hour, Response)
    ;   early_student_decision(A, Response)
    ).

early_student_decision(A, Response) :-
    HasPermit = A.student_permit,
    Lot       = A.lot,
    Hour      = A.hour,

    % no permit + wrong lot
    (   HasPermit \== true,
        \+ student_lot(Lot),
        \+ faculty_lot(Lot)
    ->  violation_response(A.goal, no_permit_wrong_lot,
                           "You do not have a permit and are not in a valid lot.",
                           Response)
    ;   HasPermit \== true
    ->  violation_response(A.goal, no_student_permit,
                           "You do not have a valid student parking permit.",
                           Response)
    ;   \+ student_lot(Lot),
        \+ faculty_lot(Lot)
    ->  violation_response(A.goal, wrong_lot_for_student,
                           "That lot is not allowed for student parking.",
                           Response)
    ;   faculty_lot(Lot),
        Hour < 17
    ->  violation_response(A.goal, faculty_lot_before_5pm,
                           "Faculty lot before 5pm is not allowed for students.",
                           Response)
    ;   overnight_hour(Hour),
        overnight_violation(Lot, time(_,Hour), HasPermit, 0, Reason)
    ->  violation_response(A.goal, Reason,
                           "Your car would be in violation of overnight rules.",
                           Response)
    ;   continue_student_location(A, Response)
    ).

continue_student_location(A, Response) :-
    (   \+ _{space_type:_} :< A
    ->  question_dict(space_type, Response)
    ;   A.space_type == disabled_space,
        \+ _{disabled_permit:_} :< A
    ->  question_dict(disabled_permit, Response)
    ;   A.space_type == disabled_space,
        A.disabled_permit \== true
    ->  violation_response(A.goal, disabled_space_without_permit,
                           "Disabled space without a disabled permit is not allowed.",
                           Response)
    ;   continue_student_curb_and_distances(A, Response)
    ).

continue_student_curb_and_distances(A, Response) :-
    (   \+ _{curb_color:_} :< A
    ->  question_dict(curb_color, Response)
    ;   % try curb-color based violations first using your big predicate
        curb_violation_or_continue(A, Response)
    ).

curb_violation_or_continue(A, Response) :-
    Lot     = A.lot,
    Hour    = A.hour,
    Permit  = A.student_permit,
    Space   = A.space_type,
    Curb    = A.curb_color,

    (   student_parking_violation(Lot, Hour,
            Permit, false,
            Space, _,_,_,
            Curb, false,
            none, false,
            Reason)
    ->  violation_response(A.goal, Reason,
            "This curb color makes parking here a violation.",
            Response)
    ;   Space == regular
    ->  % regular space: ask combined proximity question
        near_object_or_finish(A, Response)
    ;   % disabled space with permit and no curb issue
        ok_response(A.goal,
                    "Your parking appears allowed with your disabled permit.",
                    Response)
    ).


near_object_or_finish(A, Response) :-
    (   \+ _{near_object:_} :< A
    ->  % question
        question_dict(near_object, Response)
    ;   % We have an answer, inspect it
        (   A.near_object == none
        ->  % nothing within 15 ft → parking OK
            ok_response(A.goal,
                        "Your parking appears to be allowed.",
                        Response)
        ;   violation_from_near_object(A, Response)
        )
    ).

violation_from_near_object(A, Response) :-
    (   A.near_object == fire_hydrant
    ->  violation_response(A.goal, too_close_to_fire_hydrant,
        "You are parked within 15 ft of a fire hydrant, which is not allowed.",
        Response)
    ;   A.near_object == building_entrance
    ->  violation_response(A.goal, too_close_to_building,
        "You are parked within 15 ft of a building entrance, which is not allowed.",
        Response)
    ;   A.near_object == disabled_access_ramp
    ->  violation_response(A.goal, too_close_to_ramp,
        "You are parked within 15 ft of a disabled access ramp, which is not allowed.",
        Response)
    ).


%  Faculty flow 

faculty_flow(A, Response) :-
    (   \+ _{faculty_permit:_} :< A
    ->  question_dict(faculty_permit, Response)
    ;   \+ _{lot:_} :< A
    ->  question_dict(lot, Response)
    ;   \+ _{hour:_} :< A
    ->  question_dict(hour, Response)
    ;   early_faculty_decision(A, Response)
    ).

early_faculty_decision(A, Response) :-
    HasPermit = A.faculty_permit,
    Lot       = A.lot,
    Hour      = A.hour,

    (   HasPermit \== true
    ->  violation_response(A.goal, no_faculty_permit,
                           "You do not have a valid faculty/staff permit.",
                           Response)
    ;   \+ may_park(Lot, HasPermit)
    ->  violation_response(A.goal, wrong_lot_for_faculty,
                           "Your permit is not valid for this lot.",
                           Response)
    ;   overnight_hour(Hour),
        overnight_violation(Lot, time(_,Hour), HasPermit, 0, Reason)
    ->  violation_response(A.goal, Reason,
                           "This would violate overnight faculty parking rules.",
                           Response)
    ;   ok_response(A.goal,
                    "Your parking appears to be allowed.",
                    Response)
    ).

%  Helper constructors for responses 

ok_response(Goal, Msg, Dict) :-
    format_result(ok, Goal, Msg, Dict).

violation_response(Goal, Code, Msg, Dict) :-
    format_result(violation, Goal, Msg, DictWithMsg),
    Dict = DictWithMsg.put(code, Code).


format_result(ok, Goal, Message, _{
    type: result,
    status: ok,
    label: Label,
    message: Message
}) :-
    (   Goal == check_parking_spot
    ->  Label = "Eligible for parking"
    ;   Goal == check_parked_car
    ->  Label = "Valid parking"
    ).

format_result(violation, Goal, Message, _{
    type: result,
    status: violation,
    label: Label,
    message: Message
}) :-
    (   Goal == check_parking_spot
    ->  Label = "Not eligible for parking"
    ;   Goal == check_parked_car
    ->  Label = "Parking violation"
    ).
