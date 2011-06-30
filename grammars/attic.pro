
/*
about *-->
        phalanx(Ph1),
        {Ph1 =::= Loc1 and phalanx and D},
        [is,attached,to],
        phalanx(Ph2),
        {Ph2 =::= Loc2 and phalanx and D},
        {user:attaches(Loc1,Loc2,Num),rq(D < part_of some (anatomical_digit and has_order value Num))} ::
        Ph1 < attached_to some Ph2.
*/

% ATTACHMENT RULES:

/*
on_completion(
  add Ph1 < attached_to some Ph2 where
             (   
                 equivalent_to(Ph1,Phx1),class(Ph1),
                 Phx1 =::= Loc1 and phalanx and D,
                 equivalent_to(Ph2,Phx2),class(Ph2),
                 Phx2 =::= Loc2 and phalanx and D,

                 {D < anatomical_digit and has_order value Num}
             )
             ).
*/
