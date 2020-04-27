% 23 4 dup >r + r>
: double dup + ;

% double double double


%this version could be tail call optimized.
: factorial 1 swap factorial2 ;
: factorial2 dup
if dup >r * r> 1 - factorial2
else drop
then ;

10 factorial

%this version is written so tail-call optimization is clearly not happening.
: fact 1 swap fact2 ;
: fact2 dup
if dup >r 1 - fact2 r> *
else drop then ;


10 fact

: loop 1 drop loop ;
% loop