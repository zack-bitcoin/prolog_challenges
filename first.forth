% 23 4 dup >r + r>
: double dup + ;

% double double double


: factorial 1 swap factorial2 ;
: factorial2 dup if dup >r * r> 1 - factorial2 else drop then ;

10 factorial