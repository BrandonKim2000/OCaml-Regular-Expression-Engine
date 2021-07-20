open P3.Nfa
open P3.Regexp
open TestUtils
open OUnit2


let student_test_re_to_nfa ctxt =
  let m1 = regexp_to_nfa Empty_String in 
  assert_nfa_accept m1 "" ;
  assert_nfa_deny m1 "a" ;
  assert_nfa_deny m1 "b" ;
  assert_nfa_deny m1 "ab" ;
  assert_nfa_deny m1 "ba" ;
  let m2 = regexp_to_nfa (Concat (Char 'a', Char 'b')) in 
  assert_nfa_deny m2 "" ;
  assert_nfa_deny m2 "a" ;
  assert_nfa_deny m2 "b" ;
  assert_nfa_deny m2 "ba" ;
  assert_nfa_accept m2 "ab" ;
  let m3 = regexp_to_nfa (Star (Char 'a')) in 
  assert_nfa_accept m3 "";
  assert_nfa_accept m3 "a" ;
  assert_nfa_deny m3 "b" ;
  assert_nfa_deny m3 "ab" ;
  assert_nfa_deny m3 "ba"

let suite =
  "student"
  >::: [ "student_re_to_nfa" >:: student_test_re_to_nfa]

let _ = run_test_tt_main suite
