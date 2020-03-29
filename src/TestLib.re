type assertion = {
  result: bool,
  description: string,
  feedback: option(string),
};

let feedbackFor = (e, a, p) =>
  Some("Expected: " ++ p(e) ++ "\nActual: " ++ p(a) ++ "\n");

let assertEqual = (~cmp=(==), ~printer=?, ~expected, ~actual, description) => {
  let result = cmp(expected, actual);
  let feedback =
    switch (result, printer) {
    | (false, Some(p)) => feedbackFor(expected, actual, p)
    | _ => None
    };

  {result, description, feedback};
};

module Int = {
  let assertEqual = (~expected, ~actual, description) =>
    assertEqual(~expected, ~actual, ~printer=string_of_int, description);
};

module Bool = {
  let assertEqual = (~expected, ~actual, description) =>
    assertEqual(~expected, ~actual, ~printer=string_of_bool, description);
};

/*

 // Tests for TestLib
 assertEqual(~expected=1, ~actual=2);

 type myBool =
   | True
   | False;

 let myBoolCmp = (ba, bb) =>
   switch (ba, bb) {
   | (True, True) => true
   | (False, False) => true
   | _ => false
   };


 let test = _ => [
   assertEqual(~expected=5, ~actual=4, "5 should equal 4 duh"),
 ];

 let multiTest = _ => [
   assertEqual(~expected=5, ~actual=6, "Test"),
   assertEqual(~expected=6, ~actual=7, "Description"),
 ];

  let suite = [
    test,
    multiTest,
    () => [assertEqual(~cmp=myBoolCmp, ~expected=True, ~actual=False, "Compare bools")]
  ]
  */

type suiteDiagnostic = {
  failingTests: list(assertion),
  assertionCount: int,
};

let printAssertion = a => {
  Js.log(a.description);
  switch (a.feedback) {
  | Some(f) => Js.log(f)
  | None => ()
  };
};

let _runSuite = suite => {
  let assertions = Belt.List.map(suite, t => t())->Belt.List.flatten;

  {
    assertionCount: Belt.List.length(assertions),
    failingTests: assertions->Belt.List.keep(a => a.result != true),
  };
};

let runSuite = suite => {
  let {failingTests, assertionCount} = _runSuite(suite);
  switch (List.length(failingTests)) {
  | 0 => Js.log("All tests passed.")
  | _ =>
    Js.log("Failing tests:\n");
    Belt.List.forEach(failingTests, printAssertion);
  };

  Js.log("Ran " ++ string_of_int(assertionCount) ++ " assertions.");
};
