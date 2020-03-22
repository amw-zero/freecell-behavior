type assertion = {
  result: bool,
  description: string,
};

let assertEqual = (~cmp=(==), ~expected, ~actual, description) => {
  result: cmp(expected, actual),
  description,
};

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

/*
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

let _runSuite = suite =>
  List.map(t => t(), suite)
  |> List.flatten
  |> List.filter(a => a.result != true);

let runSuite = suite => {
  let failingTests = _runSuite(suite);
  let failingDescriptions = List.map(t => t.description, failingTests);

 Js.log("Failing tests:");
 Belt.List.forEach(failingDescriptions, Js.log);
};
