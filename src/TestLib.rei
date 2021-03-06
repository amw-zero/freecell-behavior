type assertion;

module Int {
  let assertEqual: (~expected: int, ~actual: int, string) => assertion;
};

module Bool {
  let assertEqual: (~expected: bool, ~actual: bool, string) => assertion;
};

let assertEqual: (~cmp: ('a, 'a) => bool=?, ~printer: ('a) => string=?, ~expected: 'a, ~actual: 'a, string) => assertion;
let runSuite: list(unit => list(assertion)) => unit;
