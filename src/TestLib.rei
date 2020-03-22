type assertion;

let assertEqual: (~cmp: ('a, 'a) => bool=?, ~expected: 'a, ~actual: 'a, string) => assertion;
let runSuite: list(unit => list(assertion)) => unit;
