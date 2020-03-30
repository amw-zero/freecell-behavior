// n >= 1
// let s = [e1, e2, ... en]
// permutations(s, 1) = [[e1], [e2], ... [en]]
// permutations(s, 2) =  [[e1, e1], [e1, e2], ... [e1, en]]
// permutations(s, 2) = for each e in s, append e to each p in permutations(s, n - 1)
let rec permutations = (s, n) =>
  switch (n) {
  | 1 => Belt.List.map(s, e => [e])
  | _ =>
    Belt.List.reduce(s, [], (a, e) =>
      a @ Belt.List.map(permutations(s, n - 1), p => p @ [e])
    )
  };

let otherElements = (set, subset) =>
  Belt.List.keep(set, e => !Belt.List.has(subset, e, (==)));

let rec combinedWith = (accum, others, set, n) => {
  Js.log("\n\nframe");
  Js.log("accum");
  Js.log(accum);

  Js.log("others");
  Js.log(others);
  if (Belt.List.length(accum) == n) {
    accum;
  } else if (Belt.List.length(others) == 1) {
    Js.log("Recursion ended");
    let res = Belt.List.concat(others, accum);
    Js.log(Belt.List.length(res));
    res;
  } else {
    Belt.List.reduce(
      others,
      [],
      (a, o) => {
        let combined = [o, ...accum];
        Belt.List.concat(
          a,
          combined->combinedWith(otherElements(set, combined), set, n),
        );
      },
    );
  };
};

// let permutations = (s, n) =>
//   Belt.List.reduce(
//     s,
//     [],
//     (p, e) => {
//       let combined = [e]->combinedWith(otherElements(s, [e]), s, n);
//       Js.log("Combined");
//       Js.log(combined);
//       Belt.List.add(p, [e]->combinedWith(otherElements(s, [e]), s, n));
//     },
//   );
