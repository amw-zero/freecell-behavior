open TestLib;

let testPermutations = () => {
  let letters = ["A", "B"];

  let perms = Combinatorics.permutations(letters, 3);

  // Js.log("[A, B, C], 8");
  // Js.log("Count " ++ string_of_int(Belt.List.length(perms)));
  // Belt.List.forEach(perms, p => Js.log(Formatting.string_of_list(p)));

  // Js.log("\n[A, B], 2");
  // Belt.List.forEach(Combinatorics.permutations(["A, B"], 2), p =>
  //   Js.log(Formatting.string_of_list(p))
  // );

  [
    assertEqual(
      ~expected=[
        ["A", "A", "A"],
        ["B", "A", "A"],
        ["A", "B", "A"],
        ["B", "B", "A"],
        ["A", "A", "B"],
        ["B", "A", "B"],
        ["A", "B", "B"],
        ["B", "B", "B"],
      ],
      ~actual=perms,
      ~printer=
        ll => {
          let l = Belt.List.map(ll, Formatting.string_of_list);
          Formatting.string_of_list(l);
        },
      "Cards of the same suit cannot be moved between cascades",
    ),
  ];
};

runSuite([testPermutations]);
