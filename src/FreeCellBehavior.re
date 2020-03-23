type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades;

module SuitComparable =
  Belt.Id.MakeComparable({
    type t = suit;
    let cmp = (e1, e2) => Pervasives.compare(e1, e2);
  });

let allSuits = [Clubs, Diamonds, Hearts, Spades];
let allRanks = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12];

type card = {
  suit,
  rank: int,
};

type cardList = list(option(card));
type cardMatrix = list(cardList);

type freeCell = {cards: cardMatrix};

let emptyFreeCell = {cards: [[]]};

module Command = {
  type cascadeBuilder = {
    cascades: cardMatrix,
    taken: int
  };

  let dealCascades = freeCell => {
    let allPairs = (e, l2) => Belt.List.map(l2, le => (e, le));

    let generateCombinations = (s1, s2) =>
      Belt.List.reduce(s1, [], (a, e) => List.concat([a, allPairs(e, s2)]));
    let cards =
      generateCombinations(allSuits, allRanks)
      ->Belt.List.map(c => Some({suit: fst(c), rank: snd(c)}));

    let cascadeLengths = [7, 7, 7, 7, 6, 6, 6, 6];
    let cascades = Belt.List.reduce(cascadeLengths, {cascades: [], taken: 0}, (cascadeBuilder, length) => {
      let cascade = Belt.List.drop(cards, cascadeBuilder.taken)
        ->Belt.Option.getExn
        ->Belt.List.take(length)
        ->Belt.Option.getExn;

      {cascades: Belt.List.add(cascadeBuilder.cascades, cascade), taken: cascadeBuilder.taken + length};
    }).cascades |> Belt.List.reverse;

    {cards: cascades};
  };
};
