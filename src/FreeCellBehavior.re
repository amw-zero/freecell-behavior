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

type color =
  | Red
  | Black;

let cardColor = card =>
  switch (card.suit) {
  | Diamonds
  | Hearts => Red
  | Clubs
  | Spades => Black
  };

let string_of_suit = suit =>
  switch (suit) {
  | Clubs => "c"
  | Diamonds => "d"
  | Hearts => "h"
  | Spades => "s"
  };

let string_of_card = card =>
  switch (card) {
  | Some(c) => string_of_int(c.rank) ++ string_of_suit(c.suit)
  | None => "empty"
  };

type cardList = list(option(card));
type cardMatrix = array(cardList);

type freeCell = {cards: cardMatrix};

let emptyFreeCell = {cards: [|[]|]};

module Command = {
  type cascadeBuilder = {
    cascades: list(list(option(card))),
    taken: int,
  };

  let dealCascades = freeCell => {
    let generateCards = () => {
      let allPairs = (e, l2) => Belt.List.map(l2, le => (e, le));
      let generateCombinations = (s1, s2) =>
        Belt.List.reduce(s1, [], (a, e) =>
          List.concat([a, allPairs(e, s2)])
        );

      generateCombinations(allSuits, allRanks)
      ->Belt.List.map(c => Some({suit: fst(c), rank: snd(c)}));
    };

    let cascadesFrom = cards => {
      let cascadeLengths = [7, 7, 7, 7, 6, 6, 6, 6];

      let nextCascade = (cards, drop, take) =>
        Belt.List.drop(cards, drop)
        ->Belt.Option.getExn
        ->Belt.List.take(take)
        ->Belt.Option.getExn;
      let cardsToCascade = (cascadeBuilder, length) => {
        cascades:
          Belt.List.add(
            cascadeBuilder.cascades,
            nextCascade(cards, cascadeBuilder.taken, length),
          ),
        taken: cascadeBuilder.taken + length,
      };

      Belt.List.reduce(
        cascadeLengths,
        {cascades: [], taken: 0},
        cardsToCascade,
      ).
        cascades
      |> Belt.List.reverse
      |> Belt.List.toArray;
    };

    let cards = generateCards();
    let cascades = cascadesFrom(cards);

    {cards: cascades};
  };

  type move =
    | Valid(card)
    | Invalid;

  let moveCardBetweenCascades = (~sourceIndex, ~destinationIndex, freeCell) => {
    let validateMove = (~src, ~dest) =>
      switch (src, dest) {
      | (Some(s), Some(d))
          when cardColor(s) != cardColor(d) && d.rank == s.rank + 1 =>
        Valid(s)
      | (Some(s), None) => Valid(s)
      | _ => Invalid
      };

    let bottomCard = (~atIndex as index) =>
      Belt.List.reverse(freeCell.cards[index])
      ->Belt.List.head
      ->Belt.Option.getExn;

    let sourceCard = bottomCard(~atIndex=sourceIndex);
    let destCard = bottomCard(~atIndex=destinationIndex);


    switch (validateMove(~src=sourceCard, ~dest=destCard)) {
    | Valid(s) =>
      let sourceCascade = freeCell.cards[sourceIndex];
      let destinationCascade = freeCell.cards[destinationIndex];

      let newDest = Belt.List.add(destinationCascade, Some(s));
      let newSource = Belt.List.drop(sourceCascade, 1) |> Belt.Option.getExn;

      freeCell.cards[sourceIndex] = newSource;
      freeCell.cards[destinationIndex] = newDest;
    | Invalid => ()
    };

    freeCell;
  };
};
