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
  string_of_int(card.rank) ++ string_of_suit(card.suit);

let string_of_card_list = cards =>
  Belt.List.map(cards, c => string_of_card(c))
  |> String.concat(", ");

let string_of_optional_card_list = cards =>
  Belt.List.map(
    cards, 
    c => 
      switch(c) {
      | Some(c) => string_of_card(c)
      | None => "empty"
      }
  )
  |> String.concat(", ");

type cardList = list(card);
type cardMatrix = array(cardList);

type freeCell = {cards: cardMatrix};

let emptyFreeCell = {cards: [|[]|]};

module Command = {
  type cascadeBuilder = {
    cascades: list(list(card)),
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
      ->Belt.List.map(c => {suit: fst(c), rank: snd(c)});
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
      Belt.List.reverse(freeCell.cards[index])->Belt.List.head;

    let sourceCard = bottomCard(~atIndex=sourceIndex);
    let destCard = bottomCard(~atIndex=destinationIndex);

    switch (validateMove(~src=sourceCard, ~dest=destCard)) {
    | Valid(s) =>
      let sourceCascade = freeCell.cards[sourceIndex];
      let destinationCascade = freeCell.cards[destinationIndex];

      let newDest = Belt.List.add(destinationCascade, s);
      let newSource = Belt.List.drop(sourceCascade, 1) |> Belt.Option.getExn;

      freeCell.cards[sourceIndex] = newSource;
      freeCell.cards[destinationIndex] = newDest;
    | Invalid => ()
    };

    freeCell;
  };
};

module Accidental = {
  let cascadesForDisplay = cascades => {
    let normalize =
      cascades => {
        let cascades = Belt.List.fromArray(cascades);
        let maxLength =
          Belt.List.reduce(
            cascades,
            0,
            (a, cascade) => {
              let length = Belt.List.length(cascade);
              length > a ? length : a;
            },
          );

        Belt.List.map(
          cascades,
          cascade => {
            let lengthDiff = maxLength - Belt.List.length(cascade);
            if (lengthDiff == 0) {
              Belt.List.map(cascade, card => Some(card));
            } else {
              Belt.List.map(cascade, card => Some(card))
              @ Belt.List.makeBy(lengthDiff, _ => None);
            };
          },
        );
      };

    let rec transpose =
      ll => {
        switch (ll) {
        | [[], ..._] => []
        | lists =>
          Belt.List.[lists->map(headExn), ...lists->map(tailExn)->transpose]
        };
      };

    normalize(cascades)->transpose->Belt.List.toArray;
  };
};
