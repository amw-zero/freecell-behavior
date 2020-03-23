open TestLib;
open FreeCellBehavior;

let testDealCascades = () => {
  let cardListSuitReducer = (cardsBySuit, card) =>
    Belt.Map.update(cardsBySuit, card.suit, oCards =>
      Belt.Option.map(oCards, cards => [card, ...cards])
    );

  let suitMap = () => {
    let map = Belt.Map.make(~id=(module SuitComparable));
    Belt.List.reduce(allSuits, map, (m, s) => Belt.Map.set(m, s, []));
  };

  let groupCardList = cards =>
    Belt.List.reduce(cards, suitMap(), cardListSuitReducer);

  let mergeCardsBySuit = (_, c1, c2) =>
    switch (c1, c2) {
    | (Some(cards1), Some(cards2)) => Some(List.concat([cards1, cards2]))
    | _ => None
    };

  let groupCardsBySuit = cards =>
    Belt.List.reduce(cards, suitMap(), (cardsBySuit, cardList) =>
      Belt.Map.merge(
        groupCardList(
          List.filter(c => Belt.Option.isSome(c), cardList)
          |> List.map(Belt.Option.getExn),
        ),
        cardsBySuit,
        mergeCardsBySuit,
      )
    );

  let freeCell = Command.dealCascades(emptyFreeCell);

  let cardsBySuit = groupCardsBySuit(freeCell.cards);
  let cardsPerSuit =
    Belt.Map.valuesToArray(cardsBySuit) |> Belt.List.fromArray;
  let allCards = cardsPerSuit |> Belt.List.flatten;

  [
    Int.assertEqual(
      ~expected=52,
      ~actual=List.length(allCards),
      "52 cards are dealt",
    ),    
    Bool.assertEqual(
      ~expected=true,
      ~actual=
        Belt.List.every(cardsPerSuit, cards => Belt.List.length(cards) == 13),
      "Each suit has 13 cards",
    ),
  ];
};

let testCascadesAreCorrectStructure = () => {
  let cards = Command.dealCascades(emptyFreeCell).cards;

  let firstFourCascades = Belt.List.take(cards, 4) |> Belt.Option.getExn;
  let lastFourCascades = Belt.List.drop(cards, 4) |> Belt.Option.getExn;

  let firstFourCascadeLengths = Belt.List.map(firstFourCascades, Belt.List.length);
  let lastFourCascadeLengths = Belt.List.map(lastFourCascades, Belt.List.length);

  let listPrinter = l => "[" ++ (Belt.List.map(l, string_of_int) |> String.concat(", ")) ++ "]";

  [
    assertEqual(
      ~expected=[7, 7, 7, 7],
      ~actual=firstFourCascadeLengths,
      ~printer=listPrinter,
      "The first 4 cascades each have 7 cards",
    ),
    assertEqual(
      ~expected=[6, 6, 6, 6],
      ~actual=lastFourCascadeLengths,
      ~printer=listPrinter,
      "The last 4 cascades each have 6 cards",
    ),
  ]
};

let testMovingCardsBetweenCascades = () => {
  [
    Int.assertEqual(
      ~expected=1,
      ~actual=2,
      "Illegal moves should be prevented",
    ),
  ]
};

let suite = [testDealCascades, testCascadesAreCorrectStructure];

runSuite(suite);
