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
    Belt.Array.reduce(cards, suitMap(), (cardsBySuit, cardList) =>
      Belt.Map.merge(groupCardList(cardList), cardsBySuit, mergeCardsBySuit)
    );

  let freeCell = Command.dealCascades(());

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

let testDealtCascadesAreCorrectStructure = () => {
  let cards = Command.dealCascades(emptyFreeCell).cards;

  let firstFourCascades =
    Belt.List.fromArray(cards)->Belt.List.take(4) |> Belt.Option.getExn;
  let lastFourCascades =
    Belt.List.fromArray(cards)->Belt.List.drop(4) |> Belt.Option.getExn;

  let firstFourCascadeLengths =
    Belt.List.map(firstFourCascades, Belt.List.length);
  let lastFourCascadeLengths =
    Belt.List.map(lastFourCascades, Belt.List.length);

  let listPrinter = l =>
    "[" ++ (Belt.List.map(l, string_of_int) |> String.concat(", ")) ++ "]";

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
  ];
};

let legalMoves = () => {
  let eightOfHearts = {suit: Hearts, rank: 8};
  let nineOfClubs = {suit: Clubs, rank: 9};
  let cascadeOne = [eightOfHearts];
  let cascadeTwo = [nineOfClubs];
  let freeCell = {cards: [|cascadeOne, cascadeTwo|]};

  let freeCell =
    Command.moveCardBetweenCascades(
      ~sourceIndex=0,
      ~destinationIndex=1,
      freeCell,
    );

  let listPrinter = l =>
    "[" ++ (Belt.List.map(l, string_of_card) |> String.concat(", ")) ++ "]";

  [
    assertEqual(
      ~expected=[],
      ~actual=freeCell.cards[0],
      ~printer=listPrinter,
      "The card is removed from the source cascade",
    ),
    assertEqual(
      ~expected=[eightOfHearts, nineOfClubs],
      ~actual=freeCell.cards[1],
      ~printer=listPrinter,
      "The card is moved to the destination cascade",
    ),
  ];
};

let testMovingCardsBetweenCascades = () => {
  let illegalMovesComb = () => {
    let aceOfHearts = {suit: Hearts, rank: 1};
    let threeOfHearts = {suit: Hearts, rank: 3};
    let twoOfClubs = {suit: Clubs, rank: 2};
    let fourOfClubs = {suit: Clubs, rank: 4};
    let jackOfSpades = {suit: Spades, rank: 13};

    let redCards = [aceOfHearts, threeOfHearts];
    let blackCards = [twoOfClubs, fourOfClubs, jackOfSpades];

    let allCards = Belt.List.concat(redCards, blackCards);
    let allPairs = Combinatorics.permutations(allCards, 2);

    let testPair = p => {
      let [src, dst] = p;

      let sourceCascade = [src];
      let destCascade = [dst];

      let {cards} =
        Command.moveCardBetweenCascades(
          ~sourceIndex=0,
          ~destinationIndex=1,
          {cards: [|sourceCascade, destCascade|]},
        );

      let sourceCascade = cards[0];
      let destCascade = cards[1];

      let areCardsOppositeColors = cardColor(src) != cardColor(dst);
      let areRanksInOrder = dst.rank == src.rank + 1;
      let legalMove = areCardsOppositeColors && areRanksInOrder;

      let check =
        if (legalMove) {
          sourceCascade == [] && destCascade == [src, dst];
        } else {
          sourceCascade == [src] && destCascade == [dst];
        };

      assertEqual(
        ~expected=true,
        ~actual=check,
        ~printer=
          _ =>
            "src: "
            ++ string_of_card_list(sourceCascade)
            ++ " | dst: "
            ++ string_of_card_list(destCascade),
        "Only legal moves are performed",
      );
    };

    Belt.List.map(allPairs, testPair);
  };

  Belt.List.concat(legalMoves(), illegalMovesComb());
};

// cascades
// list(list(card));
// vals:
// card = 52
// list = 0..52, 2^52
// list(list(card)) = 2^52 possibilities

let testCascadeDisplayPreparation = () => {
  let aceOfHearts = {suit: Hearts, rank: 1};
  let twoOfHearts = {suit: Hearts, rank: 2};
  let threeOfHearts = {suit: Hearts, rank: 3};
  let fourOfHearts = {suit: Hearts, rank: 4};
  let fiveOfHearts = {suit: Hearts, rank: 5};

  let cascades = [|
    [aceOfHearts, twoOfHearts],
    [threeOfHearts, fourOfHearts],
    [fiveOfHearts],
  |];

  let displayCascades = Accidental.cascadesForDisplay(cascades);

  [
    assertEqual(
      ~expected=[|
        [Some(aceOfHearts), Some(threeOfHearts), Some(fiveOfHearts)],
        [Some(twoOfHearts), Some(fourOfHearts), None],
      |],
      ~actual=displayCascades,
      "The card matrix is transposed and None values are inserted to pad lists until they are all of the same length for display purposes",
    ),
  ];
};

let gameSetupCases = [testDealCascades, testDealtCascadesAreCorrectStructure];

let gameMoveCases = [testMovingCardsBetweenCascades];

let suite =
  Belt.List.flatten([
    gameSetupCases,
    gameMoveCases,
    [testCascadeDisplayPreparation],
  ]);

runSuite(suite);
