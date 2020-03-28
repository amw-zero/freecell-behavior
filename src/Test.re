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

let testMovingCardsBetweenCascades = () => {
  let legalMoves = () => {
    let eightOfHearts = Some({suit: Hearts, rank: 8});
    let nineOfClubs = Some({suit: Clubs, rank: 9});
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

  let illegalMovesComb = () => {
    let aceOfHearts = {suit: Hearts, rank: 1};
    let threeOfHearts = {suit: Hearts, rank: 3};
    let twoOfClubs = {suit: Clubs, rank: 2};
    let fourOfClubs = {suit: Clubs, rank: 4};
    let jackOfSpades = {suit: Spades, rank: 13};

    let redCards = [
      aceOfHearts,
      threeOfHearts,
    ];
    let blackCards = [
      twoOfClubs,
      fourOfClubs,
      jackOfSpades,
    ];

    let allCards = Belt.List.concat(redCards, blackCards);
    let allPairs = Combinatorics.permutations(allCards, 2);

    // Belt.List.forEach(
    //   allPairs,
    //   cards => {
    //     Js.log(Formatting.string_of_card_list(cards));
    //     Js.log("-----\n");
    //   },
    // );

//    Js.log("Running " ++ (Belt.List.length(allPairs) |> string_of_int) ++ " cases");

    Belt.List.map(
      allPairs,
      p => {
        let [src, dst] = p;

        let sourceCascade = [Some(src)];
        let destCascade = [Some(dst)];

        let {cards} = Command.moveCardBetweenCascades(
          ~sourceIndex=0, 
          ~destinationIndex=1,
          {cards:[|sourceCascade, destCascade|]}
        );

        let sourceCascade = cards[0];
        let destCascade = cards[1];

        let areCardsOppositeColors = cardColor(src) != cardColor(dst);
        let areRanksInOrder = dst.rank == src.rank + 1;
        let legalMove = areCardsOppositeColors && areRanksInOrder;

        let check = 
          if (legalMove) {
            sourceCascade == [None] && destCascade == [Some(dst), Some(src)];
          } else {
            sourceCascade == [Some(src)] && destCascade == [Some(dst)];
          };

        assertEqual(
          ~expected=true,
          ~actual=check,
          ~printer=b => "src: " ++ Formatting.string_of_optional_card_list(sourceCascade)
            ++ " | dst: " ++ Formatting.string_of_optional_card_list(destCascade),
          "Illegal moves are not permitted"
        );
      }
    );

    // [
    //   Int.assertEqual(
    //     ~expected=1,
    //     ~actual=2,
    //     "Cards of the same suit cannot be moved between cascades",
    //   ),
    // ];
  };

  Belt.List.concat(legalMoves(), illegalMovesComb());
};

let gameSetupCases = [testDealCascades, testDealtCascadesAreCorrectStructure];

let gameMoveCases = [testMovingCardsBetweenCascades];

let suite = Belt.List.concat(gameSetupCases, gameMoveCases);

runSuite(suite);
