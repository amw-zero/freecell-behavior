open TestLib;
open FreeCellBehavior;

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

let testDealCascades = () => {
  let freeCell = Command.dealCascades(emptyFreeCell);

  let cardsBySuit = groupCardsBySuit(freeCell.cards);
  let cardsPerSuit =
    Belt.Map.valuesToArray(cardsBySuit) |> Belt.List.fromArray;
  let allCards = cardsPerSuit |> Belt.List.flatten;

  [
    assertEqual(
      ~expected=52,
      ~actual=List.length(allCards),
      "52 cards should be dealt",
    ),
    assertEqual(
      ~expected=true,
      ~actual=
        Belt.List.every(cardsPerSuit, cards => Belt.List.length(cards) == 13),
      "Each suit should have 13 cards",
    ),
  ];
};

let suite = [testDealCascades];

runSuite(suite);
 