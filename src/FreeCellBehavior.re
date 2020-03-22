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

type card = {
  suit,
  rank: int,
};

type cardList = list(option(card));
type cardMatrix = list(cardList);

type freeCell = {cards: cardMatrix};

let emptyFreeCell = {cards: [[]]};

module Command = {
  let dealCascades = freeCell => {
    cards: [[Some({suit: Hearts, rank: 0})]],
  };
};
