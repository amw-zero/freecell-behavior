// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$String = require("bs-platform/lib/js/string.js");
var TestLib = require("./TestLib.bs.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var FreeCellBehavior = require("./FreeCellBehavior.bs.js");

function testDealCascades(param) {
  var cardListSuitReducer = function (cardsBySuit, card) {
    return Belt_Map.update(cardsBySuit, card.suit, (function (oCards) {
                  return Belt_Option.map(oCards, (function (cards) {
                                return /* :: */[
                                        card,
                                        cards
                                      ];
                              }));
                }));
  };
  var suitMap = function (param) {
    var map = Belt_Map.make(FreeCellBehavior.SuitComparable);
    return Belt_List.reduce(FreeCellBehavior.allSuits, map, (function (m, s) {
                  return Belt_Map.set(m, s, /* [] */0);
                }));
  };
  var groupCardList = function (cards) {
    return Belt_List.reduce(cards, suitMap(/* () */0), cardListSuitReducer);
  };
  var mergeCardsBySuit = function (param, c1, c2) {
    if (c1 !== undefined && c2 !== undefined) {
      return List.concat(/* :: */[
                  c1,
                  /* :: */[
                    c2,
                    /* [] */0
                  ]
                ]);
    }
    
  };
  var groupCardsBySuit = function (cards) {
    return Belt_Array.reduce(cards, suitMap(/* () */0), (function (cardsBySuit, cardList) {
                  return Belt_Map.merge(groupCardList(List.map(Belt_Option.getExn, List.filter(Belt_Option.isSome)(cardList))), cardsBySuit, mergeCardsBySuit);
                }));
  };
  var freeCell = FreeCellBehavior.Command.dealCascades(FreeCellBehavior.emptyFreeCell);
  var cardsBySuit = groupCardsBySuit(freeCell.cards);
  var cardsPerSuit = Belt_List.fromArray(Belt_Map.valuesToArray(cardsBySuit));
  var allCards = Belt_List.flatten(cardsPerSuit);
  return /* :: */[
          TestLib.Int.assertEqual(52, List.length(allCards), "52 cards are dealt"),
          /* :: */[
            TestLib.Bool.assertEqual(true, Belt_List.every(cardsPerSuit, (function (cards) {
                        return Belt_List.length(cards) === 13;
                      })), "Each suit has 13 cards"),
            /* [] */0
          ]
        ];
}

function testDealtCascadesAreCorrectStructure(param) {
  var cards = FreeCellBehavior.Command.dealCascades(FreeCellBehavior.emptyFreeCell).cards;
  var firstFourCascades = Belt_Option.getExn(Belt_List.take(Belt_List.fromArray(cards), 4));
  var lastFourCascades = Belt_Option.getExn(Belt_List.drop(Belt_List.fromArray(cards), 4));
  var firstFourCascadeLengths = Belt_List.map(firstFourCascades, Belt_List.length);
  var lastFourCascadeLengths = Belt_List.map(lastFourCascades, Belt_List.length);
  var listPrinter = function (l) {
    return "[" + ($$String.concat(", ", Belt_List.map(l, (function (prim) {
                        return String(prim);
                      }))) + "]");
  };
  return /* :: */[
          TestLib.assertEqual(undefined, listPrinter, /* :: */[
                7,
                /* :: */[
                  7,
                  /* :: */[
                    7,
                    /* :: */[
                      7,
                      /* [] */0
                    ]
                  ]
                ]
              ], firstFourCascadeLengths, "The first 4 cascades each have 7 cards"),
          /* :: */[
            TestLib.assertEqual(undefined, listPrinter, /* :: */[
                  6,
                  /* :: */[
                    6,
                    /* :: */[
                      6,
                      /* :: */[
                        6,
                        /* [] */0
                      ]
                    ]
                  ]
                ], lastFourCascadeLengths, "The last 4 cascades each have 6 cards"),
            /* [] */0
          ]
        ];
}

function testMovingCardsBetweenCascades(param) {
  var eightOfHearts = {
    suit: /* Hearts */2,
    rank: 8
  };
  var nineOfClubs = {
    suit: /* Clubs */0,
    rank: 9
  };
  var cascadeOne = /* :: */[
    eightOfHearts,
    /* [] */0
  ];
  var cascadeTwo = /* :: */[
    nineOfClubs,
    /* [] */0
  ];
  var freeCell = {
    cards: /* array */[
      cascadeOne,
      cascadeTwo
    ]
  };
  var freeCell$1 = FreeCellBehavior.Command.moveCardBetweenCascades(0, 1, freeCell);
  var listPrinter = function (l) {
    return "[" + ($$String.concat(", ", Belt_List.map(l, FreeCellBehavior.string_of_card)) + "]");
  };
  return /* :: */[
          TestLib.assertEqual(undefined, listPrinter, /* [] */0, Caml_array.caml_array_get(freeCell$1.cards, 0), "The card is removed from the source cascade"),
          /* :: */[
            TestLib.assertEqual(undefined, listPrinter, /* :: */[
                  eightOfHearts,
                  /* :: */[
                    nineOfClubs,
                    /* [] */0
                  ]
                ], Caml_array.caml_array_get(freeCell$1.cards, 1), "The card is moved to the destination cascade"),
            /* [] */0
          ]
        ];
}

var gameSetupCases_001 = /* :: */[
  testDealtCascadesAreCorrectStructure,
  /* [] */0
];

var gameSetupCases = /* :: */[
  testDealCascades,
  gameSetupCases_001
];

var gameMoveCases = /* :: */[
  testMovingCardsBetweenCascades,
  /* [] */0
];

var suite = Belt_List.concat(gameSetupCases, gameMoveCases);

TestLib.runSuite(suite);

exports.testDealCascades = testDealCascades;
exports.testDealtCascadesAreCorrectStructure = testDealtCascadesAreCorrectStructure;
exports.testMovingCardsBetweenCascades = testMovingCardsBetweenCascades;
exports.gameSetupCases = gameSetupCases;
exports.gameMoveCases = gameMoveCases;
exports.suite = suite;
/* suite Not a pure module */
