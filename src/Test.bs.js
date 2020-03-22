// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var TestLib = require("./TestLib.bs.js");
var Belt_Map = require("bs-platform/lib/js/belt_Map.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
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
    return Belt_List.reduce(cards, suitMap(/* () */0), (function (cardsBySuit, cardList) {
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

var suite = /* :: */[
  testDealCascades,
  /* [] */0
];

TestLib.runSuite(suite);

exports.testDealCascades = testDealCascades;
exports.suite = suite;
/*  Not a pure module */
