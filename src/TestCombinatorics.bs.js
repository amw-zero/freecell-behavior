// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var TestLib = require("./TestLib.bs.js");
var Belt_List = require("bs-platform/lib/js/belt_List.js");
var Formatting = require("./Formatting.bs.js");
var Combinatorics = require("./Combinatorics.bs.js");

function testPermutations(param) {
  var perms = Combinatorics.permutations(/* :: */[
        "A",
        /* :: */[
          "B",
          /* [] */0
        ]
      ], 3);
  return /* :: */[
          TestLib.assertEqual(undefined, (function (ll) {
                  return Formatting.string_of_list(Belt_List.map(ll, Formatting.string_of_list));
                }), /* :: */[
                /* :: */[
                  "A",
                  /* :: */[
                    "A",
                    /* :: */[
                      "A",
                      /* [] */0
                    ]
                  ]
                ],
                /* :: */[
                  /* :: */[
                    "B",
                    /* :: */[
                      "A",
                      /* :: */[
                        "A",
                        /* [] */0
                      ]
                    ]
                  ],
                  /* :: */[
                    /* :: */[
                      "A",
                      /* :: */[
                        "B",
                        /* :: */[
                          "A",
                          /* [] */0
                        ]
                      ]
                    ],
                    /* :: */[
                      /* :: */[
                        "B",
                        /* :: */[
                          "B",
                          /* :: */[
                            "A",
                            /* [] */0
                          ]
                        ]
                      ],
                      /* :: */[
                        /* :: */[
                          "A",
                          /* :: */[
                            "A",
                            /* :: */[
                              "B",
                              /* [] */0
                            ]
                          ]
                        ],
                        /* :: */[
                          /* :: */[
                            "B",
                            /* :: */[
                              "A",
                              /* :: */[
                                "B",
                                /* [] */0
                              ]
                            ]
                          ],
                          /* :: */[
                            /* :: */[
                              "A",
                              /* :: */[
                                "B",
                                /* :: */[
                                  "B",
                                  /* [] */0
                                ]
                              ]
                            ],
                            /* :: */[
                              /* :: */[
                                "B",
                                /* :: */[
                                  "B",
                                  /* :: */[
                                    "B",
                                    /* [] */0
                                  ]
                                ]
                              ],
                              /* [] */0
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ], perms, "Cards of the same suit cannot be moved between cascades"),
          /* [] */0
        ];
}

TestLib.runSuite(/* :: */[
      testPermutations,
      /* [] */0
    ]);

exports.testPermutations = testPermutations;
/*  Not a pure module */