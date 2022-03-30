open Ast

let non_polymorphic_id_example =
  Lambda
    {
      id = "x";
      e1 = Lambda { id = "y"; e1 = Lambda { id = "z"; e1 = Var "z" } };
    }

let polymorphic_id_example =
  Let { id = "id"; e1 = Lambda { id = "x"; e1 = Var "x" }; e2 = Var "id" }

let nested_let_example =
  Let
    {
      id = "id";
      e1 = Lambda { id = "x"; e1 = Var "x" };
      e2 =
        Let
          {
            id = "both";
            e1 = Lambda { id = "y"; e1 = Lambda { id = "z"; e1 = Var "id" } };
            e2 =
              Let
                {
                  id = "amk";
                  e1 = Lambda { id = "x"; e1 = Var "both" };
                  e2 = Tuple { e1 = Var "amk"; e2 = Var "amk" };
                };
          };
    }

let fun_application_example =
  Lambda
    {
      id = "x";
      e1 = Lambda { id = "y"; e1 = App { e1 = Var "x"; e2 = Var "y" } };
    }

let fun_application_three_example =
  Lambda
    {
      id = "x";
      e1 =
        Lambda
          {
            id = "y";
            e1 =
              Lambda
                {
                  id = "z";
                  e1 =
                    App
                      { e1 = Var "z"; e2 = App { e1 = Var "x"; e2 = Var "y" } };
                };
          };
    }

let tuple_fun_application_example =
  Lambda
    {
      id = "x";
      e1 =
        Lambda
          {
            id = "y";
            e1 =
              Tuple
                {
                  e1 = App { e1 = Var "x"; e2 = Var "y" };
                  e2 = App { e1 = Var "x"; e2 = Var "y" };
                };
          };
    }

let lambda_outside_let_example =
  Lambda
    {
      id = "x";
      e1 =
        Lambda
          {
            id = "z";
            e1 =
              Let
                {
                  id = "y";
                  e1 =
                    Lambda
                      {
                        id = "w";
                        e1 =
                          Tuple
                            {
                              e1 = App { e1 = Var "w"; e2 = Var "x" };
                              e2 = App { e1 = Var "w"; e2 = Var "x" };
                            };
                      };
                  e2 = Var "y";
                };
          };
    }

let fst_lambda_example =
  Lambda { id = "x"; e1 = Fst (Tuple { e1 = Var "x"; e2 = Var "x" }) }

let fst_let_example =
  Lambda
    {
      id = "x";
      e1 =
        Lambda
          {
            id = "y";
            e1 =
              Let
                {
                  id = "z";
                  e1 = Tuple { e1 = Var "y"; e2 = Var "x" };
                  e2 = Fst (Var "z");
                };
          };
    }

let everything_example =
  Lambda
    {
      id = "x";
      e1 =
        Let
          {
            id = "y";
            e1 = App { e1 = Lambda { id = "w"; e1 = Var "w" }; e2 = Var "x" };
            e2 =
              Lambda
                {
                  id = "u";
                  e1 =
                    Lambda
                      {
                        id = "z";
                        e1 =
                          Tuple
                            {
                              e1 = App { e1 = Var "y"; e2 = Var "u" };
                              e2 = App { e1 = Var "y"; e2 = Var "z" };
                            };
                      };
                };
          };
    }

let everything_example2 =
  Lambda
    {
      id = "x";
      e1 =
        Let
          {
            id = "y";
            e1 = Lambda {id = "w"; e1 = App {e1 = Var "w"; e2 = Var "x"}};
            e2 =
              Lambda
                {
                  id = "u";
                  e1 =
                    Lambda
                      {
                        id = "z";
                        e1 =
                          Tuple
                            {
                              e1 = App { e1 = Var "y"; e2 = Var "u" };
                              e2 = App { e1 = Var "y"; e2 = Var "z" };
                            };
                      };
                };
          };
    }

let polymorphic_id_with_int_and_bool =
  Let
    {
      id = "id";
      e1 = Lambda { id = "x"; e1 = Var "x" };
      e2 =
        Let
          {
            id = "both";
            e1 =
              Tuple
                {
                  e1 = App { e1 = Var "id"; e2 = BasVal (Int 2) };
                  e2 = App { e1 = Var "id"; e2 = BasVal (Bool true) };
                };
            e2 = Var "both";
          };
    }


let many_nested_lets = 
  Let 
    {
      id = "id";
      e1 = Lambda {id = "x"; e1 = Var "x"};
      e2 = Let {
        id = "pair";
        e1 = Lambda {
          id = "p";
          e1 = Lambda {
            id = "x";
            e1 = Lambda {
              id = "y";
              e1 = App {
                e1 = App {
                  e1 = Var "p";
                  e2 = Var "x";
                };
                e2 = Var "y"
              }
            }
          }
        };
        e2 = Let {
          id = "p1";
          e1 = Lambda {
            id = "p";
            e1 = App {
              e1 = App {
                e1 = App {
                  e1 = Var "pair";
                  e2 = Var "id"
                };
                e2 = Var "id"
              };
              e2 = Var "p"
            }
          };
          e2 = Let {
            id = "p2";
            e1 = Lambda {
              id = "p";
              e1 = App {
                e1 = App {
                  e1 = App {
                    e1 = Var "pair";
                    e2 = Var "p1"
                  };
                  e2 = Var "p1"
                };
                e2 = Var "p"
              }
            };
            e2 = Let {
              id = "p3";
              e1 = Lambda {
                id = "p";
                e1 = App {
                  e1 = App {
                    e1 = App {
                      e1 = Var "pair";
                      e2 = Var "p2"
                    };
                    e2 = Var "p2"
                  };
                  e2 = Var "p"
                }
              };
              e2 = Let {
                id = "p4";
                e1 = Lambda {
                  id = "p";
                  e1 = App {
                    e1 = App {
                      e1 = App {
                        e1 = Var "pair";
                        e2 = Var "p3"
                      };
                      e2 = Var "p3"
                    };
                    e2 = Var "p"
                  }
                };
                e2 = Let {
                id = "p5";
                e1 = Lambda {
                  id = "p";
                  e1 = App {
                    e1 = App {
                      e1 = App {
                        e1 = Var "pair";
                        e2 = Var "p4"
                      };
                      e2 = Var "p4"
                    };
                    e2 = Var "p"
                  }
                };
                e2 = Tuple {
                  e1 = Tuple {
                    e1 = Tuple {
                      e1 = Tuple {
                        e1 = Tuple {
                          e1 = Tuple {
                            e1 = Var "p5";
                            e2 = Var "p4"
                          };
                          e2 = Var "p3"
                        };
                        e2 = Var "p2"
                      };
                      e2 = Var "p1"
                    };
                    e2 = Var "pair"
                  };
                  e2 = Var "id"
                }
              }
              }
            }
          }
        }
      }
    }

let many_lambdas = 
  Lambda {
    id = "x";
    e1 =   Lambda {
      id = "x";
      e1 =   Lambda {
        id = "x";
        e1 =   Lambda {
          id = "x";
          e1 =   Lambda {
            id = "x";
            e1 =   Lambda {
              id = "x";
              e1 = Lambda {
                id = "x";
                e1 =   Lambda {
                  id = "x";
                  e1 =   Lambda {
                    id = "x";
                    e1 =   Lambda {
                      id = "x";
                      e1 =   Lambda {
                        id = "x";
                        e1 =   Lambda {
                          id = "x";
                          e1 = Lambda {
                            id = "x";
                            e1 =   Lambda {
                              id = "x";
                              e1 =   Lambda {
                                id = "x";
                                e1 =   Lambda {
                                  id = "x";
                                  e1 =   Lambda {
                                    id = "x";
                                    e1 =   Lambda {
                                      id = "x";
                                      e1 = Lambda {
                                        id = "x";
                                        e1 =   Lambda {
                                          id = "x";
                                          e1 =   Lambda {
                                            id = "x";
                                            e1 =   Lambda {
                                              id = "x";
                                              e1 =   Lambda {
                                                id = "x";
                                                e1 =   Lambda {
                                                  id = "x";
                                                  e1 = Lambda {
                                                    id = "x";
                                                    e1 =   Lambda {
                                                      id = "x";
                                                      e1 =   Lambda {
                                                        id = "x";
                                                        e1 =   Lambda {
                                                          id = "x";
                                                          e1 =   Lambda {
                                                            id = "x";
                                                            e1 =   Lambda {
                                                              id = "x";
                                                              e1 = Lambda {
                                                                id = "x";
                                                                e1 =   Lambda {
                                                                  id = "x";
                                                                  e1 =   Lambda {
                                                                    id = "x";
                                                                    e1 =   Lambda {
                                                                      id = "x";
                                                                      e1 =   Lambda {
                                                                        id = "x";
                                                                        e1 =   Lambda {
                                                                          id = "x";
                                                                          e1 = Lambda {
                                                                            id = "x";
                                                                            e1 =   Lambda {
                                                                              id = "x";
                                                                              e1 =   Lambda {
                                                                                id = "x";
                                                                                e1 =   Lambda {
                                                                                  id = "x";
                                                                                  e1 =   Lambda {
                                                                                    id = "x";
                                                                                    e1 =   Lambda {
                                                                                      id = "x";
                                                                                      e1 = Lambda {
                                                                                        id = "x";
                                                                                        e1 =   Lambda {
                                                                                          id = "x";
                                                                                          e1 =   Lambda {
                                                                                            id = "x";
                                                                                            e1 =   Lambda {
                                                                                              id = "x";
                                                                                              e1 =   Lambda {
                                                                                                id = "x";
                                                                                                e1 =   Lambda {
                                                                                                  id = "x";
                                                                                                  e1 = Lambda {
                                                                                                    id = "x";
                                                                                                    e1 =   Lambda {
                                                                                                      id = "x";
                                                                                                      e1 =   Lambda {
                                                                                                        id = "x";
                                                                                                        e1 =   Lambda {
                                                                                                          id = "x";
                                                                                                          e1 =   Lambda {
                                                                                                            id = "x";
                                                                                                            e1 =   Lambda {
                                                                                                              id = "x";
                                                                                                              e1 = Lambda {
                                                                                                                id = "x";
                                                                                                                e1 =   Lambda {
                                                                                                                  id = "x";
                                                                                                                  e1 =   Lambda {
                                                                                                                    id = "x";
                                                                                                                    e1 =   Lambda {
                                                                                                                      id = "x";
                                                                                                                      e1 =   Lambda {
                                                                                                                        id = "x";
                                                                                                                        e1 =   Lambda {
                                                                                                                          id = "x";
                                                                                                                          e1 = Lambda {
                                                                                                                            id = "x";
                                                                                                                            e1 =   Lambda {
                                                                                                                              id = "x";
                                                                                                                              e1 =   Lambda {
                                                                                                                                id = "x";
                                                                                                                                e1 =   Lambda {
                                                                                                                                  id = "x";
                                                                                                                                  e1 =   Lambda {
                                                                                                                                    id = "x";
                                                                                                                                    e1 =   Lambda {
                                                                                                                                      id = "x";
                                                                                                                                      e1 = Lambda {
                                                                                                                                        id = "x";
                                                                                                                                        e1 =   Lambda {
                                                                                                                                          id = "x";
                                                                                                                                          e1 =   Lambda {
                                                                                                                                            id = "x";
                                                                                                                                            e1 =   Lambda {
                                                                                                                                              id = "x";
                                                                                                                                              e1 =   Lambda {
                                                                                                                                                id = "x";
                                                                                                                                                e1 =   Lambda {
                                                                                                                                                  id = "x";
                                                                                                                                                  e1 = Var "x"
                                                                                                                                                }
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                          }
                                                                                                                        }
                                                                                                                      }
                                                                                                                    }
                                                                                                                  }
                                                                                                                }
                                                                                                              }
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

let debug_example =
  Let {
        id = "pair";
        e1 = Lambda {
          id = "p";
          e1 = Lambda {
            id = "x";
            e1 = Lambda {
              id = "y";
              e1 = App {
                e1 = App {
                  e1 = Var "p";
                  e2 = Var "x";
                };
                e2 = Var "y"
              }
            }
          }
        };
        e2 = Var "pair"
  }

