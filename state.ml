type action = Configure | Run_Algoirthm | Sell_All | Refresh_and_Show | Graph

type t = {
  mutable test : string;
  mutable config : Config.t;
  mutable portfolio : Portfolio.t;
}

exception InvalidAction of string

exception QuitAction

let init =
  {
    test = "";
    config = Config.default ();
    portfolio = Portfolio.empty_portfolio;
  }

let action_of_string s = raise (InvalidAction "s")

let update state action = state.test <- "updated"

let config state = state.config

let portfolio state = state.portfolio

let load_state_from_user (user : User.t) =
  {
    test = "";
    config = User.config user;
    portfolio = User.current_portfolio user;
  }
