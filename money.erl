-module(money).
-import(customer, [spawn_customers/2]).
-import(bank, [spawn_banks/2]).
-export([start/1]).

%Master Process
start(Args) ->
  CustomerFile = lists:nth(1, Args),
  BankFile = lists:nth(2, Args),
  {ok, CustomerInfo} = file:consult(CustomerFile),
  {ok, BankInfo} = file:consult(BankFile),
  spawn_master(CustomerInfo, BankInfo).

spawn_master(CustomerInfo, BankInfo) ->
  io:format("~n~n** The financial market is opening for the day **~n~n"),

  spawn_customers(CustomerInfo, BankInfo),
  spawn_banks(BankInfo, CustomerInfo),
  
  io:format("Starting transaction log...~n~n"),
  timer:sleep(500),
  
  CustomerResults = [],
  BankResults = [{BankName, {BankOriginal, BankOriginal}} || {BankName, BankOriginal} <- BankInfo],
  print_responses(CustomerInfo, CustomerResults, BankResults),
  register(master, self()).

print_responses(CustomerInfo, CustomerResults, BankResults) ->
  receive
    {loan_request, CustomerName, NumberRequested, BankRequested} ->
      io:format("? ~p requests a loan of ~p dollar(s) from the ~p bank ~n", [CustomerName, NumberRequested, BankRequested]),
      print_responses(CustomerInfo, CustomerResults, BankResults);

    {approve_request, BankName, NumberRequested, CustomerName} ->
      io:format("$ The ~p bank approves a loan of ~p dollar(s) to ~p ~n", [BankName, NumberRequested, CustomerName]),
      BankInfoMap = maps:from_list(BankResults),
      {BankOriginal, BankBalance} = maps:get(BankName, BankInfoMap),
      UpdatedBankBalance = BankBalance - NumberRequested,
      UpdatedBankResults = lists:map(fun({Key, {Original, Value}}) -> case Key of BankName -> {BankName, {BankOriginal, UpdatedBankBalance}}; _ -> {Key, {Original, Value}} end end, BankResults),
      print_responses(CustomerInfo, CustomerResults, UpdatedBankResults);

    {deny_request, BankName, NumberRequested, CustomerName} ->
      io:format("$ The ~p bank denies a loan of ~p dollar(s) to ~p ~n", [BankName, NumberRequested, CustomerName]),
      print_responses(CustomerInfo, CustomerResults, BankResults);

    {success_end, CustomerName, RemainingLoan} ->
      CustomerInfoMap = maps:from_list(CustomerInfo),
      TargetLoan = maps:get(CustomerName, CustomerInfoMap),
      UpdatedCustomerResults = [{CustomerName, TargetLoan, RemainingLoan} | CustomerResults],
      print_responses(CustomerInfo, UpdatedCustomerResults, BankResults);

    {fail_end, CustomerName, RemainingLoan} ->
      CustomerInfoMap = maps:from_list(CustomerInfo),
      TargetLoan = maps:get(CustomerName, CustomerInfoMap),
      UpdatedCustomerResults = [{CustomerName, TargetLoan, RemainingLoan} | CustomerResults],
      print_responses(CustomerInfo, UpdatedCustomerResults, BankResults)
  after
    2000 -> 
      print_results(CustomerResults, BankResults)
  end.

print_results(CustomerResults, BankResults) ->
  io:format("~n~n** Banking Report **~n"),
  io:format("Customers:~n"),
  lists:foreach(fun({CustomerName, TargetLoan, RemainingLoan}) ->
                      io:format("  ~p: objective ~p, received ~p ~n", [CustomerName, TargetLoan, TargetLoan - RemainingLoan])
                end, CustomerResults), 
  io:format("-----~n"),

  TotalReceived = lists:foldl(fun({_CustomerName, TargetLoan, RemainingLoan}, Acc) -> Acc + TargetLoan - RemainingLoan end, 0, CustomerResults),
  TotalObjective = lists:foldl(fun({_CustomerName, TargetLoan, _RemainingLoan}, Acc) -> Acc + TargetLoan end, 0, CustomerResults),
  io:format("  Total: objective ~p, received ~p ~n~n", [TotalObjective, TotalReceived]),  

  io:format("Banks:~n"),
  lists:foreach(fun({BankName, {BankOriginal, BankBalance}}) ->
                      io:format("  ~p: original ~p, balance ~p ~n", [BankName, BankOriginal, BankBalance])
                end, BankResults), 
  io:format("-----~n"),

  TotalOriginal = lists:foldl(fun({_BankName, {BankOriginal, _BankBalance}}, Acc) -> Acc + BankOriginal end, 0, BankResults),
  TotalLoaned = lists:foldl(fun({_BankName, {BankOriginal, BankBalance}}, Acc) -> Acc + BankOriginal - BankBalance end, 0, BankResults),
  io:format("  Total: original ~p, loaned ~p ~n~n", [TotalOriginal, TotalLoaned]).