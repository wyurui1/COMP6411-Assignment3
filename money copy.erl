-module(money).
-export([start/1]).

start(Args) ->
  CustomerFile = lists:nth(1, Args),
  BankFile = lists:nth(2, Args),
  {ok, CustomerInfo} = file:consult(CustomerFile),
  {ok, BankInfo} = file:consult(BankFile),
  Log = [],
  spawn_master(Log, CustomerInfo, BankInfo).

spawn_master(Log, CustomerInfo, BankInfo) ->
  io:format("Try to spawn Master process.~n"),
  BankPids = spawn_banks(Log, BankInfo, CustomerInfo),
  timer:sleep(200),
  CustomerPids = spawn_customers(Log, CustomerInfo, BankPids),
  collect_loan_responses(Log, CustomerPids, BankPids).

spawn_customers(Log, [], _Banks) ->
  print_log(Log),
  [];
spawn_customers(Log, [{CustomerName, CustomerBalance} | RestCustomers], Banks) ->
  CustomerPid = spawn_link(fun() -> customer_task(Log, CustomerName, CustomerBalance, Banks) end),
  [{CustomerName, CustomerPid} | spawn_customers(Log, RestCustomers, Banks)].

spawn_banks(Log, [], _Customers) ->
  print_log(Log),
  [];
spawn_banks(Log, [{BankName, BankBalance} | RestBanks], Customers) ->
  BankPid = spawn_link(fun() -> bank_task(Log, BankName, BankBalance, Customers) end),
  [{BankName, BankPid} | spawn_banks(Log, RestBanks, Customers)].

customer_task(Log, CustomerName, CustomerBalance, Banks) ->
  io:format("Customer ~p created. ~p ~n", [CustomerName, Banks]),
  NewLog = Log ++ [io_lib:format("Customer ~p created.", [CustomerName])],
  perform_customer_tasks(NewLog, CustomerName, CustomerBalance, Banks).

bank_task(Log, BankName, BankBalance, Customers) ->
  io:format("Bank ~p created.~n", [BankName]),
  NewLog = Log ++ [io_lib:format("Bank ~p created.", [BankName])],
  perform_bank_tasks(NewLog, BankName, BankBalance, Customers).

perform_customer_tasks(Log, CustomerName, CustomerBalance, BankPids) ->
  NewLog = Log ++ [io_lib:format("Customer ~p created.", [CustomerName])],
  perform_loan_requests(NewLog, CustomerName, CustomerBalance, BankPids).

perform_loan_requests(Log, _CustomerName, 0, _Banks) ->
  print_log(Log),
  ok;
perform_loan_requests(Log, CustomerName, RemainingLoan, BankPids) ->
  {LoanRequest, UpdatedRemainingLoan} = generate_loan_request(RemainingLoan),
  {BankName, UpdatedBankPids} = select_bank(BankPids),
  io:format("Customer ~p requesting loan of ~p dollars from Bank ~p. Current Balance is ~p. ~n", [CustomerName, LoanRequest, UpdatedBankPids, UpdatedRemainingLoan]),
  NewLog = Log ++ [io_lib:format("Customer ~p requesting loan of ~p dollars from Bank ~p.", [CustomerName, LoanRequest, BankName])],
  
  timer:sleep(random:uniform(91) + 10), 
  UpdatedBankPids ! {self(), {loan_request, LoanRequest}},
  perform_loan_requests(NewLog, CustomerName, UpdatedRemainingLoan, BankPids).

perform_bank_tasks(Log, BankName, BankBalance, Customers) ->
  NewLog = Log ++ [io_lib:format("Bank ~p balance: ~p", [BankName, BankBalance])],
  receive {CustomerPid, {loan_request, LoanRequest}} ->
    case BankBalance - LoanRequest >= 0 of
      true ->
        BankBalance1 = BankBalance - LoanRequest,
        NewLog1 = NewLog ++ [io_lib:format("Loan granted to Customer ~p.", [CustomerPid])],
        io:format("Loan granted to Customer ~p. Balance is ~p. ~n", [CustomerPid, BankBalance1]),
        CustomerPid ! {bank_response, granted},
        perform_bank_tasks(NewLog1, BankName, BankBalance1, Customers);
      false ->
        NewLog1 = NewLog ++ [io_lib:format("Loan rejected to Customer ~p.", [CustomerPid])],
        io:format("Loan rejected to Customer ~p.~n", [CustomerPid]),
        CustomerPid ! {bank_response, rejected},
        perform_bank_tasks(NewLog1, BankName, BankBalance, Customers)
    end
  end.

collect_loan_responses(Log, Customers, Banks) ->
  collect_loan_responses(Log, Customers, Banks, []).

collect_loan_responses(Log, [], _Banks, UpdatedCustomers) ->
  print_log(Log),
  UpdatedCustomers;
collect_loan_responses(Log, [{CustomerName, CustomerPid} | RestCustomers], Banks, UpdatedCustomers) ->
  receive
    {bank_response, granted} ->
      NewLog = Log ++ [io_lib:format("Loan granted to Customer ~p.", [CustomerName])],
      collect_loan_responses(NewLog, RestCustomers, Banks, UpdatedCustomers);
    {bank_response, rejected} ->
      NewLog = Log ++ [io_lib:format("Loan rejected to Customer ~p.", [CustomerName])],
      UpdatedCustomers1 = [{CustomerName, CustomerPid} | UpdatedCustomers],
      collect_loan_responses(NewLog, RestCustomers, Banks, UpdatedCustomers1)
  end.

print_log([]) ->
  ok;
print_log([Msg | Rest]) ->
  %io:format("~s~n", [Msg]),
  print_log(Rest).

generate_loan_request(RemainingLoan) ->
  MaxLoanAmount = erlang:min(50, RemainingLoan),
  case MaxLoanAmount of
    0 -> {0, RemainingLoan};
    _ ->
      LoanRequest = rand:uniform(MaxLoanAmount),
      UpdatedRemainingLoan = RemainingLoan - LoanRequest,
      {LoanRequest, UpdatedRemainingLoan}
  end.


select_bank(Banks) ->
  RandomIndex = rand:uniform(length(Banks)),
  lists:nth(RandomIndex, Banks).

remove_bank(_BankName, []) ->
  [];
remove_bank(BankName, [{BankName, _} | RestBanks]) ->
  remove_bank(BankName, RestBanks);
remove_bank(BankName, [Bank | RestBanks]) ->
  [Bank | remove_bank(BankName, RestBanks)].

