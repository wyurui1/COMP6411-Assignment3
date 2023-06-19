-module(customer).
-export([spawn_customers/2]).

% Customer Process
spawn_customers([], _Banks) ->
  [];
spawn_customers([{CustomerName, CustomerBalance} | RestCustomers], Banks) ->
  MasterPid = self(),  
  CustomerPid = spawn(fun() ->   timer:sleep(200), customer_task(MasterPid, CustomerName, CustomerBalance, Banks) end),
  register(CustomerName, CustomerPid),
  [{CustomerName, CustomerPid} | spawn_customers(RestCustomers, Banks)].

customer_task(_MasterPid, _CustomerName, 0, _Banks) ->
  _MasterPid ! {success_end, _CustomerName, 0},
  exit(normal);
customer_task(_MasterPid, _CustomerName, _RemainingLoan, []) ->
  _MasterPid ! {fail_end, _CustomerName, _RemainingLoan},
  exit(normal);
customer_task(MasterPid, CustomerName, RemainingLoan, Banks) ->
  {LoanRequest, _RemainingLoan} = generate_loan_request(RemainingLoan),
  {BankName, _BankLimit} = select_bank(Banks),
  
  TargetBankPid = whereis(BankName),
  timer:sleep(rand:uniform(91) + 10), 
  TargetBankPid ! {loan_request, LoanRequest, CustomerName},
  MasterPid ! {loan_request, CustomerName, LoanRequest, BankName},

  receive 
    {approved_response, _BankName} ->
      UpdatedRemainingLoan = RemainingLoan - LoanRequest,
      customer_task(MasterPid, CustomerName, UpdatedRemainingLoan, Banks);   
    {denied_response, BankName} ->
      RestBanks = remove_bank(BankName, Banks),
      customer_task(MasterPid, CustomerName, RemainingLoan, RestBanks)
  end.

generate_loan_request(RemainingLoan) ->
  MaxLoanAmount = erlang:min(50, RemainingLoan),
  case MaxLoanAmount of
    0 -> {0, RemainingLoan};
    _ ->
      LoanRequest = rand:uniform(MaxLoanAmount),
      {LoanRequest, RemainingLoan}
  end.

select_bank(Banks) ->
  RandomIndex = rand:uniform(length(Banks)),
  lists:nth(RandomIndex, Banks).

remove_bank(_BankName, []) ->
  [];
remove_bank(BankName, [{BankName, _} | RestBanks]) ->
  remove_bank(BankName, RestBanks);
remove_bank(BankName, [{OtherBankName, BankPid} | RestBanks]) ->
  [{OtherBankName, BankPid} | remove_bank(BankName, RestBanks)].