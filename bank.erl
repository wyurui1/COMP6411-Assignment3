-module(bank).
-export([spawn_banks/2]).

% Bank Process
spawn_banks([], _Customers) ->
  [];
spawn_banks([{BankName, BankBalance} | RestBanks], Customers) ->
  MasterPid = self(),
  BankPid = spawn(fun() -> bank_task(MasterPid, BankName, BankBalance, Customers) end),
  register(BankName, BankPid),
  [{BankName, BankPid} | spawn_banks(RestBanks, Customers)].

bank_task(MasterPid, BankName, BankBalance, Customers) ->
  receive {loan_request, LoanRequest, CustomerName} ->
    case BankBalance - LoanRequest > 0 of
      true ->
        BankBalance1 = BankBalance - LoanRequest,
        CustomerPid = whereis(CustomerName),
        CustomerPid ! {approved_response, BankName},
        MasterPid ! {approve_request, BankName, LoanRequest, CustomerName},
        bank_task(MasterPid, BankName, BankBalance1, Customers);
      false ->
        CustomerPid = whereis(CustomerName),
        CustomerPid ! {denied_response, BankName},
        MasterPid ! {deny_request, BankName, LoanRequest, CustomerName},
        bank_task(MasterPid, BankName, BankBalance, Customers)
    end
  end.