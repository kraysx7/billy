-module(billy_cbserver).
-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MODULE: transaction

create_transaction(ParamsMap) ->
    Node = billy_config:get(cbserver_node),
    rpc:call(Node, 'transaction', 'create', [ParamsMap]).

get_transaction(ParamsMap) ->
    Node = billy_config:get(cbserver_node),
    rpc:call(Node, 'transaction', 'get', [ParamsMap]).

get_transaction_cost_summ(ParamsMap) ->
    Node = billy_config:get(cbserver_node),
    rpc:call(Node, 'transaction', 'get_cost_summ', [ParamsMap]).

update_transaction(ParamsMap) ->
    Node = billy_config:get(cbserver_node),
    rpc:call(Node, 'transaction', 'update', [ParamsMap]).

close_transaction(TransactionId, NewBalance, Status) ->
    Node = billy_config:get(cbserver_node),
    rpc:call(Node, 'transaction', 'close', [TransactionId, NewBalance, Status]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  MODULE: user

%% Avalible ParamsMap :
%% #{remote_user_id, password}
%% #{remote_user_id}
%% #{name}
%% #{email}
%% #{user_id}
%% #{user_name, password}
get_user(ParamsMap) ->
    Node = billy_config:get(cbserver_node),
    rpc:call(Node, 'user', 'get', [ParamsMap]).


update_user_balance(UserId, Cost) ->
    Node = billy_config:get(cbserver_node),
    rpc:call(Node, 'user', 'update_balance', [UserId, Cost]).  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  MODULE: rate

get_rate(ParamsMap) ->
    Node = billy_config:get(cbserver_node),
    rpc:call(Node, 'rate', 'get', [ParamsMap]).
