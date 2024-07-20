%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).       
 
-export([start/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
   % ok=test1(),
   % ok=test2(),
    


    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(5000),
    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test2()->    
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
test1()->    
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {badrpc,{'EXIT',{badarith,_}}}=rpc:call(node(),adder,divi,[2,0],5000),
    {badrpc,{'EXIT',{undef,[{glurk,glurk,[2,0],[]}]}}}=rpc:call(node(),glurk,glurk,[2,0],5000),
    {badrpc,{'EXIT',{noproc,{gen_server,call,[adder,{add,2,0},infinity]}}}}=rpc:call(node(),adder,add,[2,0],5000),
    ok=application:start(adder),
    pong=adder:ping(),
    {badrpc,{'EXIT',_}}=rpc:call(node(),adder,add,[m,0],5000),
    {badrpc,{'EXIT',{undef,[{adder,add,[1,2,0],[]}]}}}=rpc:call(node(),adder,add,[1,2,0],5000),
    {badrpc,timeout}=rpc:call(node(),adder,add,[1,2,0],0),

    %% 
    ok=application:stop(adder), 
    {badrpc,{'EXIT',Reason1}}=rpc:call(node(),adder,divi,[2,0],5000),
    {badrpc,{'EXIT',Reason2}}=rpc:call(node(),glurk,glurk,[2,0],5000),
    {badrpc,{'EXIT',Reason3}}=rpc:call(node(),adder,add,[2,0],5000),
    ok=application:start(adder),
    pong=adder:ping(),
    {badrpc,{'EXIT',Reason4}}=rpc:call(node(),adder,add,[m,0],5000),
    {badrpc,{'EXIT',Reason5}}=rpc:call(node(),adder,add,[1,2,0],5000),
    {badrpc,timeout}=rpc:call(node(),adder,add,[1,2,0],0),

    %% cast
    ok=application:stop(adder), 
    true=rpc:cast(node(),adder,divi,[2,0]),
    true=rpc:cast(node(),glurk,glurk,[2,0]),
    true=rpc:cast(node(),adder,add,[2,0]),
    ok=application:start(adder),
    pong=adder:ping(),
    true=rpc:cast(node(),adder,add,[m,0]),
    true=rpc:cast(node(),adder,add,[1,2,0]),
 
    io:format("Start ~p~n",[{Reason1,Reason2,Reason3,Reason4,Reason5,?MODULE,?FUNCTION_NAME}]),
    
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    ok=application:start(adder), 
    pong=log:ping(),
    pong=rd:ping(),
    pong=adder:ping(),

    42=adder:add(20,22),
    ok.
