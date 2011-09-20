-module(eon).

-behaviour(gen_server).

-record(server,{pid , table, filename = ""}).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,
	terminate/2,code_change/3]).

-export([start/0,insert/1,insertlist/1,close/1,remove/1,
	 print_record/0,get_record/0,save/1,save_as/1,
	 load/1,import/1,new/1,search/1,lookup_strid/3]).

-export([file2list/1,remove_last/2,autosave/1]).

start()->
    gen_server:start({local,?MODULE},?MODULE,[],[{debug,[trace]}]).

init([]) ->
    %% duplicate_bag to allows duplicate keys and values
    Table = ets:new(table,[duplicate_bag]),
    Server = #server{pid = self(),table = Table},
    %%autosave(Server),
    %%spawn(?MODULE,autosave,[Server]),
    {ok,Server}.

new(Filename)->
    gen_server:cast(?MODULE,{new,Filename}).

insert(Object)->
    gen_server:cast(?MODULE,{insert,Object}).

insertlist(List)->
    gen_server:cast(?MODULE,{insertlist,List}).

remove(Object)->
    gen_server:cast(?MODULE,{remove,Object}).

search(Table)->
    gen_server:call(?MODULE,{search,Table}).

%% D1 = träff område, D2 = delområde, D3 = skada
lookup_strid(D1,D2,D3) ->
    gen_server:call(?MODULE,{lookup_strid,D1,D2,D3}).

save(Filename)->
    gen_server:cast(?MODULE,{save,Filename}).

save_as(Filename)->
    gen_server:cast(?MODULE,{save_as,Filename}).

load(Filename)->
    gen_server:cast(?MODULE,{load,Filename}).

import(Filename)->
    gen_server:cast(?MODULE,{load,Filename}).

%% helper functions
close(Pid) ->
    gen_server:call(Pid,terminate).

get_record()->
    gen_server:call(?MODULE,get_record).

print_record()->
    gen_server:cast(?MODULE,{print_record}).
%% helper functions end

%% server functions

handle_cast({new,Filename},Server)->
    Newtable = ets:new(newtable,[duplicate_bag]),
    Newserver = Server#server{table = Newtable, filename = Filename},
    {noreply,Newserver};

%% Object should be on the form
%% {table,key,value} where table is which table, key is the roll of the dice
%% and value is the result of the dice
handle_cast({insert,Object},Server)->
    ets:insert(Server#server.table,Object),
    {noreply,Server};

%% List is a list of Objects
handle_cast({insertlist,[]},Server) ->
    {noreply,Server};

handle_cast({insertlist,[X|XS]},Server)->
    handle_cast({insert,X},Server),
    handle_cast({insertlist,XS},Server);

handle_cast({remove,Object},Server) ->
    ets:delete(Server#server.table,Object),
    {noreply,Server};

handle_cast({save,Filename},Server)->
    ets:tab2file(Server#server.table,Filename),
    {noreply,Server};

handle_cast({save_as,Filename},Server)->
    Newserver = Server#server{filename = Filename},
    ets:tab2file(Server#server.table,Filename),
    {noreply,Newserver};

handle_cast({load,Filename},Server)->
    Newserver = Server#server{filename = Filename,
			      table = ets:file2tab(Filename)},
    %%Newtable = ets:file2tab(Filename),
    {noreply,Newserver};

handle_cast({import,Filename},Server)->
    file:open(Filename,[read,binary]),
    Newserver = Server#server{filename = Filename},
    List = file2list(Filename),
    handle_cast({insertlist,List},Server),
    file:close(Filename),
    {noreply,Newserver};

handle_cast({print_record},Server) ->
    io:format("server pid: ~p.~n",[Server#server.pid]),
	Tablist = ets:select(Server#server.table,
			     [{{'$1','$2','$3'},[],['$_']}]),
    io:format("table: ~p.~n",[Tablist]),
    io:format("filename: ~p.~n",[Server#server.filename]),
    {noreply,Server}.

handle_call({search,Table},_From,Server)->
    Test = ets:lookup(Server#server.table,Table),
    {reply,Test,Server};

handle_call({lookup_strid,D1,D2,D3},_From,Server)->
    if
	D1<10 ->
	    if
		D2<4->
		    Test = {huvud,skalle};
		D2<7->
		    Test = {huvud,hals};
		D2<10->
		    Test = {huvud,ansikte}
	    end;
	D1<20 ->
	    Test = bröst;
	D1<30 ->
	    Test = bål;
	D1<40 ->
	    Test = 'höger arm';
	D1<50 ->
	    Test = 'vänster arm';
	D1<60 ->
	    Test = 'Höger ben';
	D1<70 ->
	    Test = 'vänster ben'
    end,
    %% ladda korrekt tabel
    {reply,Test,Server};

%% returns the pid
handle_call(get_record,_From,Server) ->
    {reply,Server#server.pid,Server};

handle_call(terminate,_From,Server)->
    handle_cast({save,Server#server.filename},Server),
    {stop,normal,ok,Server}.

%% default syntax
%%handle_call(_Msg,_From,Table)->
%%    {replay,ok,Table}.

%%handle_cast(_Msg,Table)->
%%    {noreplay,Table}.
%% end default syntax

%% stuff that must be here

handle_info(Msg,Server)->
    io:format("unexpected message: ~p.~n",[Msg]),
    {noreply,Server}.

terminate(normal,_Server)->
    io:format("server closed~n"),
    ok.

code_change(_Oldversion,?MODULE,_Extra)->
    {ok,?MODULE}.

%% local functions

file2list(File)->
	{ok,Device} = file:open(File,[read]),
	lists:reverse(file2listAux(Device,[])).
	
file2listAux(Device,Acc)->
	case io:get_line(Device,"") of
		eof -> file:close(Device), Acc;
		Line ->
			file2listAux(Device,[remove_last([Line],[])|Acc])
	end.

remove_last([],Acc) -> Acc;
remove_last([X|XS],Acc) ->
    remove_last(XS,[lists:delete($\n,X)|Acc]).
	
autosave(Server) ->
	io:format("are we here~n"),
	File = Server#server.filename,
	receive
		{autosave} ->
			if 
				File /= "" ->
					io:format("autosaveing file: ~p.~n",[File]),
					handle_cast({save,File},Server);
				true ->
					io:format("no filename~n"),
					autosave(Server)
			end
		after
			1000 ->
			io:format("after 5000~n"),
			self() ! {autosave},
			autosave(Server)
	end.
			
