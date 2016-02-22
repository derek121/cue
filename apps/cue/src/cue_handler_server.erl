-module(cue_handler_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_worker/1]).
-export([process/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_worker(Socket) ->
  gen_server:cast(?SERVER, {add_worker, Socket}),
  ok.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{}}.


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({add_worker, Socket}, State) ->
  spawn_link(?MODULE, process, [Socket]),
  {noreply, State};
  
handle_cast(_Request, State) ->
  {noreply, State}.


-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.


-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(Reason, State) ->
  lager:info("terminate(~p, ~p)", [Reason, State]),
  ok.


-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

process(Socket) ->
  process2(Socket, cue_queue:new()).

process2(_Socket, undefined) ->
  ok;
process2(Socket, Queue) ->
  case do_read(Socket) of
    {ok, RawInput} -> 
      Queue2 = handle_data(Socket, trim_crnl(RawInput), Queue),
      process2(Socket, Queue2);
    
    {error, closed} -> 
      ok
    %% Let any other error cause a crash...
  end.
  
handle_data(Socket, Input, Queue) ->
  Command = split_data(Input),
  handle_operation(Socket, Command, Queue).

handle_operation(Socket, [<<"in">>, Element], Queue) ->
  Queue2 = cue_queue:in(Queue, Element),
  do_write(Socket, [<<"Added to end of queue: ">>, Element]),
  Queue2;
handle_operation(Socket, [<<"out">>], Queue) ->
  case cue_queue:out(Queue) of
    [{Element, Queue2}] -> 
      do_write(Socket, [<<"Removed from front of queue: ">>, Element]),
      Queue2;
    [] ->
      do_write(Socket, [<<"Queue is empty">>]),
      []
  end;
handle_operation(Socket, [<<"">>], Queue) ->
  do_write(Socket, [<<"">>]),
  Queue;
handle_operation(Socket, [<<"quit">>], _Queue) ->
  do_close(Socket),
  undefined;
handle_operation(Socket, _Split, Queue) ->
  do_write(Socket, [<<"Illegal command. ">>, usage()]),
  Queue.

%% Trim trailing \r\n if present
trim_crnl(In) when is_binary(In) ->
  case binary:split(In, <<"\r\n">>) of
    [B, <<>>] -> B;
    [B]       -> B
  end.
  
%% Split on the first space, if present, and remove any leading whitespace in the
%%   second element. For example, <<"foo     bar">> will be split into
%%   [<<"foo">>, <<"bar">>]
split_data(Data) when is_binary(Data) ->
  case binary:split(Data, <<" ">>) of
    [B]     -> [B];
    [B, B2] -> [B, trim_leading_whitespace(B2)]
  end.
    
trim_leading_whitespace(<<" ", Rest/binary>>) ->
  trim_leading_whitespace(Rest);
trim_leading_whitespace(B) ->
  B.

do_read(Socket) ->
  gen_tcp:recv(Socket, 0).

do_write(Socket, Data) ->
  gen_tcp:send(Socket, [<<"[">>, date_now(), <<"] ">>, Data, <<"\n">>]).

do_close(Socket) ->
  gen_tcp:close(Socket).

date_now() ->
  qdate:to_string("Y-m-d H:i:s", calendar:local_time()).

usage() ->
  <<"Usage: in {value} | out | quit">>.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).

trim_crnl_test() ->
  ?assertEqual(<<"foo">>, trim_crnl(<<"foo\r\n">>)),
  ?assertEqual(<<"foo">>, trim_crnl(<<"foo">>)),
  ok.

split_data_test() ->
  ?assertEqual([<<"foo">>], split_data(<<"foo">>)),
  ?assertEqual([<<"foo">>, <<"bar">>], split_data(<<"foo   bar">>)).

-endif.

