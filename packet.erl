-module(packet).

-export([client/0,
         server4/0,
         server0/0]).

-define(PORT, 4000).
-define(DURATION, 30_000).
-define(MSG_BYTES, 1400).
%% ACTIVE applies only to server4 to get a fairer comparison
%% since server0 reads the entire TCP buffer in one go
-define(ACTIVE, once). %% true | once | N

client() ->
    {ok, Sock} = gen_tcp:connect("localhost", ?PORT, [binary, {packet, 0}]),
    io:format("Starting client~n", []),
    client_loop(Sock, 0).

client_loop(Sock, Counter) ->
    case gen_tcp:send(Sock, <<?MSG_BYTES:32, 1:(8*?MSG_BYTES)>>) of
        ok ->
            case Counter rem 10_000 of
                0 ->
                    io:format("client: sent ~w messages~n", [Counter]);
                _ ->
                    ok
            end,
            client_loop(Sock, Counter + 1);
        {error, Reason} ->
            io:format("client got '~p' with ~w sent messages~n", [Reason, Counter])
    end.

%% uses socket option {packet, 4}
server4() ->
    {ok, LSock} = gen_tcp:listen(?PORT, [binary, {packet, 4}, {active, ?ACTIVE}]),
    io:format("server: listening~n", []),
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("server: accepted~n", []),
    _TimerRef = erlang:send_after(?DURATION, self(), stop),
    server4_loop(Sock, 0).

server4_loop(Sock, Counter) ->
    case ?ACTIVE of
        once ->
            ok = inet:setopts(Sock, [{active, once}]);
        _ ->
            ok
    end,
    receive
        {tcp, Sock, Data} ->
            %% Use the data.
            ?MSG_BYTES = byte_size(Data),
            server4_loop(Sock, Counter + 1);
        {tcp_passive, Sock} ->
            % io:format("server: entered passive mode with ~w received messages~n", [Counter]),
            ok = inet:setopts(Sock, [{active, ?ACTIVE}]),
            server4_loop(Sock, Counter);
        Other ->
            io:format("server: got '~p' with ~w received messages~n", [Other, Counter]),
            ok = gen_tcp:close(Sock)
    end.

-record(parser,
        {
         data ::
         undefined |
         %% used only if the binary is smaller than 4 bytes
         binary() |
         {RemainingBytes :: non_neg_integer(), Reversed :: iodata()},
         counter :: non_neg_integer()
        }).

%% uses socket option {packet, 0}
server0() ->
    {ok, LSock} = gen_tcp:listen(?PORT, [binary, {packet, 0}]),
    io:format("server: listening~n", []),
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("server: accepted~n", []),
    _TimerRef = erlang:send_after(?DURATION, self(), stop),
    server0_loop(Sock, #parser{counter = 0}).

server0_loop(Sock, #parser{counter = Counter} = State0) ->
    ok = inet:setopts(Sock, [{active, once}]),
    receive
        {tcp, Sock, Data} ->
            State = incoming_data(Data, State0),
            server0_loop(Sock, State);
        Other ->
            io:format("server: got '~p' with ~w received messages~n", [Other, Counter]),
            ok = gen_tcp:close(Sock)
    end.

incoming_data(<<>>, State) ->
    State;
incoming_data(<<Size:32, Frame:Size/binary, Rem/binary>>,
              #parser{data = undefined,
                      counter = Counter} = State) ->
    %% Use the data. In this case, we sanity check that we parsed correctly.
    ?MSG_BYTES = byte_size(Frame),
    incoming_data(Rem,
                  State#parser{data = undefined,
                               counter = Counter + 1});
incoming_data(<<Size:32, Rem/binary>>,
              #parser{data = undefined} = State) ->
    %% not enough data to complete message, stash and await more data
    State#parser{data = {Size - byte_size(Rem), Rem}};
incoming_data(Data,
              #parser{data = undefined} = State)
  when byte_size(Data) < 4 ->
    %% not enough data to even know the size required
    %% just stash binary and hit last clause next
    State#parser{data = Data};
incoming_data(Data,
              #parser{data = {Size, Partial},
                      counter = Counter} = State) ->
    case Data of
        <<Part:Size/binary, Rem/binary>> ->
            %% Enough data, assemble frame into binary.
            Frame = assemble_frame(Partial, Part),
            %% Use the data. In this case, we sanity check that we parsed correctly.
            ?MSG_BYTES = byte_size(Frame),
            incoming_data(Rem,
                          State#parser{data = undefined,
                                       counter = Counter + 1});
        Rem ->
            %% still not enough data, stash further
            State#parser{data = {Size - byte_size(Rem),
                                 prepend_data(Partial, Rem)}}
    end;
incoming_data(Data,
              #parser{data = Partial} = State)
  when is_binary(Partial) ->
    incoming_data(<<Partial/binary, Data/binary>>,
                  State#parser{data = undefined}).

prepend_data(Prev, Data) when is_binary(Prev) ->
    [Data, Prev];
prepend_data(Prev, Data) when is_list(Prev) ->
    [Data | Prev].

assemble_frame(Prev, Data) when is_binary(Prev) ->
    <<Prev/binary, Data/binary>>;
assemble_frame(Prev, Data) when is_list(Prev) ->
    IOListReversed = [Data | Prev],
    IOList = lists:reverse(IOListReversed),
    %% This is expensive, but we want to compare apples with apples:
    %% server4/0 also gives us a single binary per message (instead of an iolist).
    iolist_to_binary(IOList).
