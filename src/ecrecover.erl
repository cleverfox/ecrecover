-module(ecrecover).

%% API
-export([recover/2, recover/3, sign/3]).

%% NIF
-export([load/0]).
-on_load(load/0).

%%=============================================================================
%% NIF API

load() ->
    EbinDir = filename:dirname(code:which(?MODULE)),
    AppDir = filename:dirname(EbinDir),
    PrivDir = filename:join(AppDir, "priv"),
    SoName = filename:join(PrivDir, atom_to_list(?MODULE)),
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

%%=============================================================================
%% External API

-spec recover(<<_:(32*8)>>, <<_:(65*8)>>) -> <<_:(32*8)>>.
recover(Hash, <<V, Sig:64/binary>>) when V == 27; V == 28 ->
    RecId = V - 27,
    case recover(Hash, Sig, RecId) of
        {ok, <<4, XY:64/binary>>} ->
            <<_:12/bytes, ShortPub:20/bytes>> = keccak256(XY),
            <<0:96, ShortPub/binary>>;
        {error, _} ->
            <<0:256>>
        end;
recover(_Hash, _VSig) ->
    <<0:256>>.

-spec recover(<<_:(32*8)>>, <<_:(64*8)>>, integer()) -> {ok,<<_:(65*8)>>}.
recover(_Hash, _Sig, _RecId) ->
    not_loaded(?LINE).

-spec sign(<<_:256>>, <<_:256>>, <<_:256>>) -> {ok,<<_:272>>,integer()}.
sign(_Hash, _Priv, _Nonce) ->
    not_loaded(?LINE).

%%=============================================================================
%% Internal Functions

keccak256(Bin) ->
    sha3:hash(256, Bin).
