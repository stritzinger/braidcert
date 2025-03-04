-module(braidcert_api).

-export([
    start_handler/0,
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_accepted/2,
    content_types_provided/2,
    provide/2,
    accept/2
]).

start_handler() ->
    case application:get_env(braidcert, generate_ca, false) of
        true  -> generate_ca();
        false -> ok
    end,
    Port = application:get_env(braidcert, cowboy_port, 8080),
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', ?MODULE, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(braidcert, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),
    ok.

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(#{path := <<"/ca">>} = Req, State) ->
    Methods = [<<"GET">>],
    {Methods, Req, State};

allowed_methods(#{path := <<"/csr/", _/binary>>} = Req, State) ->
    Methods = [<<"PUT">>],
    {Methods, Req, State}.

is_authorized(Req, State) ->
    {ok, Key} = application:get_env(braidcert, key),
    try cowboy_req:parse_header(<<"authorization">>, Req) of
        {bearer, Key} ->
            {true, Req, State};
        _ ->
            {{false, <<"Bearer">>}, Req, State}
    catch _:_ ->
        {{false, <<"Bearer">>}, Req, State}
    end.

content_types_accepted(Req, State) ->
    ContentTypes = [
        {<<"application/octet-stream">>, accept}
    ],
    {ContentTypes, Req, State}.

content_types_provided(Req, State) ->
    ContentTypes = [
        {<<"application/octet-stream">>, provide}
    ],
    {ContentTypes, Req, State}.

provide(#{path := <<"/ca">>} = Req, State) ->
    provide_ca(Req, State).

accept(#{path := <<"/csr/", _/binary>>} = Req, State) ->
    accept_csr(Req, State).

provide_ca(Req, State) ->
    {ok, CA} = file:read_file(ca_cert_path()),
    Req1 = cowboy_req:set_resp_header(
        <<"content-type">>, "application/octet-stream", Req),
    {CA, Req1, State}.

accept_csr(#{path := <<"/csr/", ClientID/binary>>} = Req, State) ->
    {ok, Csr, Req1} = cowboy_req:read_body(Req),
    %---
    CertDir = client_cert_dir(ClientID),
    file:del_dir_r(CertDir),
    ok = filelib:ensure_dir(CertDir),
    ok = file:make_dir(CertDir),
    CsrFile = client_csr_path(ClientID),
    ok = file:write_file(CsrFile, Csr),
    %---
    CertFile = generate_cert(ClientID, CsrFile),
    {ok, Cert} = file:read_file(CertFile),
    %---
    file:del_dir_r(CertDir),
    %---
    Req2 = cowboy_req:set_resp_body(Cert, Req1),
    {true, Req2, State}.

generate_ca() ->
    % Generate private key:
    Cmd = "openssl",
    Args1 = [
        "genrsa",
        "-out", ca_key_path(),
        "4096"
    ],
    {ok, _} = run_cmd(Cmd, Args1),
    % Generate CA certificate:
    Args2 = [
        "req",
        "-new",
        "-x509",
        "-config", ca_cfg_path(),
        "-days", "30000",
        "-key", ca_key_path(),
        "-out", ca_cert_path()
    ],
    {ok, _} = run_cmd(Cmd, Args2).

generate_cert(ClientID, CsrFile) ->
    CertFile = client_cert_path(ClientID),
    Cmd = "openssl",
    Args = [
        "x509",
        "-req",
        "-in", CsrFile,
        "-CA", ca_cert_path(),
        "-CAkey", ca_key_path(),
        "-CAcreateserial",
        "-extfile", client_ext_path(),
        "-copy_extensions", "copy",
        "-days", "30000",
        "-out", CertFile
    ],
    {ok, _} = run_cmd(Cmd, Args),
    CertFile.

ca_cert_path() ->
    filename:join(certs_dir(), "braidcert.CA.pem").

ca_key_path() ->
    filename:join(certs_dir(), "braidcert.CA.key").

ca_cfg_path() ->
    Priv = code:priv_dir(braidcert),
    filename:join([Priv, "cert_cfg", "braidcert.CA.cfg"]).

client_ext_path() ->
    Priv = code:priv_dir(braidcert),
    filename:join([Priv, "cert_cfg", "braidnet_v3_ext.cfg"]).

client_csr_path(ClientID) ->
    filename:join(client_cert_dir(ClientID), "request.csr").

client_cert_path(ClientID) ->
    filename:join(client_cert_dir(ClientID), "cert.pem").

client_cert_dir(ClientID) when is_binary(ClientID) ->
    client_cert_dir(erlang:binary_to_list(ClientID));
client_cert_dir(ClientID) ->
    filename:join([certs_dir(), "requests", ClientID]).

certs_dir() ->
    case application:get_env(braidcert, certs_dir, undefined) of
        undefined ->
            {ok, Cwd} = file:get_cwd(),
            Path = filename:join(Cwd, "certs"),
            filelib:ensure_path(Path),
            Path;
        WorkDir ->
            WorkDir
    end.

run_cmd(Executable, Args) ->
    Path = os:find_executable(Executable),
    Opts = [
        {args, Args}, {line, 1024},
        use_stdio, binary, exit_status, stderr_to_stdout
    ],
    Port = erlang:open_port({spawn_executable, Path}, Opts),
    case run_cmd_receive(Port, <<"">>, []) of
        {0, Lines} -> {ok, Lines};
        {Error, Lines} -> {{error, Error}, Lines}
    end.

run_cmd_receive(Port, CurrentLine, Lines) ->
    receive
        {Port, {exit_status, Status}} ->
            Lines1 = lists:reverse(Lines),
            {Status, Lines1};
        {Port, {data, {noeol, Data}}} ->
            CurrentLine1 = <<CurrentLine/binary, Data/binary>>,
            run_cmd_receive(Port, CurrentLine1, Lines);
        {Port, {data, {eol, Data}}} ->
            CurrentLine1 = <<CurrentLine/binary, Data/binary>>,
            run_cmd_receive(Port, <<"">>, [CurrentLine1 | Lines])
    end.
