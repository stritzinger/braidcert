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
    Port = application:get_env(braidcert, port, 8080),
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
    {ok, CA} = file:read_file(ca_file()),
    Req1 = cowboy_req:set_resp_header(
        <<"content-type">>, "application/octet-stream", Req),
    {CA, Req1, State}.

accept_csr(#{path := <<"/csr/", ID/binary>>} = Req, State) ->
    {ok, Csr, Req1} = cowboy_req:read_body(Req),
    %---
    CertDir = cert_dir(ID),
    file:del_dir_r(CertDir),
    ok = filelib:ensure_dir(CertDir),
    ok = file:make_dir(CertDir),
    CsrFile = csr_file(CertDir),
    ok = file:write_file(CsrFile, Csr),
    %---
    CertFile = generate_cert(CertDir, CsrFile),
    {ok, Cert} = file:read_file(CertFile),
    %---
    file:del_dir_r(CertDir),
    %---
    Req2 = cowboy_req:set_resp_body(Cert, Req1),
    {true, Req2, State}.

generate_cert(CertDir, CsrFile) ->
    CertFile = cert_file(CertDir),
    Cmd = "openssl",
    Args = [
        "x509",
        "-req",
        "-in", CsrFile,
        "-CA", ca_file(),
        "-CAkey", ca_keyfile(),
        "-CAcreateserial",
        "-extfile", ext_file(),
        "-copy_extensions", "copy",
        "-out", CertFile
    ],
    {ok, _} = run_cmd(Cmd, Args),
    CertFile.

ext_file() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "certs", "cfg", "braidnet_v3_ext.cfg"]).

ca_file() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "certs", "braidcert.CA.pem"]).

ca_keyfile() ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "certs", "braidcert.CA.key"]).

csr_file(CertDir) ->
    filename:join(CertDir, "request.csr").

cert_file(CertDir) ->
    filename:join(CertDir, "cert.pem").

cert_dir(ID) when is_binary(ID) ->
    cert_dir(erlang:binary_to_list(ID));
cert_dir(ID) ->
    {ok, Cwd} = file:get_cwd(),
    filename:join([Cwd, "certs", ID]).

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
