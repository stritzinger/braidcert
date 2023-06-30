# braidcert

This application serves as the Public Key Infrastructure (PKI) for
[braidnet](https://github.com/stritzinger/braidnet).

Braidcert handles generating SSL certificates used for TLS distribution
between braidnet instances, as well as braidnode instances.

Also refer to the README.md in the braidnet repository for braidnet-specific
details on setting up the applications to work together.


## Build

    $ rebar3 compile


## Local deployment

To run braidcert locally, just start a rebar3 shell:

    $ rebar3 shell


## Production environment

You might want to replace the throwaway CA certificate braidcert generates
with a permanent one. See the [Configuration](#configuration) section below
for disabling the automatic CA certificate creation.

For now, braidcert (and braidnet) is meant to be deployed on [Fly.io](https://fly.io).
Create a new Fly application, replace the value of the `app` field
in the `fly.toml` file in this repo with your Fly app's name,
and deploy using `flyctl`.

## Configuration

Customize the certificate configuration files under `certs/cfg/`.

When braidcert is ran via the rebar3 shell, `config/shell.config` applies.

When the relx release is ran, `config/container.config.src` applies.

The possible configuration values are:
```erlang
[
    {braidcert, [
        % The port braidcert should listen on for requests from braidnet:
        {cowboy_port, integer()},
        % Token for HTTP Bearer authentication between braidcert and braidnet:
        {key, binary()},
        % Whether braidcert should generate a new CA certificate at startup:
        {generate_ca, boolean()}
    ]}
].
```
