# Braidcert

This application serves as the Public Key Infrastructure (PKI) for
[Braidnet](https://github.com/stritzinger/braidnet).

Braidcert handles generating SSL certificates used for TLS distribution
between Braidnet instances, as well as Braidnode instances.

Also refer to the README.md in the Braidnet repository for Braidnet-specific
details on setting up the applications to work together.


## Build

    $ rebar3 compile


## Local deployment

To run braidcert locally, just start a rebar3 shell:

    $ rebar3 shell


## Production environment

You might want to replace the throwaway CA certificate Braidcert generates
with a permanent one. See the [Configuration](#configuration) section below
for disabling the automatic CA certificate creation.

For now, Braidcert (and Braidnet) is meant to be deployed on [Fly.io](https://fly.io).
Create a new Fly application, replace the value of the `app` field
in the `fly.toml` file in this repo with your Fly app's name,
and deploy using `flyctl`.

## Configuration

Customize the certificate configuration files under `certs/cfg/`.

When Braidcert is ran via the rebar3 shell, `config/shell.config` applies.

When the relx release is ran, `config/container.config.src` applies.

The possible configuration values are:
```erlang
[
    {braidcert, [
        % The port Braidcert should listen on for requests from Braidnet:
        {cowboy_port, integer()},
        % Token for HTTP Bearer authentication between Braidcert and Braidnet:
        {key, binary()},
        % Whether Braidcert should generate a new CA certificate at startup:
        {generate_ca, boolean()}
    ]}
].
```
