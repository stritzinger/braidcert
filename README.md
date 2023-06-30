Braidcert
=====

This application serves as the Public Key Infrastructure (PKI) for
[Braidnet](https://github.com/stritzinger/braidnet).

Braidcert handles generating SSL certificates used for TLS distribution
between Braidnet instances, as well as Braidnode instances.

Also refer to the README.md in the Braidnet repository for Braidnet-specific
details on setting up the applications to work together.


Build
-----

    $ rebar3 compile


Local deployment
-----
To run braidcert locally, first execute the startup script manually:

    $ ./hooks/pre_start

This will generate a self-signed CA certificate for Braidcert under `certs/`.

Then, just start a rebar3 shell:

    $ rebar3 shell


Production environment
-----
The relx release will by default also run the above pre_start script to generate
a certificate.
You might want to replace this throwaway certificate with a permanent one.

For now, Braidcert (and Braidnet) is meant to be deployed on [Fly.io](https://fly.io).
Create a new Fly application, replace the value of the `app` field
in the `fly.toml` file in this repo with your Fly app's name,
and deploy using `flyctl`.

Configuration
-----
Customize the certificate configuration files under `certs/cfg/`.

When Braidcert is ran via the rebar3 shell, `config/shell.config` applies.

When the relx release is ran, `config/container.config.src` applies.
In this case, you'll have to provide a secret under the `key` field
that Braidcert and Braidnet can use for authentication.
