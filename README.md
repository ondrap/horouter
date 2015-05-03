HoRouter
========

This is an alternative router implementation for Cloud Foundry. It is written mostly
for fun but it seems it could work quite well.

### Usage

```sh
cabal update
cabal install

# run it
runhaskell Main.hs

# ...or build then run
cabal build
./dist/build/horouter/horouter -c examples/config.yml

Waiting for instances to publish their mappings...
Serving requests.

```

### What it can do

* Respects the gorouter NATS protocol for automatic registration of application
  instances.

* WebSockets support (untested).

* Keepalive connections to the application instances.

* Least connection loadbalancing with limiting maximum parallel requests
  on each application instance. (local; not distributed)

* Resonable behaviour when an app instance is unreachable

* Support for persistence (based on existence of JSESSIONID cookie)

* Reasonably compatibile YAML configuration file with gorouter.

### What is planned

* Configurable persistence support

* Working logging system.

* Endpoints /route and /varz.

* Customizable error pages

* statistics that could help with deciding if autoscaling is needed

* throttling

* distributed least connection loadbalancing

* application to view these statistics
