HoRouter
========

This is an alternative router implementation for Cloud Foundry. It is written mostly 
for fun but it seems it could work quite well. 

What it can do:
    
* Respects the gorouter NATS protocol for automatic registration of application
  instances.

* WebSockets support (untested).
    
* Keepalive connections to the application instances.
    
* Least connection loadbalancing with limiting maximum parallel requests 
  on each application instance. (local; not distributed)

* Automatic removal of node that returns 'Connection refused' from the pool.

What is planned:

* The current http-client package uses 4K block, which makes it quite slow.
  128K block makes it much faster, but will require a change in http-client.

* Different timeouts for creating TCP connection to DEA and for response timeout
  (faster recovery during failure).
    
* Reasonably compatibile YAML configuration file with gorouter.
    
* Working logging system.
    
* Endpoints /route and /varz.
    
* Support for persistence (alghough I am unsure if the existence of 
  jsessionid is a good way to support it)

* distributed least connection loadbalancing

* statistics that could help with deciding weather autoscaling is needed
    
* application to view these statistics
