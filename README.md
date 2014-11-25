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
  
* Resonable behaviour when an app instance is unreachable

What is planned:

* Support for persistence (alghough I am unsure if the existence of 
  jsessionid is a good way to support it)

* Reasonably compatibile YAML configuration file with gorouter.
    
* Working logging system.
    
* Endpoints /route and /varz.
    
* distributed least connection loadbalancing

* statistics that could help with deciding if autoscaling is needed
    
* application to view these statistics
