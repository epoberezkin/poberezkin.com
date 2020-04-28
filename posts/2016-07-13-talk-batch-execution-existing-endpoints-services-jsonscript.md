---
title: Batch API requests with JSONScript
author: Evgeny Poberezkin
tags: talk, coding, javascript
ref: https://skillsmatter.com/skillscasts/8395-batch-execution-of-existing-endpoints-and-services-with-jsonscript
---

This is a [talk at FullStack London 2016](https://skillsmatter.com/skillscasts/8395-batch-execution-of-existing-endpoints-and-services-with-jsonscript).

<a href="https://skillsmatter.com/skillscasts/8395-batch-execution-of-existing-endpoints-and-services-with-jsonscript">
  <img src="/images/talk2016.jpg" alt="Batch API requests with JSONScript" width="100%">
</a>

> A very common situation in web development: you need to make multiple requests, often with some conditions and logic between calls, to get the required result.

> It can be achieved in three ways:

> 1. Sending multiple requests to the server and implementing all the processing logic in the client. The advantage of this approach is that the server remains unchanged and the client can easily change the logic and flow of requests. The disadvantage is the latency and the traffic - each request should travel via the network.
> 2. Implementing additional methods/endpoints/parameters in the server. The advantage of this approach is that the client has to make only one request. The disadvantage is that it requires changing the server (= coding + testing + documenting + deploying + monitoring + supporting...). When it is possible, it inevitably leads to the growing complexity of the remote system as more and more specialised methods/APIs are added to it.
> 3. Implement batch endpoints, e.g. using JSON RPC standard. While they allows to execute multiple calls in a single HTTP request, it doesn’t allow to implement any logic between the calls.

> [JSONScript](https://www.jsonscript.org/) is a simple tool that allows to create “a batch endpoint on steroids” - server-side scripted execution of existing endpoints and services.

> It is currently implemented in express middleware that allows to add JSONScript batch endpoint in a single line of code.
