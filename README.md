# HTTP/HTTPS redirection microservice

This application is a standalone microservice with options to:

* Force HTTP -> HTTPS redirection
* Force `www` hostnames

## What problem does it solve?

* You are running a HTTP service that is not reversed proxied by APACHE or NGINX and you want a generic way to force HTTPS redirection
* You want to force all users to use the `www` carnical host name

## Why use this project
* It's packaged as an extreamly small Docker image making it portable and easy to deply

## Why did I build httpredirector

I am using `Google Cloud` to deploy a HTTP service running in a managed group pacakged as a Docker image. 

The service runs on port `8080` and I have a `Cloud Loadbalancer` sat in front of the service terminating `SSL`.

I wanted a simple easy generic way to force SSL. As I am deploying using Docker I can run this microservice on a node and route all non `HTTPS` traffic at the load balancer to the redirection service.

Existing Docker images supported HTTP -> HTTPS on with no support for forcing `www`. The Docker images where also extramly large. `httpredirector` is a 1.8MB Docker image.


## Building

### Create a static compiled binary

To create a staticaly compiled binary we use Docker.

From the project directory run the below command which should give you a bash prompt.

```
docker run -v $(pwd):/usr/src/build -v ${HOME}/.stack:/root/.stack -w /usr/src/build -it q4uw/haskell_build_env bash
```

This will:
* Give you a bash prompt in the container
* Mount your project directory in the docker container
* Mount your local `stack` home in the container to make use of artifact caching
* Set the working directory to the project root

To staticly compile the project run the below command which will place the binary in the `./dist` directory.

```
stack install --local-bin-path /usr/src/build/dist --ghc-options '-optl-static -fPIC -optc-Os'
```

Next compress the binary using `UPX`. This significantly reduces the size.

```
upx --best --ultra-brute dist/httpredirector-exe
```

You can now exit the shell.

### Package the binary as a docker image

```
docker build --tag httpredirector/httpredirector .
```

## Running the image

* To force SSL you need to pass the `FORCE_SSL` environment variable to Docker
* To force `WWW` redirect you need to pass the `FORCE_WWW` environment variable to Docker

Example useage which exposes the redirection on port 80

```
docker run --rm -ti -e FORCE_SSL=TRUE -e FORCE_WWW=TRUE -p 80:8080 httpredirector/httpredirector
```

Verify it is working

```
$ curl -I localhost:8081

HTTP/1.1 307 Temporary Redirect
Date: Sat, 10 Dec 2016 07:19:01 GMT
Server: Warp/3.2.9
Location: https://www.localhost:8081/
```

