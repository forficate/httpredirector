# HTTP/HTTPS redirection microservice

This application is a standalone microservice with options to:

* Force HTTP -> HTTPS redirection
* Force `www` hostnames
* Hostname whitelisting to prevent unathorized 3rd parties using you as a redirect service

## What problem does it solve?

* You are running a HTTP service that is not reversed proxied by APACHE or NGINX and you want a generic way to force HTTPS redirection
* You want to force all users to use the `www` carnical host name
* You are already deploying Docker containers

## Why use this project
* It's packaged as an extreamly small Docker image making it portable and easy to deploy at only 1.8MB in size.

## Why did I build httpredirector

I am using `Google Cloud` to deploy a HTTP service running in a managed group packaged as a Docker image. 

The service runs on port `8080` and I have a `Cloud Loadbalancer` in front of the service terminating `SSL`.

I wanted a simple easy generic way to force SSL. As I am deploying using Docker I can run this microservice on a node and route all non `HTTPS` traffic at the load balancer to the redirection service.

Existing HTTPS redirection images on Dockerhub did not support forcing `www` and are also extremely large. `httpredirector` is a 1.8MB Docker image and supports both.

**The existing redirection images on Dockerhub also allow anyone to use your server as a redirection service. This is true of [geldim/https-redirect](https://hub.docker.com/r/geldim/https-redirect/) which has 500K+ pulls and [camptocamp/https-redirect](https://hub.docker.com/r/camptocamp/https-redirect/) which has 10k+ pulls.**

httpredirector supports hostname white listing to prevent unathorised useage.

## Running the image

* To force SSL you need to pass the `FORCE_SSL` environment variable to Docker
* To force `WWW` redirect you need to pass the `FORCE_WWW` environment variable to Docker
* To restrict hosts set the `VALID_HOSTS` environment variable with a comma serperated list of host names. Example: `VALID_HOSTS="www.example.com,example.com"`

Example usage which exposes the redirection on port 80

```
docker run --rm -ti \
  -e FORCE_SSL=TRUE \
  -e FORCE_WWW=TRUE \
  -p 80:8080 \
  q4uw/httpredirector:0.0.1
```

Verify it is working

```
$ curl -I localhost

HTTP/1.1 307 Temporary Redirect
Date: Sat, 10 Dec 2016 07:19:01 GMT
Server: Warp/3.2.9
Location: https://www.localhost/
```


## Building from source
Check out the project

```
git clone git@github.com:ajevans85/httpredirector.git
```

### Create a static compiled binary

To create a staticaly compiled binary we use Docker.

From the project directory run the below command.

```
docker run \
  --rm \
  -v $(pwd):/usr/src/build \
  -v ${HOME}/.stack:/root/.stack \
  -w /usr/src/build \
  -it \
  q4uw/haskell_build_env:0.0.1
```

This will:
* Give you a bash prompt in the container
* Mount your project directory in the docker container
* Mount your local `stack` home in the container to make use of artifact caching
* Set the working directory to the project root

To staticly compile the project run the below command which will place the binary in the `./dist` directory.

```
stack install \
  --local-bin-path /usr/src/build/dist \
  --ghc-options '-optl-static -fPIC -optc-Os'
```

Next compress the binary using `UPX`. This significantly reduces the size.

```
upx --best --ultra-brute dist/httpredirector-exe
```

You can now exit the shell.

### Package the binary as a docker image

```
docker build --tag q4uw/httpredirector .
```
