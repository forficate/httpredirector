CONTAINER=docker run --rm -v $(PWD):/usr/src/build -v ${HOME}/.stack:/root/.stack -w /usr/src/build -it q4uw/haskell_build_env:0.0.1

clean:
	$(CONTAINER) stack clean
	rm -rf $(PWD)/dist

dist:
	$(CONTAINER) stack install --local-bin-path /usr/src/build/dist --ghc-options '-optl-static -fPIC -optc-Os'
	$(CONTAINER) upx --best --ultra-brute dist/httpredirector-exe

docker-image:
	docker build --tag https-redirector/httpredirector .
