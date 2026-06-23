.PHONY: build clean shell

build:
	docker compose run --rm latex

clean:
	rm -rf build

shell:
	docker compose run --rm --entrypoint /bin/bash latex
