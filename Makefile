PROJECT_NAME:=proofbox
DATABASE:=proofbox
WEB_HOST:=http://localhost:8888
API_HOST:=http://localhost:8080
API_PORT:=8080
RLS_DIR:=www
CONTACT_EMAIL:=elias.bendjaballah@gmail.com
VERSION:=1.0

-include Makefile.config

.EXPORT_ALL_VARIABLES:

PGDATABASE=$(DATABASE)

all: clean-tmp build website api-server openapi

build_dock_arch :
	docker-compose -f ./scripts/Containers/Docker_arch/docker-compose.yml --compatibility up

tear_down_dock_arch :
	docker-compose -f ./scripts/Containers/Docker_arch/docker-compose.yml --compatibility down

test_dock :
	./_build/default/backend_arch/docker_comm.exe 

db-updater:
	@dune build src/db/db-update

config/db-version.txt:
	@mkdir -p config
	@echo 0 > config/db-version.txt

manual-db-reset:
	@echo "Manually fixing version index"
	@$(shell psql $(PGDATABASE) -c "UPDATE ezpg_info SET value = 0 where name ='version'" -t -A)

db-update: config/db-version.txt db-updater
	@_build/default/src/db/db-update/db_updater.exe --witness config/db-version.txt --database $(PGDATABASE)

db-downgrade: db-updater
	rm -rf scripts/Containers/storage/*
	$(eval DBVERSION := $(shell psql $(PGDATABASE) -c "select value from ezpg_info where name='version'" -t -A))
	_build/default/src/db/db-update/db_updater.exe --allow-downgrade --database $(PGDATABASE) --target `expr $(DBVERSION) - 1`

db-version:
	psql $(PGDATABASE) -c "select value from ezpg_info where name='version'" -t -A

see-db-types:
	@echo "Displaying $(PGDATABASE) types :"
	@echo "users"
	@echo "------------------------------------------"
	psql $(PGDATABASE) -c "select column_name , data_type from information_schema.columns where table_name = 'users'"
	@echo "------------------------------------------"
	@echo "jobs_description"
	@echo "------------------------------------------"
	psql $(PGDATABASE) -c "select column_name , data_type from information_schema.columns where table_name = 'jobs_description'"
	@echo "------------------------------------------"
	@echo "jobs_cache"
	@echo "------------------------------------------"
	psql $(PGDATABASE) -c "select column_name , data_type from information_schema.columns where table_name = 'jobs_cache'"

build: db-update
	dune build --profile release

website: config/info.json
	@mkdir -p www
	@cp -f _build/default/src/ui/main_ui.bc.js www/$(PROJECT_NAME)-ui.js
	@rsync -ar static/* www
	@cp config/info.json www
	@sed -i 's/%{project_name}/$(PROJECT_NAME)/g' www/index.html

api-server: _build/default/src/api/api_server.exe
	@mkdir -p bin
	@cp -f _build/default/src/api/api_server.exe bin/api-server
	@cp -f _build/default/src/ui/testing_reqs.exe bin/testing_reqs

release:
	@sudo cp -r www/* $(RLS_DIR)

clean:
	@dune clean

clean-tmp:
	rm -rf writters_draft/*.txt

install:
	@dune install

build-deps:
	@opam install --deps-only .

config/info.json config/api_config.json:
	@mkdir -p config
	@echo "{\"apis\": [\"$(API_HOST)\"]}" > config/info.json
	@echo "{\"port\": $(API_PORT)}" > config/api_config.json

init: build-deps config

git-init:
	rm -rf .git
	git init

openapi: _build/default/src/api/doc/openapi.exe
	@_build/default/src/api/doc/openapi.exe --version $(VERSION) --title "$(PROJECT_NAME) API" --contact "$(CONTACT_EMAIL)" --servers "api" $(API_HOST) -o www/openapi.json
