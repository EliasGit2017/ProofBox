#! bin/sh
make db-downgrade;

make all;

./bin/api-server config/api_config.json
