#!/bin/bash
echo "Setting up config for environment: $NODE_ENV"
if [ "$NODE_ENV" = "development" ]; then
  cp ./src/config/config.dev.js ./src/config/config.js
else
  cp ./src/config/config.prd.js ./src/config/config.js
fi