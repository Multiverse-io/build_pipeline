import Config

config :build_pipeline, print_runner_output: true
config :build_pipeline, env: config_env()

import_config "#{Mix.env()}.exs"
