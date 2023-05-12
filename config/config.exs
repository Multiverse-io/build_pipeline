import Config

config :build_pipeline, print_runner_output: true
config :build_pipeline, env: config_env()
config :build_pipeline, bp_binary: "bp"

import_config "#{Mix.env()}.exs"
