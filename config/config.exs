import Config

config :build_pipeline, print_runner_output: true

import_config "#{Mix.env()}.exs"
