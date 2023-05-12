defmodule BuildPipeline.Run.EnvVars.FetchEnvVars do
  def fetch(name), do: System.get_env(name)
end
