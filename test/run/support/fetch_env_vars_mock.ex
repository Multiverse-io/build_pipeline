defmodule BuildPipeline.Run.Support.FetchEnvVarsMock do
  alias Mimic
  alias BuildPipeline.Run.EnvVars.FetchEnvVars

  def setup(opts \\ []) do
    Mimic.copy(FetchEnvVars)
    Mimic.stub(FetchEnvVars, :fetch, fn _ -> Keyword.get(opts, :from_failed, nil) end)
  end
end
