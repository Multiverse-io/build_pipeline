defmodule BuildPipeline.Run.Support.EnvVarsSystemMock do
  alias Mimic
  alias BuildPipeline.Run.Const

  @from_failed Const.from_failed_env_var_name()

  def setup(opts \\ []) do
    Mimic.copy(System)
    Mimic.stub(System, :get_env, fn @from_failed -> Keyword.get(opts, :from_failed, nil) end)
  end
end
