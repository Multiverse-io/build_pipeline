defmodule BuildPipeline.Run.Support.EnvVarsSystemMock do
  alias Mimic
  alias BuildPipeline.Run.Const

  @save_result Const.save_result_env_var_name()
  @from_failed Const.from_failed_env_var_name()

  def setup(opts \\ []) do
    Mimic.copy(System)

    Mimic.stub(System, :get_env, fn
      @save_result -> Keyword.get(opts, :save_result, nil)
      @from_failed -> Keyword.get(opts, :from_failed, nil)
    end)
  end
end
