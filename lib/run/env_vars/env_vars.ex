defmodule BuildPipeline.Run.EnvVars do
  alias BuildPipeline.Run.Const
  alias BuildPipeline.Run.EnvVars.FetchEnvVars

  @from_failed Const.from_failed_env_var_name()

  def read do
    case FetchEnvVars.fetch(@from_failed) do
      "true" -> {:ok, save_result_and_from_failed(true)}
      "false" -> {:ok, save_result_and_from_failed(false)}
      nil -> {:ok, %{}}
      bad -> {:error, {:load_env_vars, error_message(@from_failed, bad)}}
    end
  end

  defp save_result_and_from_failed(value) do
    %{save_result: value, run_from_failed: value}
  end

  defp error_message(key, bad_value) do
    """
    The loaded environment variable was
      #{key}=#{bad_value}
    but I only accept the values
      true
      false
    """
  end
end
