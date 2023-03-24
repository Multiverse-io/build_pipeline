defmodule BuildPipeline.Run.EnvVars do
  alias BuildPipeline.Run.Const

  @save_result_env_var_name Const.save_result_env_var_name()

  def put_config(config) do
    case System.get_env(@save_result_env_var_name) do
      "true" -> {:ok, update_config(config, :save_result, true)}
      "false" -> {:ok, update_config(config, :save_result, false)}
      nil -> {:ok, config}
      bad -> {:error, {:load_env_vars, error_message(bad)}}
    end
  end

  defp error_message(bad_env_var_value) do
    """
    The loaded environment variable was
      #{@save_result_env_var_name}=#{bad_env_var_value}
    but I only accept the values
      true
      false
    """
  end

  defp update_config(config, key, env_var_value) do
    Map.update(config, key, env_var_value, fn old_value -> old_value or env_var_value end)
  end
end
