defmodule BuildPipeline.Run.EnvVars do
  alias BuildPipeline.Run.Const

  @save_result_env_var_name Const.save_result_env_var_name()

  def put_config(config) do
    case save_result() do
      {:ok, nil} ->
        {:ok, config}

      {:ok, save_result} when is_boolean(save_result) ->
        {:ok, Map.put(config, :save_result, save_result)}

      _ ->
        raise "no"
        # {:ok, value} -> Map.put(setup, :save_result)
    end
  end

  defp save_result do
    case System.get_env(@save_result_env_var_name) do
      "true" -> {:ok, true}
      "false" -> {:ok, false}
      nil -> {:ok, nil}
      other -> {:error, other}
    end
  end
end
