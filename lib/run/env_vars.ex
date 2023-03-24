defmodule BuildPipeline.Run.EnvVars do
  alias BuildPipeline.Run.Const

  @save_result Const.save_result_env_var_name()
  @from_failed Const.from_failed_env_var_name()

  def read do
    Enum.reduce_while(decoders(), {:ok, %{}}, fn decoder, {:ok, setup} ->
      case decoder.(setup) do
        {:ok, setup} -> {:cont, {:ok, setup}}
        error -> {:halt, error}
      end
    end)
  end

  defp decoders, do: [&from_failed/1, &save_result/1]

  defp save_result(setup) do
    case System.get_env(@save_result) do
      "true" -> {:ok, put_save_result(setup, true)}
      "false" -> {:ok, put_save_result(setup, false)}
      nil -> {:ok, setup}
      bad -> {:error, {:load_env_vars, error_message(@save_result, bad)}}
    end
  end

  defp from_failed(setup) do
    case System.get_env(@from_failed) do
      "true" -> {:ok, put_save_result_and_from_failed(setup, true)}
      "false" -> {:ok, put_save_result_and_from_failed(setup, false)}
      nil -> {:ok, setup}
      bad -> {:error, {:load_env_vars, error_message(@from_failed, bad)}}
    end
  end

  defp put_save_result(%{save_result: true} = setup, _save_result) do
    setup
  end

  defp put_save_result(setup, save_result) do
    Map.put(setup, :save_result, save_result)
  end

  defp put_save_result_and_from_failed(setup, value) do
    setup
    |> Map.put(:save_result, value)
    |> Map.put(:run_from_failed, value)
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
