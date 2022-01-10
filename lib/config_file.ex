defmodule BuildPipeline.ConfigFile do
  alias BuildPipeline.Result

  @command_types %{
    "shellCommand" => :shell_command
  }

  # TODO add test to assert all build_step_name's are unique
  # TODO add test to assert all build_step_name dependsOn exist
  def read(%{cwd: cwd} = setup) do
    file_location = "#{cwd}/build_pipeline/config.json"

    case File.read(file_location) do
      {:ok, config_file_contents} -> {:ok, {config_file_contents, setup}}
      _ -> {:error, {:config_file_not_found, file_location}}
    end
  end

  def parse_and_validate({config_file_contents, setup}) do
    config_file_contents
    |> Jason.decode()
    |> Result.and_then(&build_build_pipeline_tree/1)
    |> Result.and_then(&{:ok, %{build_pipeline: &1, setup: setup}})
  end

  defp build_build_pipeline_tree(json) do
    json
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {action, order}, {:ok, tree} ->
      case build_build_step(action, order) do
        {:error, error} -> {:halt, {:error, {:invalid_config, error}}}
        {:ok, build_step} -> {:cont, {:ok, [build_step | tree]}}
      end
    end)
    |> Result.and_then(&validate_command_types/1)
    |> Result.and_then(&validate_depends_on_exist/1)
  end

  defp validate_depends_on_exist(tree) do
    all_build_step_names = tree |> Enum.map(& &1.build_step_name) |> MapSet.new()

    tree
    |> Enum.find(fn %{depends_on: depends_on} ->
      depends_on = MapSet.new(depends_on)
      not MapSet.subset?(depends_on, all_build_step_names)
    end)
    |> case do
      nil ->
        {:ok, tree}

      %{build_step_name: build_step_name} ->
        {:error,
         {:invalid_config,
          "I failed to parse the build_pipeline_config because the build step named '#{build_step_name}' has a 'dependsOn' build step name that was not found"}}
    end
  end

  @simple_steps [
    {:build_step_name, "buildStepName"},
    {:command, "command"},
    {:command_type, "commandType"}
  ]

  defp build_build_step(json, order) do
    initial_build_step = {:ok, %{order: order}}

    @simple_steps
    |> Enum.reduce_while(initial_build_step, fn {step_key, json_key}, {:ok, build_step} ->
      case add_simple_step(build_step, step_key, json_key, json) do
        {:ok, build_step} ->
          {:cont, {:ok, build_step}}

        {:error, error} ->
          {:halt, {:error, error}}
      end
    end)
    |> Result.and_then(fn build_step -> add_command_env_vars(build_step, json) end)
    |> Result.and_then(fn build_step -> add_depends_on(build_step, json) end)
  end

  defp add_command_env_vars(build_step, json) do
    json
    |> Map.get("envVars")
    |> parse_command_env_vars()
    |> case do
      {:ok, env_vars} ->
        {:ok, Map.put(build_step, :command_env_vars, env_vars)}

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_depends_on(build_step, json) do
    json
    |> Map.get("dependsOn")
    |> case do
      nil ->
        {:error,
         "I failed to parse the build_pipeline_config because a build step was missing the key 'dependsOn'"}

      depends_on when is_list(depends_on) ->
        {:ok, Map.put(build_step, :depends_on, MapSet.new(depends_on))}

      depends_on ->
        {:error,
         "I failed to parse the build_pipeline_config because a build step had a non-list dependsOn of '#{depends_on}'"}
    end
  end

  defp add_simple_step(build_step, step_key, json_key, json) do
    case Map.get(json, json_key) do
      nil ->
        {:error,
         "I failed to parse the build_pipeline_config because a build step was missing the key '#{json_key}'"}

      build_step_value ->
        {:ok, Map.put(build_step, step_key, build_step_value)}
    end
  end

  defp parse_command_env_vars(nil) do
    {:ok, []}
  end

  defp parse_command_env_vars(command_env_vars) when is_list(command_env_vars) do
    Enum.reduce_while(command_env_vars, {:ok, []}, fn
      %{"name" => env_var_name, "value" => env_var_value}, {:ok, acc} ->
        {:cont, {:ok, [{to_charlist(env_var_name), to_charlist(env_var_value)} | acc]}}

      invalid, _acc ->
        {:halt, {:error, command_env_vars_error_msg(invalid)}}
    end)
  end

  defp parse_command_env_vars(invalid) do
    {:error, command_env_vars_error_msg(invalid)}
  end

  defp command_env_vars_error_msg(invalid) do
    ~s|I failed to parse the build_pipeline_config because a built step had bad envVars of #{inspect(invalid)}. They should be in the form "envVars": [{"name": "MIX_ENV", "value": "test"}]|
  end

  defp validate_command_types(tree) do
    Enum.reduce_while(tree, {:ok, []}, fn build_step, {:ok, acc} ->
      command_type = Map.fetch!(build_step, :command_type)

      case Map.get(@command_types, command_type) do
        nil ->
          {:halt,
           {:error,
            {:invalid_config,
             "I failed to parse the build_pipeline_config because a build step had an invalid commandType of '#{command_type}'"}}}

        valid_command_type ->
          {:cont, {:ok, [Map.put(build_step, :command_type, valid_command_type) | acc]}}
      end
    end)
  end
end
