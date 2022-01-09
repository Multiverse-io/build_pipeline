defmodule BuildPipeline.ConfigFile do
  alias BuildPipeline.Result

  @build_step_keys %{
    build_step_name: "buildStepName",
    command: "command",
    depends_on: "dependsOn",
    command_type: "commandType"
  }

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
        build_step -> {:cont, {:ok, [build_step | tree]}}
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

  defp build_build_step(build_step, order) do
    parsed_build_step = %{
      build_step_name: Map.get(build_step, "buildStepName"),
      command: Map.get(build_step, "command"),
      depends_on: Map.get(build_step, "dependsOn"),
      command_type: Map.get(build_step, "commandType"),
      order: order
    }

    case command_env_vars(build_step["envVars"]) do
      {:ok, command_env_vars} ->
        parsed_build_step = Map.put(parsed_build_step, :command_env_vars, command_env_vars)

        parsed_build_step
        |> Enum.reduce_while({:ok, parsed_build_step}, fn
          {build_step_key, nil}, _ ->
            {:halt,
             {:error,
              "I failed to parse the build_pipeline_config because a build step was missing the key '#{Map.fetch!(@build_step_keys, build_step_key)}'"}}

          {_build_step_key, _}, _ ->
            {:cont, {:ok, parsed_build_step}}
        end)
        |> Result.and_then(fn %{depends_on: depends_on} = parsed_build_step ->
          if is_list(depends_on) do
            Map.put(parsed_build_step, :depends_on, MapSet.new(depends_on))
          else
            {:error,
             "I failed to parse the build_pipeline_config because a build step had a non-list dependsOn of '#{depends_on}'"}
          end
        end)

      {:error, error} ->
        {:error, error}
    end
  end

  defp command_env_vars(nil) do
    {:ok, []}
  end

  defp command_env_vars(command_env_vars) when is_list(command_env_vars) do
    Enum.reduce_while(command_env_vars, {:ok, []}, fn
      %{"name" => env_var_name, "value" => env_var_value}, {:ok, acc} ->
        {:cont, {:ok, [{to_charlist(env_var_name), to_charlist(env_var_value)} | acc]}}

      invalid, _acc ->
        {:halt, {:error, command_env_vars_error_msg(invalid)}}
    end)
  end

  defp command_env_vars(invalid) do
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
