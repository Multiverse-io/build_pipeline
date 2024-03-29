defmodule BuildPipeline.Run.ConfigFile do
  alias BuildPipeline.Run.Result

  @moduledoc false
  @command_types %{
    "shellCommand" => :shell_command,
    "script" => :script
  }

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
    |> Result.and_then(&build_build_pipeline/1)
    |> Result.and_then(&{:ok, %{build_pipeline: &1, setup: setup}})
    |> case do
      {:error, %Jason.DecodeError{} = error} -> {:error, {:invalid_config, error}}
      other -> other
    end
  end

  defp build_build_pipeline(json) do
    json
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {action, order}, {:ok, build_pipeline} ->
      case build_build_step(action, order) do
        {:error, error} -> {:halt, {:error, {:invalid_config, error}}}
        {:ok, build_step} -> {:cont, {:ok, [build_step | build_pipeline]}}
      end
    end)
    |> Result.and_then(&validate_command_types/1)
    |> Result.and_then(&validate_build_step_names_are_unique/1)
    |> Result.and_then(&validate_depends_on/1)
  end

  @simple_steps [
    {:build_step_name, "buildStepName"},
    {:command, "command"},
    {:command_type, "commandType"}
  ]

  defp build_build_step(json, order) do
    initial_build_step = {:ok, %{order: order, status: :incomplete}}

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

  defp validate_command_types(build_pipeline) do
    Enum.reduce_while(build_pipeline, {:ok, []}, fn build_step, {:ok, acc} ->
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

  defp validate_build_step_names_are_unique(build_pipeline) do
    build_pipeline
    |> Enum.reduce_while(MapSet.new(), fn %{build_step_name: name}, names ->
      if MapSet.member?(names, name) do
        {:halt, {:error, {:invalid_config, duplicate_build_step_name_error(name)}}}
      else
        {:cont, MapSet.put(names, name)}
      end
    end)
    |> case do
      {:error, error} -> {:error, error}
      _ -> {:ok, build_pipeline}
    end
  end

  defp validate_depends_on(build_pipeline) do
    build_pipeline
    |> Map.new(&Map.pop!(&1, :build_step_name))
    |> deps_tree()
    |> Result.and_then(fn -> {:ok, build_pipeline} end)
  end

  defp deps_tree(pipeline), do: deps_tree([], Map.to_list(pipeline), pipeline)

  defp deps_tree({:error, error}, [], _pipeline) do
    {:error, error}
  end

  defp deps_tree(_, [], _pipeline) do
    :ok
  end

  defp deps_tree(tree, [{name, _} | rest], pipeline) do
    case deps_branch(name, name, [name], pipeline) do
      {:error, error} -> {:error, error}
      branch -> deps_tree([{name, branch} | tree], rest, pipeline)
    end
  end

  defp deps_branch(root, dep, branch, pipeline) do
    case Map.get(pipeline, dep) do
      nil ->
        no_depedency_error(dep)

      %{depends_on: child_deps} ->
        child_deps = MapSet.to_list(child_deps)

        root
        |> circular_depencency_check(branch, child_deps)
        |> Result.and_then(fn ->
          Enum.reduce_while(child_deps, branch, fn child_dep, branch ->
            case deps_branch(root, child_dep, branch, pipeline) do
              {:error, error} -> {:halt, {:error, error}}
              new_branch -> {:cont, new_branch ++ branch}
            end
          end)
        end)
    end
  end

  defp circular_depencency_check(root, branch, nodes) do
    Enum.reduce_while(nodes, :ok, fn node, _ ->
      if Enum.member?(branch, node) do
        {:halt, circular_reference_error(root, node)}
      else
        {:cont, :ok}
      end
    end)
  end

  defp duplicate_build_step_name_error(name) do
    ~s|I failed to parse the build_pipeline_config because a the buildStepName "#{name}" was duplicated, but buildStepNames must be unique|
  end

  defp circular_reference_error(name_1, name_2) do
    {:error,
     {:invalid_config,
      "I failed to parse the build_pipeline_config because I found a circular dependency: #{name_1} <-> #{name_2}"}}
  end

  defp no_depedency_error(depends_on) do
    {:error,
     {:invalid_config,
      "I failed to parse the build_pipeline_config because a build step had a dependsOn of '#{depends_on}' that does not exist"}}
  end
end
