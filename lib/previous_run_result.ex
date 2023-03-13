defmodule BuildPipeline.PreviousRunResult do
  alias BuildPipeline.{Const, Result}

  @skipable_results Const.skipable_results()
  @unskipable_results Const.unskipable_results()

  # TODO write tests for these things
  def read(%{setup: %{run_from_failed: false}} = config) do
    {:ok, config}
  end

  def read(%{setup: %{run_from_failed: true, cwd: cwd}} = config) do
    file_location = Path.join([cwd, "build_pipeline", "previous_run_result.json"])

    case File.read(file_location) do
      {:ok, file_contents} -> {:ok, {file_contents, config}}
      _ -> {:error, {:previous_run_result_file_not_found, file_location}}
    end
  end

  def parse_and_validate(%{setup: %{run_from_failed: false}} = config) do
    {:ok, config}
  end

  def parse_and_validate({file_contents, %{build_pipeline: build_pipeline} = config}) do
    file_contents
    |> Jason.decode()
    |> Result.and_then(fn previous_run_result ->
      update_build_pipeline(build_pipeline, previous_run_result)
    end)
    |> Result.and_then(&{:ok, Map.put(config, :build_pipeline, &1)})
  end

  defp update_build_pipeline(build_pipeline, previous_run_result) do
    build_pipeline = build_pipeline_to_map(build_pipeline)

    previous_run_result
    |> Enum.reduce_while(
      {:ok, build_pipeline},
      fn prevously_run_build_step, {:ok, acc} ->
        case prevously_run_build_step do
          %{"buildStepName" => step_name, "result" => result} ->
            update_build_step(acc, step_name, result)

          _ ->
            {:halt, {:error, {:previous_run_result, "blah"}}}
        end
      end
    )
    |> Result.and_then(&build_pipeline_to_list/1)
  end

  defp update_build_step(build_pipeline, step_name, result) when result in @skipable_results do
    put_build_step_skip(build_pipeline, step_name, true)
  end

  defp update_build_step(build_pipeline, step_name, result) when result in @unskipable_results do
    put_build_step_skip(build_pipeline, step_name, false)
  end

  defp update_build_step(_build_pipeline, _step_name, _result) do
    {:halt, {:error, {:previous_run_result, "blah"}}}
  end

  defp put_build_step_skip(build_pipeline, step_name, skip) do
    case Map.get(build_pipeline, step_name) do
      nil -> {:halt, {:error, {:previous_run_result, "blah"}}}
      _build_step -> {:cont, {:ok, put_in(build_pipeline, [step_name, :skip], skip)}}
    end
  end

  defp build_pipeline_to_map(build_pipeline) do
    Map.new(build_pipeline, fn build_step -> Map.pop!(build_step, :build_step_name) end)
  end

  defp build_pipeline_to_list(build_pipeline) do
    build_pipeline =
      build_pipeline
      |> Enum.sort_by(&(&1 |> elem(1) |> Map.fetch!(:order)), &<=/2)
      |> Enum.map(fn {step_name, build_step} ->
        Map.put(build_step, :build_step_name, step_name)
      end)

    {:ok, build_pipeline}
  end
end
