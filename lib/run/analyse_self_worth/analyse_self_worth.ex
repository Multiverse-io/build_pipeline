defmodule BuildPipeline.Run.AnalyseSelfWorth do
  alias BuildPipeline.Run
  alias BuildPipeline.Run.{PrettyDurationMessage, Result}
  alias BuildPipeline.Run.AnalyseSelfWorth.{BuildPipelineRun, SerialRun}

  def run(command_line_args) do
    {:ok, %{}}
    |> Result.and_then(fn timings -> time_build_pipeline_run(command_line_args, timings) end)
    |> Result.and_then(fn timings -> time_serial_run(command_line_args, timings) end)
    |> case do
      {:ok, %{build_pipeline_in_microseconds: build_pipeline, serially_in_microseconds: serially}} ->
        IO.puts("""
        *********************************************************************
        Self Worth Analysis
        *********************************************************************

        build_pipeline runtime = #{PrettyDurationMessage.create(build_pipeline)}
        serial runtime = #{PrettyDurationMessage.create(serially)}

        #{self_worth_msg(build_pipeline, serially)}
        *********************************************************************
        """)

        Run.exit_with_code(0)

        {:ok,
         %{build_pipeline_in_microseconds: build_pipeline, serially_in_microseconds: serially}}

      {:error, error} ->
        IO.puts("Failed to analyse self worth because the run failed:\n#{inspect(error)}")
        Run.exit_with_code(1)

      _error ->
        IO.puts("Failed to analyse self worth because the run failed")
        Run.exit_with_code(1)
    end
  end

  defp self_worth_msg(build_pipeline, serially) do
    diff = serially - build_pipeline

    cond do
      diff > 0 ->
        "I made things faster to the tune of #{PrettyDurationMessage.create(diff)} !\nSelf worth affirmed!"

      diff < 0 ->
        "I made things slower by #{PrettyDurationMessage.create(-diff)}. \nThis is a sad day for me"

      diff == 0 ->
        "I made no difference? \nHow unsatisfying"
    end
  end

  defp time_serial_run(command_line_args, timings) do
    debug_run_result = SerialRun.run(command_line_args)

    case debug_run_result do
      {:ok, %{result: :success, build_pipeline: build_pipeline}} ->
        runtime =
          Enum.reduce(build_pipeline, 0, fn build_step, runtime ->
            build_step.duration_in_microseconds + runtime
          end)

        {:ok, Map.put(timings, :serially_in_microseconds, runtime)}

      _ ->
        :error
    end
  end

  defp time_build_pipeline_run(command_line_args, timings) do
    case BuildPipelineRun.run(command_line_args) do
      {:ok, timing} -> {:ok, Map.put(timings, :build_pipeline_in_microseconds, timing)}
      error -> error
    end
  end
end
