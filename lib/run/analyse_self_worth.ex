defmodule BuildPipeline.Run.AnalyseSelfWorth do
  alias BuildPipeline.Run
  alias BuildPipeline.Run.{PrettyDurationMessage, Result}

  # TODO speed up the run tests by parallelising them, they're too slow with async: false now!
  # use mimic to set the terminal output stuff instead of an system wide Application.get_env thing so that async: true can be turned on!
  # TODO when running with debug, add up all the times of the individual build_steps instead. that would give a more realistic non-build_pipeline time. compare that to the build_pipeline time with parallelism!
  def run(command_line_args) do
    {:ok, %{}}
    |> Result.and_then(fn timings -> time_build_pipeline_run(command_line_args, timings) end)
    |> Result.and_then(fn timings -> time_serial_run(command_line_args, timings) end)
    |> case do
      {:ok, %{build_pipeline: build_pipeline, serially: without_build_pipeline}} ->
        puts("""
        *********************************************************************
        Self Wort Analysis
        *********************************************************************

        build_pipeline runtime = #{PrettyDurationMessage.create(build_pipeline)}
        serial runtime = #{PrettyDurationMessage.create(without_build_pipeline)}

        #{self_worth_msg(build_pipeline, without_build_pipeline)}
        *********************************************************************
        """)

        Run.exit_with_code(0)
        {:ok, :noop}

      _ ->
        Run.exit_with_code(1)
    end
  end

  defp self_worth_msg(build_pipeline, without_build_pipeline) do
    diff = without_build_pipeline - build_pipeline

    cond do
      diff > 0 ->
        "I made things faster to the tune of #{PrettyDurationMessage.create(diff)} !\nSelf worth affirmed!"

      diff < 0 ->
        "I made things slower by #{PrettyDurationMessage.create(diff)}. \nThis is a sad day for me"

      diff == 0 ->
        "I made no difference? \nHow unsatisfying"
    end
  end

  defp time_serial_run(command_line_args, timings) do
    debug_run_result =
      Run.main(command_line_args, fn setup ->
        setup
        |> put_in([:setup, :halt_when_done], false)
        |> put_in([:setup, :mode], :debug)
      end)

    case debug_run_result do
      {:ok, %{result: :success, build_pipeline: build_pipeline}} ->
        runtime =
          Enum.reduce(build_pipeline, 0, fn %{duration_in_microseconds: duration_in_microseconds},
                                            runtime ->
            runtime + duration_in_microseconds
          end)

        {:ok, Map.put(timings, :serially, runtime)}

      _ ->
        IO.inspect("1")
        :error
    end
  end

  defp time_build_pipeline_run(command_line_args, timings) do
    start_time = DateTime.utc_now()
    run_result = System.cmd(bp_binary(), ["run" | command_line_args])

    {output, exit_code} = run_result

    if exit_code == 0 do
      end_time = DateTime.utc_now()
      timing = DateTime.diff(end_time, start_time, :microsecond)
      {:ok, Map.put(timings, :build_pipeline, timing)}
    else
      puts("Couldn't analyse self worth because a run failed!")
      puts(output)
      :error
    end
  end

  # TODO more sophisticated run here, e.g. if in test mode run binary X, otherwise Y. not done properly here
  defp bp_binary do
    if Application.get_env(:build_pipeline, :env) == :test do
      "./bp"
    else
      "bp"
    end
  end

  defp puts(message) do
    if should_print_runner_output?() do
      IO.puts(message)
    end
  end

  defp should_print_runner_output? do
    Application.get_env(:build_pipeline, :print_runner_output, true)
  end
end
