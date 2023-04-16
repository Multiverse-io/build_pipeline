defmodule BuildPipeline.Run.AnalyseSelfWorth do
  alias BuildPipeline.Run
  alias BuildPipeline.Run.{PrettyDurationMessage, Result}

  # TODO speed up the run tests by parallelising them, they're too slow with async: false now!
  # use mimic to set the terminal output stuff instead of an system wide Application.get_env thing so that async: true can be turned on!
  # TODO when running with debug, add up all the times of the individual build_steps instead. that would give a more realistic non-build_pipeline time. compare that to the build_pipeline time with parallelism!
  def run(command_line_args) do
    {:ok, %{}}
    |> Result.and_then(fn timings ->
      time_run(command_line_args, :build_pipeline, timings)
    end)
    |> Result.and_then(fn timings ->
      # time_run(command_line_args ++ ["--debug"], :serially, timings)
      debug_run(command_line_args)
    end)
    |> case do
      {:ok, %{build_pipeline: build_pipeline, serially: without_build_pipeline}} ->
        puts("""
        build_pipeline runtime with parallelism = #{PrettyDurationMessage.create(build_pipeline)}
        build_pipeline runtime without parallelism = #{PrettyDurationMessage.create(without_build_pipeline)}
        """)

        Run.exit_with_code(0)
        {:ok, :noop}

      _ ->
        puts("Failed!")
        Run.exit_with_code(1)
    end
  end

  defp debug_run(command_line_args) do
    Run.main(command_line_args, fn setup ->
      put_in(setup, [:setup, :halt_when_done], false)
    end)
    |> IO.inspect()

    raise "no"
  end

  defp time_run(command_line_args, run_name, timings) do
    start_time = DateTime.utc_now()
    run_result = System.cmd(bp_binary(), ["run" | command_line_args])

    {_, exit_code} = run_result

    if exit_code == 0 do
      end_time = DateTime.utc_now()
      timing = DateTime.diff(end_time, start_time, :microsecond)
      {:ok, Map.put(timings, run_name, timing)}
    else
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
