defmodule BuildPipeline.Server do
  use GenServer
  alias BuildPipeline.BuildStepRunner
  alias IO.ANSI

  @default_genserver_options []

  def child_spec(setup, parent_pid, genserver_options \\ @default_genserver_options) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [{setup, parent_pid}, genserver_options]},
      restart: :temporary
    }
  end

  def start_link({setup, parent_pid}, genserver_options \\ @default_genserver_options) do
    GenServer.start_link(__MODULE__, {setup, parent_pid}, genserver_options)
  end

  @impl true
  def init({setup, parent_pid}) do
    %{build_pipeline: build_pipeline} = setup

    runners = init_waiting_runners(build_pipeline)

    start_runners()

    {:ok,
     %{
       runners: runners,
       parent_pid: parent_pid,
       output_lines: put_pending_runners(runners)
     }}
  end

  @impl true
  def handle_cast(:start_runners, state) do
    start_runners_if_able(state)
    {:noreply, state}
  end

  # TODO prevent circular references in the JSON
  # TODO release to hex
  # TODO verbose mode (runs everything serially & outputs std_out and std_error live)
  # TODO show in the runner_output lines that runners have been aborted if any have failed
  def handle_cast({:runner_finished, runner_pid, result}, state) do
    state
    |> update_completed_runners(runner_pid, result)
    |> continue_unless_step_failed(runner_pid)
  end

  # https://stackoverflow.com/questions/11283625/overwrite-last-line-on-terminal
  def handle_cast({:runner_starting, runner_pid}, state) do
    command = state[:runners][runner_pid][:command]
    message = "#{ANSI.magenta()}#{command} [Running]#{ANSI.reset()}"

    output_lines = put_on_runner_output_line(state.output_lines, runner_pid, message)

    {:noreply, %{state | output_lines: output_lines}}
  end

  @impl true
  def terminate(:normal, %{parent_pid: parent_pid} = state) do
    send(parent_pid, {:server_done, parse_result(state)})
  end

  defp put_pending_runners(runners) do
    runners
    |> Enum.map(fn {pid, build_step} -> {pid, build_step} end)
    |> Enum.sort(fn {_, %{order: order_1}}, {_, %{order: order_2}} -> order_1 <= order_2 end)
    |> Map.new(fn {pid, %{order: order, command: command}} ->
      line_number = order + 1

      message = "#{ANSI.light_magenta()}#{command} [Pending]#{ANSI.reset()}"

      if should_print_runner_output?() do
        IO.puts(message)
      end

      {pid, %{line_number: line_number, content: message}}
    end)
  end

  defp put_on_runner_output_line(output_lines, runner_pid, message) do
    %{line_number: line_number} = Map.fetch!(output_lines, runner_pid)

    max_lines =
      Enum.reduce(output_lines, 0, fn {_pid, %{line_number: line_number}}, acc ->
        if line_number >= acc do
          line_number
        else
          acc
        end
      end)

    line_shift = max_lines - line_number + 1

    if should_print_runner_output?() do
      IO.write(
        "\r#{ANSI.cursor_up(line_shift)}\r#{ANSI.clear_line()}#{message}#{ANSI.cursor_down(line_shift)}\r"
      )
    end

    output_line = %{line_number: line_number, content: message}
    Map.put(output_lines, runner_pid, output_line)
  end

  defp parse_result(%{runners: runners}) do
    build_pipeline =
      runners
      |> Enum.sort(fn {_, %{order: order_1}}, {_, %{order: order_2}} -> order_1 <= order_2 end)
      |> Enum.map(fn {_, build_step} -> build_step end)

    failed? = Enum.any?(build_pipeline, fn build_step -> build_step.status == :incomplete end)

    if failed? do
      %{build_pipeline: build_pipeline, result: :failure}
    else
      %{build_pipeline: build_pipeline, result: :success}
    end
  end

  defp update_completed_runners(state, runner_pid, result) do
    runners =
      Map.update!(state.runners, runner_pid, fn runner ->
        runner
        |> Map.merge(result)
        |> Map.put(:status, :complete)
      end)

    %{state | runners: runners}
  end

  defp continue_unless_step_failed(state, runner_pid) do
    case Map.fetch!(state.runners, runner_pid) do
      %{exit_code: 0, duration_in_microseconds: duration_in_microseconds} ->
        duration =
          cond do
            duration_in_microseconds < 1000 ->
              "#{duration_in_microseconds} μs"

            duration_in_microseconds < 1_000_000 ->
              "#{round(duration_in_microseconds / 1000)} ms"

            duration_in_microseconds < 60_000_000 ->
              "#{Float.round(duration_in_microseconds / 1_000_000, 1)} s"

            true ->
              "#{round(duration_in_microseconds / 60_000_000)} min"
          end

        command = state[:runners][runner_pid][:command]
        message = "#{ANSI.green()}#{command} [Finished in #{duration}] ✔ #{ANSI.reset()}"

        output_lines = put_on_runner_output_line(state.output_lines, runner_pid, message)

        %{state | output_lines: output_lines}
        |> start_runners_if_able()
        |> finished_if_all_runners_done()

      %{exit_code: _non_zero} ->
        command = state[:runners][runner_pid][:command]
        message = "#{ANSI.red()}#{command} [Failed]#{ANSI.reset()}"

        output_lines =
          state.output_lines
          |> put_on_runner_output_line(runner_pid, message)
          |> put_aborted_for_incomplete_runners_msg(state.runners)

        {:stop, :normal, %{state | output_lines: output_lines}}
    end
  end

  defp put_aborted_for_incomplete_runners_msg(output_lines, runners) do
    Enum.reduce(runners, output_lines, fn
      {_runner_pid, %{status: :complete}}, acc ->
        acc

      {runner_pid, %{command: command}}, acc ->
        message = "#{ANSI.magenta()}#{ANSI.crossed_out()}#{command} [Aborted]#{ANSI.reset()}"
        put_on_runner_output_line(acc, runner_pid, message)
    end)
  end

  defp finished_if_all_runners_done(state) do
    all_runners_by_name =
      MapSet.new(state.runners, fn {_runner_pid, %{build_step_name: build_step_name}} ->
        build_step_name
      end)

    completed_runners_by_name = completed_runners_by_name(state.runners)

    if MapSet.equal?(all_runners_by_name, completed_runners_by_name) do
      {:stop, :normal, state}
    else
      {:noreply, state}
    end
  end

  defp start_runners_if_able(state) do
    completed_runners = completed_runners_by_name(state.runners)

    Enum.each(state.runners, fn {runner_pid, _build_step} ->
      GenServer.cast(runner_pid, {:run_if_able, completed_runners})
    end)

    state
  end

  defp completed_runners_by_name(runners) do
    runners
    |> Enum.filter(fn
      {_runner_pid, %{exit_code: _}} -> true
      _ -> false
    end)
    |> MapSet.new(fn {_runner_pid, %{build_step_name: build_step_name}} -> build_step_name end)
  end

  defp start_runners do
    server_pid = self()
    spawn_link(fn -> GenServer.cast(server_pid, :start_runners) end)
  end

  defp init_waiting_runners(build_pipeline) do
    Enum.reduce(build_pipeline, %{}, fn build_step, runners ->
      {:ok, runner_pid} = BuildStepRunner.start_link(build_step, self())

      build_step = Map.put(build_step, :status, :incomplete)
      Map.put(runners, runner_pid, build_step)
    end)
  end

  defp should_print_runner_output? do
    Application.get_env(:build_pipeline, :print_runner_output)
  end
end
