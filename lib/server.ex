defmodule BuildPipeline.Server do
  use GenServer
  alias BuildPipeline.{BuildStepRunner, TerminalPrinter, TerminalMessages}

  @moduledoc false
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
    %{
      build_pipeline: build_pipeline,
      setup: %{mode: mode, cwd: cwd, terminal_width: terminal_width}
    } = setup

    runners = init_waiting_runners(build_pipeline, cwd)

    state = %{
      runners: runners,
      parent_pid: parent_pid,
      terminal_width: terminal_width,
      mode: mode
    }

    state
    |> TerminalMessages.pending()
    |> TerminalPrinter.runner_update(state)

    {:ok, state, {:continue, :start_runners}}
  end

  @impl true
  def handle_continue(:start_runners, state) do
    start_runners_if_able(state)
    {:noreply, state}
  end

  @impl true
  def handle_cast(:start_runners, state) do
    start_runners_if_able(state)
    {:noreply, state}
  end

  def handle_cast({:runner_finished, runner_pid, result}, state) do
    # IO.inpect result
    state
    |> update_completed_runners(runner_pid, result)
    |> continue_unless_step_failed(runner_pid)
  end

  # https://stackoverflow.com/questions/11283625/overwrite-last-line-on-terminal
  def handle_cast({:runner_starting, runner_pid}, state) do
    state
    |> TerminalMessages.running(runner_pid)
    |> TerminalPrinter.runner_update(state)

    {:noreply, state}
  end

  @impl true
  def terminate(:normal, %{parent_pid: parent_pid} = state) do
    send(parent_pid, {:server_done, parse_result(state)})
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
      %{exit_code: 0} = result ->
        state
        |> TerminalMessages.succeeded(result, runner_pid)
        |> TerminalPrinter.runner_update(state)

        state
        |> start_runners_if_able()
        |> finished_if_all_runners_done()

      %{exit_code: _non_zero} = result ->
        state
        |> TerminalMessages.failed(result, runner_pid)
        |> TerminalPrinter.runner_update(state)

        state
        |> TerminalMessages.abort()
        |> TerminalPrinter.runner_update(state)

        state
        |> TerminalMessages.failed_output(result)
        |> TerminalPrinter.runner_update(state)

        {:stop, :normal, state}
    end
  end

  defp finished_if_all_runners_done(state) do
    if Enum.all?(state.runners, fn {_runner_pid, %{status: status}} -> status == :complete end) do
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
      {_runner_pid, %{status: :complete}} -> true
      _ -> false
    end)
    |> MapSet.new(fn {_runner_pid, %{build_step_name: build_step_name}} -> build_step_name end)
  end

  defp init_waiting_runners(build_pipeline, cwd) do
    Enum.reduce(build_pipeline, %{}, fn build_step, runners ->
      {:ok, runner_pid} = BuildStepRunner.start_link(build_step, self(), cwd)

      build_step =
        build_step
        |> Map.put(:status, :incomplete)
        |> Map.put(:terminal_line_number, build_step.order + 1)

      Map.put(runners, runner_pid, build_step)
    end)
  end
end
