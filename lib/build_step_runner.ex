defmodule BuildPipeline.BuildStepRunner do
  use GenServer
  alias BuildPipeline.ShellCommandRunner
  @moduledoc false

  def start_link(build_step, server_pid, cwd, opts \\ []) do
    GenServer.start_link(__MODULE__, {build_step, server_pid, cwd, opts})
  end

  @impl true
  def init({build_step, server_pid, cwd, opts}) do
    {:ok,
     %{build_step: build_step, status: :waiting, server_pid: server_pid, opts: opts, cwd: cwd}}
  end

  # TODO delete this
  @impl true
  def handle_cast({:run_if_able, completed_runners}, state) do
    %{build_step: %{depends_on: depends_on}, status: status} = state

    if status == :waiting && MapSet.subset?(depends_on, completed_runners) do
      {:noreply, %{state | status: :running}, {:continue, :run}}
    else
      {:noreply, state}
    end
  end

  # TODO write a test
  # TODO don't update opts here, set them on initialisation only
  def handle_cast({:run_if_waiting, new_opts}, state) do
    if state.status == :waiting do
      opts = Keyword.merge(state.opts, new_opts)
      {:noreply, %{state | status: :running, opts: opts}, {:continue, :run}}
    else
      {:noreply, state}
    end
  end

  def handle_cast(:run, state) do
    handle_continue(:run, state)
  end

  def handle_cast({:update_status, status}, state) do
    {:noreply, %{state | status: status}}
  end

  @impl true
  def handle_continue(:run, state) do
    %{
      build_step: %{
        command_type: command_type,
        command: command,
        command_env_vars: command_env_vars
      },
      cwd: cwd
    } = state

    case command_type do
      :shell_command ->
        run_shell_command(command, command_env_vars, state)

      :script ->
        run_shell_command("#{cwd}/build_pipeline/scripts/#{command}", command_env_vars, state)
    end
  end

  @impl true
  def terminate(:normal, state) do
    GenServer.cast(state.server_pid, {:runner_finished, self(), state.result})
  end

  # TODO test write_as_you_go being passed in
  defp run_shell_command(command, command_env_vars, state) do
    print_cmd_output = Keyword.get(state.opts, :print_cmd_output, false)

    GenServer.cast(state.server_pid, {:runner_starting, self()})
    start_time = DateTime.utc_now()

    {output, exit_code} =
      ShellCommandRunner.run(command, command_env_vars, print_cmd_output: print_cmd_output)

    end_time = DateTime.utc_now()
    duration = DateTime.diff(end_time, start_time, :microsecond)
    result = %{output: output, exit_code: exit_code, duration_in_microseconds: duration}

    state =
      state
      |> Map.put(:result, result)
      |> Map.put(:status, :finished)

    {:stop, :normal, state}
  end
end
