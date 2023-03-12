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

  @impl true
  def handle_cast(:run_if_waiting, state) do
    if state.status == :waiting do
      {:noreply, %{state | status: :running}, {:continue, :run}}
    else
      {:noreply, state}
    end
  end

  def handle_cast(:run, state) do
    handle_continue(:run, state)
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
