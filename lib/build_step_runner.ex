defmodule BuildPipeline.BuildStepRunner do
  use GenServer
  alias BuildPipeline.ShellCommandRunner

  def start_link(build_step, server_pid, opts \\ []) do
    GenServer.start_link(__MODULE__, {build_step, server_pid, opts})
  end

  @impl true
  def init({build_step, server_pid, opts}) do
    {:ok, %{build_step: build_step, status: :waiting, server_pid: server_pid, opts: opts}}
  end

  @impl true
  def handle_cast({:run_if_able, completed_runners}, state) do
    state =
      case state do
        %{build_step: %{depends_on: depends_on}, status: :waiting} ->
          if MapSet.subset?(depends_on, completed_runners) do
            run_build_step()
            %{state | status: :running}
          else
            state
          end

        _state ->
          state
      end

    {:noreply, state}
  end

  def handle_cast(:run, state) do
    case state do
      %{
        build_step: %{
          command_type: :shell_command,
          command: command,
          command_env_vars: command_env_vars
        }
      } ->
        run_shell_command(command, command_env_vars, state)

      state ->
        {:noreply, state}
    end
  end

  def handle_cast({:update_status, status}, state) do
    {:noreply, %{state | status: status}}
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

  defp run_build_step do
    runner_pid = self()
    spawn_link(fn -> GenServer.cast(runner_pid, {:update_status, :running}) end)
    spawn_link(fn -> GenServer.cast(runner_pid, :run) end)
  end
end
