defmodule BuildPipeline.ShellCommandRunner do
  use GenServer

  def run(command, env_vars, opts \\ []) do
    {:ok, _pid} =
      GenServer.start_link(__MODULE__, %{
        command: command,
        env_vars: env_vars,
        caller_pid: self(),
        write_as_you_go: Keyword.fetch!(opts, :print_cmd_output)
      })

    receive do
      {:exit, {command_output, exit_code}} -> {command_output, exit_code}
    end
  end

  @impl true
  def init(state) do
    port =
      Port.open({:spawn, state.command}, [{:env, state.env_vars}, :exit_status, :stderr_to_stdout])

    {:ok, Map.merge(state, %{port: port, command_output: ""})}
  end

  @impl true
  def handle_info({_port, {:data, command_output}}, state) do
    # this line looks weird and pointless, but it solves elm make output from being wrong
    # and outputting "Main âââ>", instead of "Main ───>"
    # https://elixirforum.com/t/converting-a-list-of-bytes-from-utf-8-or-iso-8859-1-to-elixir-string/20032/2
    command_output = :unicode.characters_to_binary(:erlang.list_to_binary(command_output))

    if state.write_as_you_go do
      IO.write(command_output)
    end

    {:noreply, Map.update!(state, :command_output, &(&1 <> command_output))}
  end

  def handle_info({_port, {:exit_status, exit_status}}, state) do
    send(state.caller_pid, {:exit, {state.command_output, exit_status}})
    {:stop, :normal, state}
  end
end
