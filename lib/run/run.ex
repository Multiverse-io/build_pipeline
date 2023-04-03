defmodule BuildPipeline.Run do
  alias BuildPipeline.Run.{
    CommandLineArguments,
    ConfigFile,
    EnvVars,
    PreviousRunResult,
    Result,
    UnskipAllIfSkippingAll,
    Server,
    TerminalWidth
  }

  def main(command_line_args \\ []) do
    run(command_line_args)
  end

  defp run(command_line_args) do
    command_line_args
    |> preflight_checks()
    |> run_if_preflight_checks_passed()
  end

  defp preflight_checks(command_line_args) do
    EnvVars.read()
    |> Result.and_then(&CommandLineArguments.parse(&1, command_line_args))
    |> Result.and_then(&ConfigFile.read/1)
    |> Result.and_then(&ConfigFile.parse_and_validate/1)
    |> Result.and_then(&TerminalWidth.append_to_setup/1)
    |> Result.and_then(&PreviousRunResult.read/1)
    |> Result.and_then(&PreviousRunResult.parse_and_validate/1)
    |> Result.and_then(&UnskipAllIfSkippingAll.parse/1)
  end

  defp run_if_preflight_checks_passed({:ok, setup}) do
    children = [Server.child_spec(setup, self())]

    {:ok, supervisor_pid} = Supervisor.start_link(children, strategy: :one_for_one)

    receive do
      {:server_done, result} ->
        if should_print_runner_output?() do
          result |> finished_message() |> IO.puts()
        end

        Supervisor.stop(supervisor_pid)

        unless Application.get_env(:build_pipeline, :env) == :test do
          result |> exit_code() |> System.halt()
        end
    end
  end

  defp run_if_preflight_checks_passed({:error, error}) do
    case error do
      {:invalid_config, %Jason.DecodeError{}} ->
        IO.puts("I failed to parse the config.json because it was not valid JSON")

      {:invalid_config, error_message} ->
        IO.puts(error_message)

      {:config_file_not_found, bad_config_file_path} ->
        IO.puts(
          "I failed to find a config.json file in the place you told me to look '#{bad_config_file_path}'"
        )

      {:terminal_width, :tput_not_on_system} ->
        IO.puts(
          "I tried to run 'tput cols' but it failed because it looks like I'm not able to run the 'tput' binary?"
        )

      {:terminal_width, :unexpected_tput_result, _} ->
        IO.puts("I tried to run 'tput cols' but it returned a result I couldn't parse! Damn")

      {:previous_run_result, %Jason.DecodeError{}} ->
        IO.puts(
          "I failed to parse the previous_run_result.json because it was not valid JSON. I suggest you delete it and run the full build from scratch"
        )

      {:previous_run_result, error} ->
        IO.puts(error)

      {error_name, details} ->
        IO.puts("#{inspect(error_name)} error :-(\n")
        IO.puts(details)

      other ->
        IO.puts("I failed to startup with an error of\n#{inspect(other)}")
    end

    :error
  end

  defp exit_code(%{result: :success}), do: 0
  defp exit_code(%{result: _}), do: 1

  defp finished_message(%{result: :success}) do
    "#{IO.ANSI.green()}\n************************\nBuild Pipeline - Success\n************************#{IO.ANSI.reset()}"
  end

  defp finished_message(%{result: _}) do
    "#{IO.ANSI.red()}\n************************\nBuild Pipeline - Failure\n************************#{IO.ANSI.reset()}"
  end

  defp should_print_runner_output? do
    Application.get_env(:build_pipeline, :print_runner_output, true)
  end
end
