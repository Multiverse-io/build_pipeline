defmodule BuildPipeline.Run do
  alias BuildPipeline.Run.{
    AnalyseSelfWorth,
    CommandLineArguments,
    ConfigFile,
    EnvVars,
    PreviousRunResult,
    Result,
    UnskipAllIfSkippingAll,
    Server,
    TerminalWidth
  }

  def main(command_line_args \\ [], setup_override_fun \\ fn x -> x end) do
    run(command_line_args, setup_override_fun)
  end

  defp run(command_line_args, setup_override_fun) do
    command_line_args
    |> preflight_checks(setup_override_fun)
    |> analyse_self_worth()
    |> run_if_preflight_checks_passed()
  end

  defp analyse_self_worth({:ok, setup}) do
    case setup.setup.mode do
      {:analyse_self_worth, command_line_args} ->
        AnalyseSelfWorth.run(command_line_args)
        {:ok, :noop}

      _ ->
        {:ok, setup}
    end
  end

  defp analyse_self_worth(error) do
    error
  end

  defp preflight_checks(command_line_args, setup_override_fun) do
    EnvVars.read()
    |> Result.and_then(&CommandLineArguments.parse(&1, command_line_args))
    |> Result.and_then(&ConfigFile.read/1)
    |> Result.and_then(&ConfigFile.parse_and_validate/1)
    |> Result.and_then(&TerminalWidth.append_to_setup/1)
    |> Result.and_then(&PreviousRunResult.read/1)
    |> Result.and_then(&PreviousRunResult.parse_and_validate/1)
    |> Result.and_then(&UnskipAllIfSkippingAll.parse/1)
    |> Result.and_then(fn setup -> {:ok, setup_override_fun.(setup)} end)
  end

  defp run_if_preflight_checks_passed({:ok, :noop}) do
    :ok
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

        maybe_exit(result)
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

    exit_with_code(1)
  end

  defp maybe_exit(result) do
    if result.halt_when_done == true && Application.get_env(:build_pipeline, :env) !== :test do
      exit_code = if result.result == :success, do: 0, else: 1
      System.halt(exit_code)
    else
      {:ok, result}
    end
  end

  def exit_with_code(exit_code) do
    if Application.get_env(:build_pipeline, :env) == :test do
      if exit_code == 0, do: :ok, else: :error
    else
      System.halt(exit_code)
    end
  end

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
