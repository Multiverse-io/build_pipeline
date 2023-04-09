defmodule BuildPipeline.Run.TerminalMessages do
  alias IO.ANSI
  alias BuildPipeline.Run.{CoreRunnerMessage, PrettyDurationMessage}

  def pending(%{runners: runners}) do
    runners
    |> Enum.sort(fn {_, %{order: order_1}}, {_, %{order: order_2}} -> order_1 <= order_2 end)
    |> Enum.map(fn {_pid, runner} ->
      {ansi_prefix, suffix} =
        if runner.status == :skip do
          {ANSI.green_background() <> ANSI.black(), "[Skipped]"}
        else
          {ANSI.light_magenta(), "[Pending]"}
        end

      %{
        ansi_prefix: ansi_prefix,
        prefix: CoreRunnerMessage.create(runner),
        suffix: suffix,
        truncate: true
      }
    end)
  end

  def running(%{mode: :normal, runners: runners}, runner_pid) do
    runner = Map.fetch!(runners, runner_pid)

    %{
      ansi_prefix: ANSI.magenta(),
      prefix: CoreRunnerMessage.create(runner),
      suffix: "[Running]",
      runner_pid: runner_pid,
      line_update: true
    }
  end

  def running(%{mode: :verbose, runners: runners}, runner_pid) do
    runner = Map.fetch!(runners, runner_pid)

    %{
      message: "#{ANSI.magenta()}#{CoreRunnerMessage.create(runner)} [Running]",
      line_update: false
    }
  end

  def running(%{mode: :debug, runners: runners}, runner_pid) do
    runner = Map.fetch!(runners, runner_pid)

    message = """
    #{ANSI.magenta()}---------------------------------------------------------------------
    #{CoreRunnerMessage.create(runner)} [Running]
    ---------------------------------------------------------------------#{ANSI.reset()}
    """

    %{message: message, line_update: false}
  end

  def succeeded(%{mode: :normal}, runner, runner_pid) do
    %{duration_in_microseconds: duration_in_microseconds} = runner

    %{
      ansi_prefix: ANSI.green(),
      prefix: CoreRunnerMessage.create(runner),
      suffix: "[Succeeded in #{PrettyDurationMessage.create(duration_in_microseconds)}] ✔ ",
      runner_pid: runner_pid,
      line_update: true
    }
  end

  def succeeded(%{mode: mode}, runner, _runner_pid) when mode in [:verbose, :debug] do
    %{duration_in_microseconds: duration_in_microseconds, output: output} = runner

    message = """
    #{ANSI.green()}---------------------------------------------------------------------
    #{CoreRunnerMessage.create(runner)} [Succeeded in #{PrettyDurationMessage.create(duration_in_microseconds)}] ✔

    #{ANSI.reset()}#{output}
    #{ANSI.green()}---------------------------------------------------------------------#{ANSI.reset()}
    """

    %{line_update: false, message: message}
  end

  def failed(%{mode: :verbose} = _server_state, runner, _runner_pid) do
    %{duration_in_microseconds: duration_in_microseconds, output: output} = runner

    message = """
    #{ANSI.red()}---------------------------------------------------------------------
    #{CoreRunnerMessage.create(runner)} [Failed in #{PrettyDurationMessage.create(duration_in_microseconds)}] ✘

    #{ANSI.reset()}#{output}
    #{ANSI.red()}---------------------------------------------------------------------#{ANSI.reset()}
    """

    %{line_update: false, message: message}
  end

  def failed(%{mode: :debug} = _server_state, runner, _runner_pid) do
    %{duration_in_microseconds: duration_in_microseconds} = runner

    message = """
    #{ANSI.red()}---------------------------------------------------------------------
    #{CoreRunnerMessage.create(runner)} [Failed in #{PrettyDurationMessage.create(duration_in_microseconds)}] ✘
    #{ANSI.red()}---------------------------------------------------------------------#{ANSI.reset()}
    """

    %{line_update: false, message: message}
  end

  def failed(%{mode: :normal} = _server_state, runner, runner_pid) do
    %{duration_in_microseconds: duration_in_microseconds} = runner

    %{
      ansi_prefix: ANSI.red(),
      prefix: CoreRunnerMessage.create(runner),
      suffix: "[Failed in #{PrettyDurationMessage.create(duration_in_microseconds)}] ✘ ",
      runner_pid: runner_pid,
      line_update: true
    }
  end

  def abort(%{mode: mode, runners: runners} = _server_state) do
    runners
    |> Enum.reject(fn {_runner_pid, %{status: status}} -> status in [:complete, :skip] end)
    |> Enum.map(fn {runner_pid, runner} ->
      abort_message(mode, runner, runner_pid)
    end)
  end

  def failed_output(%{mode: mode}, _runner) when mode in [:verbose, :debug] do
    []
  end

  def failed_output(%{mode: :normal}, %{output: output}) do
    %{
      message: output,
      line_update: false
    }
  end

  defp abort_message(mode, runner, _) when mode in [:verbose, :debug] do
    %{
      message:
        "#{ANSI.magenta()}#{ANSI.crossed_out()}#{CoreRunnerMessage.create(runner)} [Aborted]",
      line_update: false
    }
  end

  defp abort_message(:normal, runner, runner_pid) do
    %{
      ansi_prefix: "#{ANSI.magenta()}#{ANSI.crossed_out()}",
      prefix: CoreRunnerMessage.create(runner),
      suffix: "[Aborted]",
      runner_pid: runner_pid,
      line_update: true
    }
  end
end
