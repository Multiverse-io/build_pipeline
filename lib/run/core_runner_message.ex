defmodule BuildPipeline.Run.CoreRunnerMessage do
  def create(runner) do
    %{command: command, command_env_vars: command_env_vars} = runner

    command_env_vars
    |> Enum.reverse()
    |> Enum.reduce(command, fn {env_key, env_value}, msg ->
      "#{env_key}=#{env_value} #{msg}"
    end)
  end
end
