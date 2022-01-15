defmodule BuildPipeline.BuildStepBuilder do
  @moduledoc false
  @noop_command_in_bash "true"

  def build(build_step_name \\ "noop") do
    %{
      build_step_name: build_step_name,
      command: @noop_command_in_bash,
      command_env_vars: [],
      command_type: :shell_command,
      depends_on: MapSet.new()
    }
  end

  def with_depends_on(build_step, depends_on) do
    Map.put(build_step, :depends_on, depends_on)
  end

  def with_shell_command(build_step, shell_command) do
    Map.merge(build_step, %{command_type: :shell_command, command: shell_command})
  end

  def with_script(build_step, script) do
    Map.merge(build_step, %{command_type: :script, command: script})
  end
end
