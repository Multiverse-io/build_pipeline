defmodule BuildPipeline.Builders.ServerSetupBuilder do
  def build do
    %{build_pipeline: [], setup: %{mode: :normal, cwd: ".", terminal_width: 156}}
  end

  def with_build_pipeline(server_setup, build_pipeline) do
    update_in(server_setup, [:build_pipeline], fn _ -> build_pipeline end)
  end

  def with_mode(server_setup, mode) do
    update_in(server_setup, [:setup, :mode], fn _ -> mode end)
  end

  def with_cwd(server_setup, cwd) do
    update_in(server_setup, [:setup, :cwd], fn _ -> cwd end)
  end
end
