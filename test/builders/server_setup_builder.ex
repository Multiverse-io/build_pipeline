defmodule BuildPipeline.Builders.ServerSetupBuilder do
  def build do
    %{build_pipeline: [], setup: %{verbose: false, cwd: ".", terminal_width: 156}}
  end

  def with_build_pipeline(server_setup, build_pipeline) do
    update_in(server_setup, [:build_pipeline], fn _ -> build_pipeline end)
  end

  def with_verbose(server_setup, verbose) do
    update_in(server_setup, [:setup, :verbose], fn _ -> verbose end)
  end

  def with_cwd(server_setup, cwd) do
    update_in(server_setup, [:setup, :cwd], fn _ -> cwd end)
  end
end
