defmodule BuildPipeline.Builders.ServerSetupBuilder do
  def build do
    %{
      build_pipeline: [],
      setup: %{mode: :normal, cwd: ".", terminal_width: 156, save_result: false, run_from_failed: false}
    }
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

  def with_run_from_failed(server_setup, run_from_failed) do
    update_in(server_setup, [:setup, :run_from_failed], fn _ -> run_from_failed end)
  end
end
