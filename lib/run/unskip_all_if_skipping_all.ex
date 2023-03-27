defmodule BuildPipeline.Run.UnskipAllIfSkippingAll do
  def parse(%{build_pipeline: build_pipeline, setup: %{run_from_failed: true}} = config) do
    build_pipeline =
      if entire_pipeline_to_be_skipped?(build_pipeline) do
        unskip_all(build_pipeline)
      else
        build_pipeline
      end

    {:ok, %{config | build_pipeline: build_pipeline}}
  end

  def parse(%{setup: %{run_from_failed: false}} = config) do
    {:ok, config}
  end

  defp entire_pipeline_to_be_skipped?(build_pipeline) do
    Enum.all?(build_pipeline, fn %{status: status} -> status == :skip end)
  end

  defp unskip_all(build_pipeline) do
    Enum.map(build_pipeline, fn step -> %{step | status: :incomplete} end)
  end
end
